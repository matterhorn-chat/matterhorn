{-# LANGUAGE TypeFamilies #-}

module State.Setup where

import           Prelude ()
import           Prelude.Compat

import           Brick.BChan
import qualified Control.Concurrent.STM as STM
import           Control.Concurrent.MVar (newEmptyMVar)
import           Control.Exception (catch)
import           Control.Monad (forM, when)
import qualified Data.Foldable as F
import qualified Data.HashMap.Strict as HM
import           Data.Maybe (listToMaybe, fromJust)
import qualified Data.Sequence as Seq
import           Data.Time.LocalTime (getCurrentTimeZone)
import           Lens.Micro.Platform
import           System.Exit (exitFailure)
import           System.IO (Handle)

import           Network.Mattermost
import           Network.Mattermost.Lenses
import           Network.Mattermost.Logging (mmLoggerDebug)

import           Config
import           InputHistory
import           Login
import           State (updateMessageFlag)
import           State.Common
import           TeamSelect
import           Themes
import           State.Setup.Threads
import           Types
import           Types.Channels
import qualified Zipper as Z

loadFlaggedMessages :: Seq.Seq Preference -> ChatState -> IO ()
loadFlaggedMessages prefs st = doAsyncWithIO Normal st $ do
  return $ sequence_ [ updateMessageFlag (flaggedPostId fp) True
                     | Just fp <- F.toList (fmap preferenceToFlaggedPost prefs)
                     , flaggedPostStatus fp
                     ]

setupState :: Maybe Handle -> Config -> RequestChan -> BChan MHEvent -> IO ChatState
setupState logFile config requestChan eventChan = do
  -- If we don't have enough credentials, ask for them.
  connInfo <- case getCredentials config of
      Nothing -> interactiveGatherCredentials config Nothing
      Just connInfo -> return connInfo

  let setLogger = case logFile of
        Nothing -> id
        Just f  -> \ cd -> cd `withLogger` mmLoggerDebug f

  let loginLoop cInfo = do
        cd <- fmap setLogger $
                -- we don't implement HTTP fallback right now, we just
                -- go straight for HTTP if someone has indicated that
                -- they want it. We probably should in the future
                -- always try HTTPS first, and then, if the
                -- configuration option is there, try falling back to
                -- HTTP.
                if (configUnsafeUseHTTP config)
                  then initConnectionDataInsecure (ciHostname cInfo)
                         (fromIntegral (ciPort cInfo))
                  else initConnectionData (ciHostname cInfo)
                         (fromIntegral (ciPort cInfo))

        let login = Login { username = ciUsername cInfo
                          , password = ciPassword cInfo
                          }
        result <- (Right <$> mmLogin cd login)
                    `catch` (\e -> return $ Left $ ResolveError e)
                    `catch` (\e -> return $ Left $ ConnectError e)
                    `catch` (\e -> return $ Left $ OtherAuthError e)

        -- Update the config with the entered settings so we can let the
        -- user adjust if something went wrong rather than enter them
        -- all again.
        let modifiedConfig =
                config { configUser = Just $ ciUsername cInfo
                       , configPass = Just $ PasswordString $ ciPassword cInfo
                       , configPort = ciPort cInfo
                       , configHost = Just $ ciHostname cInfo
                       }

        case result of
            Right (Right (sess, user)) ->
                return (sess, user, cd)
            Right (Left e) ->
                interactiveGatherCredentials modifiedConfig (Just $ LoginError e) >>=
                    loginLoop
            Left e ->
                interactiveGatherCredentials modifiedConfig (Just e) >>=
                    loginLoop

  (session, myUser, cd) <- loginLoop connInfo

  initialLoad <- mmGetInitialLoad session
  when (Seq.null $ initialLoadTeams initialLoad) $ do
      putStrLn "Error: your account is not a member of any teams"
      exitFailure

  myTeam <- case configTeam config of
      Nothing -> do
          interactiveTeamSelection $ F.toList $ initialLoadTeams initialLoad
      Just tName -> do
          let matchingTeam = listToMaybe $ filter matches $ F.toList $ initialLoadTeams initialLoad
              matches t = teamName t == tName
          case matchingTeam of
              Nothing -> interactiveTeamSelection (F.toList (initialLoadTeams initialLoad))
              Just t -> return t

  quitCondition <- newEmptyMVar
  slc <- STM.atomically STM.newTChan

  let themeName = case configTheme config of
          Nothing -> defaultThemeName
          Just t -> t
      theme = case lookup themeName themes of
          Nothing -> fromJust $ lookup defaultThemeName themes
          Just t -> t
      cr = ChatResources session cd requestChan eventChan
             slc theme quitCondition config mempty
  initializeState cr myTeam myUser

initializeState :: ChatResources -> Team -> User -> IO ChatState
initializeState cr myTeam myUser = do
  prefs <- mmGetMyPreferences (cr^.crSession)
  let lastChan = getLastChannelPreference prefs

  let session = cr^.crSession
      requestChan = cr^.crRequestQueue
      myTeamId = getId myTeam
      isLastChannel c =
          case lastChan of
              Nothing -> c^.channelNameL == "town-square"
              Just lastChanId -> c^.channelIdL == lastChanId

  -- Get all channels, but filter down to just the one we want to start
  -- in. We get all, rather than requesting by name or ID, because
  -- we don't know whether the server will give us a last-viewed
  -- preference, and when it doesn't, we need to look for Town Square
  -- by name. Even this is ultimately not entirely correct since Town
  -- Square can be renamed!
  chans <- Seq.filter isLastChannel <$> mmGetChannels session myTeamId

  -- Since the only channel we are dealing with is by construction the
  -- last channel, we don't have to consider other cases here:
  msgs <- forM (F.toList chans) $ \c -> do
      let cChannel = makeClientChannel c & ccInfo.cdCurrentState .~ state
          state = ChanInitialSelect
      return (getId c, cChannel)

  tz    <- getCurrentTimeZone
  hist  <- do
      result <- readHistory
      case result of
          Left _ -> return newHistory
          Right h -> return h

  -- Start background worker threads:
  -- * User status refresher
  startUserRefreshThread session requestChan
  -- * Timezone change monitor
  startTimezoneMonitorThread tz requestChan
  -- * Subprocess logger
  startSubprocessLoggerThread (cr^.crSubprocessLog) requestChan
  -- * Spell checker and spell check timer, if configured
  spResult <- maybeStartSpellChecker (cr^.crConfiguration) (cr^.crEventQueue)

  let chanNames = mkChanNames myUser mempty chans
      chanIds = [ (chanNames ^. cnToChanId) HM.! i
                | i <- chanNames ^. cnChans ]
      chanZip = Z.fromList chanIds
      st = newState cr chanZip myUser myTeam tz hist spResult
             & csChannels %~ flip (foldr (uncurry addChannel)) msgs
             & csNames .~ chanNames

  loadFlaggedMessages prefs st
  return st
