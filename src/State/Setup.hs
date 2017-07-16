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
import           Data.Maybe (listToMaybe, maybeToList, fromJust, catMaybes)
import           Data.Monoid ((<>))
import qualified Data.Sequence as Seq
import           Data.Time.LocalTime (getCurrentTimeZone)
import           Lens.Micro.Platform
import           System.Exit (exitFailure)
import           System.IO (Handle)
import           Text.Aspell (AspellOption(..), startAspell)

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
import           Types.Users
import qualified Zipper as Z

loadFlaggedMessages :: ChatState -> IO ()
loadFlaggedMessages st = doAsyncWithIO Normal st $ do
  prefs <- mmGetMyPreferences (st^.csResources.crSession)
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
        cd <- setLogger `fmap`
                initConnectionData (ciHostname cInfo)
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

loadAllUsers :: Session -> IO (HM.HashMap UserId User)
loadAllUsers session = go HM.empty 0
  where go users n = do
          newUsers <- mmGetUsers session (n * 50) 50
          if HM.null newUsers
            then return users
            else go (newUsers <> users) (n+1)

initializeState :: ChatResources -> Team -> User -> IO ChatState
initializeState cr myTeam myUser = do
  let session = cr^.crSession
      requestChan = cr^.crRequestQueue
  let myTeamId = getId myTeam

  chans <- mmGetChannels session myTeamId

  msgs <- forM (F.toList chans) $ \c -> do
      let cChannel = makeClientChannel c & ccInfo.cdCurrentState .~ state
          state = if c^.channelNameL == "town-square"
                  then ChanInitialSelect
                  else initialChannelState
      return (getId c, cChannel)

  teamUsers <- mmGetProfiles session myTeamId 0 10000
  users <- loadAllUsers session
  let mkUser u = (u^.userIdL, userInfoFromUser u (HM.member (u^.userIdL) teamUsers))
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
  -- * Spell check timer
  spResult <- case configEnableAspell $ cr^.crConfiguration of
      False -> return Nothing
      True -> do
          let aspellOpts = catMaybes [ UseDictionary <$> (configAspellDictionary $ cr^.crConfiguration)
                                     ]
              spellCheckerTimeout = 500 * 1000 -- 500k us = 500ms
          asResult <- either (const Nothing) Just <$> startAspell aspellOpts
          case asResult of
              Nothing -> return Nothing
              Just as -> do
                  resetSCChan <- startSpellCheckerThread (cr^.crEventQueue) spellCheckerTimeout
                  let resetSCTimer = STM.atomically $ STM.writeTChan resetSCChan ()
                  return $ Just (as, resetSCTimer)

  let chanNames = mkChanNames myUser users chans
      Just townSqId = chanNames ^. cnToChanId . at "town-square"
      chanIds = [ (chanNames ^. cnToChanId) HM.! i
                | i <- chanNames ^. cnChans ] ++
                [ c
                | i <- chanNames ^. cnUsers
                , c <- maybeToList (HM.lookup i (chanNames ^. cnToChanId)) ]
      chanZip = Z.findRight (== townSqId) (Z.fromList chanIds)
      st = newState cr chanZip myUser myTeam tz hist spResult
             & csUsers %~ flip (foldr (uncurry addUser)) (fmap mkUser users)
             & csChannels %~ flip (foldr (uncurry addChannel)) msgs
             & csNames .~ chanNames

  loadFlaggedMessages st
  return st
