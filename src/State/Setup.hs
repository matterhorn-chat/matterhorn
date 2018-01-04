{-# LANGUAGE TypeFamilies #-}
module State.Setup
  ( setupState
  )
where

import           Prelude ()
import           Prelude.Compat

import           Brick.BChan
import           Brick.Themes (themeToAttrMap, loadCustomizations)
import qualified Control.Concurrent.STM as STM
import           Control.Concurrent.MVar (newEmptyMVar, putMVar)
import           Control.Exception (catch)
import           Control.Monad (forM, when)
import           Data.Monoid ((<>))
import qualified Data.Foldable as F
import qualified Data.HashMap.Strict as HM
import           Data.Maybe (listToMaybe, fromMaybe, fromJust, isNothing)
import qualified Data.Sequence as Seq
import qualified Data.Text as T
import           Lens.Micro.Platform
import           System.Exit (exitFailure)
import           System.FilePath ((</>), isRelative, dropFileName)
import           System.IO (Handle)

import           Network.Mattermost.Endpoints
import           Network.Mattermost.Types
import           Network.Mattermost.Logging (mmLoggerDebug)

import           Config
import           InputHistory
import           Login
import           LastRunState
import           State (updateMessageFlag)
import           State.Common
import           TeamSelect
import           Themes
import           TimeUtils (lookupLocalTimeZone)
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
                  then initConnectionDataInsecure (cInfo^.ciHostname)
                         (fromIntegral (cInfo^.ciPort))
                  else initConnectionData (cInfo^.ciHostname)
                         (fromIntegral (cInfo^.ciPort))

        let login = Login { username = cInfo^.ciUsername
                          , password = cInfo^.ciPassword
                          }
        result <- (Right <$> mmLogin cd login)
                    `catch` (\e -> return $ Left $ ResolveError e)
                    `catch` (\e -> return $ Left $ ConnectError e)
                    `catch` (\e -> return $ Left $ OtherAuthError e)

        -- Update the config with the entered settings so we can let the
        -- user adjust if something went wrong rather than enter them
        -- all again.
        let modifiedConfig =
                config { configUser = Just $ cInfo^.ciUsername
                       , configPass = Just $ PasswordString $ cInfo^.ciPassword
                       , configPort = cInfo^.ciPort
                       , configHost = Just $ cInfo^.ciHostname
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

  teams <- mmGetUsersTeams UserMe session
  when (Seq.null teams) $ do
      putStrLn "Error: your account is not a member of any teams"
      exitFailure

  myTeam <- case configTeam config of
      Nothing -> do
          interactiveTeamSelection $ F.toList teams
      Just tName -> do
          let matchingTeam = listToMaybe $ filter matches $ F.toList teams
              matches t = teamName t == tName
          case matchingTeam of
              Nothing -> interactiveTeamSelection (F.toList teams)
              Just t -> return t

  userStatusLock <- newEmptyMVar
  putMVar userStatusLock ()

  slc <- STM.newTChanIO
  wac <- STM.newTChanIO

  prefs <- mmGetUsersPreferences UserMe session

  let themeName = case configTheme config of
          Nothing -> internalThemeName defaultTheme
          Just t -> t
      baseTheme = internalTheme $ fromMaybe defaultTheme (lookupTheme themeName)

  -- Did the configuration specify a theme customization file? If so,
  -- load it and customize the theme.
  custTheme <- case configThemeCustomizationFile config of
      Nothing -> return baseTheme
      Just path ->
          -- If we have no configuration path (i.e. we used the default
          -- config) then ignore theme customization.
          let pathStr = T.unpack path
          in if isRelative pathStr && isNothing (configAbsPath config)
             then return baseTheme
             else do
                 let absPath = if isRelative pathStr
                               then (dropFileName $ fromJust $ configAbsPath config) </> pathStr
                               else pathStr
                 result <- loadCustomizations absPath baseTheme
                 case result of
                     Left e -> do
                         putStrLn $ "Error loading theme customization from " <> show absPath <> ": " <> e
                         exitFailure
                     Right t -> return t

  let cr = ChatResources session cd requestChan eventChan
             slc wac (themeToAttrMap custTheme) userStatusLock config mempty prefs

  initializeState cr myTeam myUser

initializeState :: ChatResources -> Team -> User -> IO ChatState
initializeState cr myTeam myUser = do
  let session = cr^.crSession
      requestChan = cr^.crRequestQueue
      myTeamId = getId myTeam

  -- Create a predicate to find the last selected channel by reading the
  -- run state file. If unable to read or decode or validate the file, this
  -- predicate is just `isTownSquare`.
  isLastSelectedChannel <- do
    result <- readLastRunState $ teamId myTeam
    case result of
      Right lrs | isValidLastRunState cr myUser lrs -> return $ \c ->
           channelId c == lrs^.lrsSelectedChannelId
      _ -> return isTownSquare

  -- Get all channels, but filter down to just the one we want to start
  -- in. We get all, rather than requesting by name or ID, because
  -- we don't know whether the server will give us a last-viewed preference.
  -- We first try to find a channel matching with the last selected channel ID,
  -- failing which we look for the Town Square channel by name.
  -- This is not entirely correct since the Town Square channel can be renamed!
  userChans <- mmGetChannelsForUser UserMe myTeamId session
  let lastSelectedChans = Seq.filter isLastSelectedChannel userChans
      chans = if Seq.null lastSelectedChans
                then Seq.filter isTownSquare userChans
                else lastSelectedChans

  -- Since the only channel we are dealing with is by construction the
  -- last channel, we don't have to consider other cases here:
  msgs <- forM (F.toList chans) $ \c -> do
      let cChannel = makeClientChannel c & ccInfo.cdCurrentState .~ state
          state = ChanInitialSelect
      return (getId c, cChannel)

  tz    <- lookupLocalTimeZone
  hist  <- do
      result <- readHistory
      case result of
          Left _ -> return newHistory
          Right h -> return h

  -- Start background worker threads:
  -- * User status refresher
  startUserRefreshThread (cr^.crUserStatusLock) session requestChan
  -- * Refresher for users who are typing currently
  when (configShowTypingIndicator (cr^.crConfiguration)) $
    startTypingUsersRefreshThread requestChan
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

  loadFlaggedMessages (cr^.crPreferences) st
  return st
