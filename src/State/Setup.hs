{-# LANGUAGE TypeFamilies #-}
module State.Setup
  ( setupState
  )
where

import           Prelude ()
import           Prelude.MH

import           Brick.BChan
import           Brick.Themes ( themeToAttrMap, loadCustomizations )
import           Control.Concurrent.MVar ( newMVar )
import qualified Control.Concurrent.STM as STM
import           Control.Exception ( catch )
import           Data.Maybe ( fromJust )
import qualified Data.Sequence as Seq
import qualified Data.Text as T
import           Data.Time.Clock ( getCurrentTime )
import           Lens.Micro.Platform ( (.~) )
import           System.Exit ( exitFailure )
import           System.FilePath ( (</>), isRelative, dropFileName )
import           System.IO.Error ( catchIOError )

import           Network.Mattermost.Endpoints
import           Network.Mattermost.Types

import           Config
import           InputHistory
import           LastRunState
import           Login
import           State.Flagging
import           State.Setup.Threads
import           TeamSelect
import           Themes
import           TimeUtils ( lookupLocalTimeZone )
import           Types
import           Types.Common
import qualified Zipper as Z


incompleteCredentials :: Config -> ConnectionInfo
incompleteCredentials config = ConnectionInfo hStr (configPort config) uStr pStr
    where
        hStr = maybe "" id $ configHost config
        uStr = maybe "" id $ configUser config
        pStr = case configPass config of
            Just (PasswordString s) -> s
            _                       -> ""

convertLoginExceptions :: IO a -> IO (Either AuthenticationException a)
convertLoginExceptions act =
    (Right <$> act)
        `catch` (\e -> return $ Left $ ResolveError e)
        `catch` (\e -> return $ Left $ ConnectError e)
        `catchIOError` (\e -> return $ Left $ AuthIOError e)
        `catch` (\e -> return $ Left $ OtherAuthError e)

apiLogEventToLogMessage :: LogEvent -> IO LogMessage
apiLogEventToLogMessage ev = do
    now <- getCurrentTime
    let msg = T.pack $ "Function: " <> logFunction ev <>
                       ", event: " <> show (logEventType ev)
    return $ LogMessage { logMessageCategory = LogAPI
                        , logMessageText = msg
                        , logMessageContext = Nothing
                        , logMessageTimestamp = now
                        }

setupState :: Maybe FilePath -> Config -> IO ChatState
setupState mLogLocation initialConfig = do
  -- If we don't have enough credentials, ask for them.
  connInfo <- case getCredentials initialConfig of
      Nothing -> interactiveGatherCredentials (incompleteCredentials initialConfig) Nothing
      Just connInfo -> return connInfo

  eventChan <- newBChan 25
  logMgr <- newLogManager eventChan (configLogMaxBufferSize initialConfig)

  let logApiEvent ev = apiLogEventToLogMessage ev >>= sendLogMessage logMgr
      setLogger cd = cd `withLogger` logApiEvent
      poolCfg = ConnectionPoolConfig { cpIdleConnTimeout = 60
                                     , cpStripesCount = 1
                                     , cpMaxConnCount = 5
                                     }
      loginLoop cInfo = do
        cd <- fmap setLogger $
                if (configUnsafeUseHTTP initialConfig)
                  then initConnectionDataInsecure (cInfo^.ciHostname)
                         (fromIntegral (cInfo^.ciPort))
                         poolCfg
                  else initConnectionData (cInfo^.ciHostname)
                         (fromIntegral (cInfo^.ciPort))
                         poolCfg

        let login = Login { username = cInfo^.ciUsername
                          , password = cInfo^.ciPassword
                          }
        result <- convertLoginExceptions $ mmLogin cd login

        -- Update the config with the entered settings so that later,
        -- when we offer the option of saving the entered credentials to
        -- disk, we can do so with an updated config.
        let config =
                initialConfig { configUser = Just $ cInfo^.ciUsername
                              , configPass = Just $ PasswordString $ cInfo^.ciPassword
                              , configPort = cInfo^.ciPort
                              , configHost = Just $ cInfo^.ciHostname
                              }

        case result of
            Right (Right (sess, user)) ->
                return (sess, user, cd, config)
            Right (Left e) ->
                interactiveGatherCredentials cInfo (Just $ LoginError e) >>=
                    loginLoop
            Left e ->
                interactiveGatherCredentials cInfo (Just e) >>=
                    loginLoop

  (session, me, cd, config) <- loginLoop connInfo

  teams <- mmGetUsersTeams UserMe session
  when (Seq.null teams) $ do
      putStrLn "Error: your account is not a member of any teams"
      exitFailure

  myTeam <- case configTeam config of
      Nothing -> do
          interactiveTeamSelection $ toList teams
      Just tName -> do
          let matchingTeam = listToMaybe $ filter matches $ toList teams
              matches t = (sanitizeUserText $ teamName t) == tName
          case matchingTeam of
              Nothing -> interactiveTeamSelection (toList teams)
              Just t -> return t

  userStatusLock <- newMVar ()
  userIdSet <- STM.atomically $ STM.newTVar mempty
  slc <- STM.newTChanIO
  wac <- STM.newTChanIO

  prefs <- mmGetUsersPreferences UserMe session
  let userPrefs = setUserPreferences prefs defaultUserPreferences
      themeName = case configTheme config of
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

  requestChan <- STM.atomically STM.newTChan

  let cr = ChatResources session cd requestChan eventChan
             slc wac (themeToAttrMap custTheme) userStatusLock
             userIdSet config mempty userPrefs mempty logMgr

  st <- initializeState cr myTeam me

  -- If we got an initial log location, start logging there.
  case mLogLocation of
      Nothing -> return ()
      Just loc -> startLoggingToFile logMgr loc

  return st

initializeState :: ChatResources -> Team -> User -> IO ChatState
initializeState cr myTeam me = do
  let session = getResourceSession cr
      requestChan = cr^.crRequestQueue
      myTId = getId myTeam

  -- Create a predicate to find the last selected channel by reading the
  -- run state file. If unable to read or decode or validate the file, this
  -- predicate is just `isTownSquare`.
  isLastSelectedChannel <- do
    result <- readLastRunState $ teamId myTeam
    case result of
      Right lrs | isValidLastRunState cr me lrs -> return $ \c ->
           channelId c == lrs^.lrsSelectedChannelId
      _ -> return isTownSquare

  -- Get all channels, but filter down to just the one we want to start
  -- in. We get all, rather than requesting by name or ID, because
  -- we don't know whether the server will give us a last-viewed preference.
  -- We first try to find a channel matching with the last selected channel ID,
  -- failing which we look for the Town Square channel by name.
  userChans <- mmGetChannelsForUser UserMe myTId session
  let lastSelectedChans = Seq.filter isLastSelectedChannel userChans
      chans = if Seq.null lastSelectedChans
                then Seq.filter isTownSquare userChans
                else lastSelectedChans

  -- Since the only channel we are dealing with is by construction the
  -- last channel, we don't have to consider other cases here:
  chanPairs <- forM (toList chans) $ \c -> do
      cChannel <- makeClientChannel (userId me) c
      return (getId c, cChannel)

  tz <- lookupLocalTimeZone
  hist <- do
      result <- readHistory
      case result of
          Left _ -> return newHistory
          Right h -> return h

  --------------------------------------------------------------------
  -- Start background worker threads:
  --
  -- * Syntax definition loader
  startSyntaxMapLoaderThread (cr^.crConfiguration) (cr^.crEventQueue)

  -- * Main async queue worker thread
  startAsyncWorkerThread (cr^.crConfiguration) (cr^.crRequestQueue) (cr^.crEventQueue)

  -- * User status thread
  startUserStatusUpdateThread (cr^.crUserIdSet) (cr^.crUserStatusLock) session requestChan

  -- * Refresher for users who are typing currently
  when (configShowTypingIndicator (cr^.crConfiguration)) $
    startTypingUsersRefreshThread requestChan

  -- * Timezone change monitor
  startTimezoneMonitorThread tz requestChan

  -- * Subprocess logger
  startSubprocessLoggerThread (cr^.crSubprocessLog) requestChan

  -- * Spell checker and spell check timer, if configured
  spResult <- maybeStartSpellChecker (cr^.crConfiguration) (cr^.crEventQueue)

  -- End thread startup ----------------------------------------------

  now <- getCurrentTime
  let chanIds = mkChannelZipperList now Nothing (cr^.crUserPreferences) clientChans noUsers
      chanZip = Z.fromList chanIds
      clientChans = foldr (uncurry addChannel) noChannels chanPairs
      startupState =
          StartupStateInfo { startupStateResources      = cr
                           , startupStateChannelZipper  = chanZip
                           , startupStateConnectedUser  = me
                           , startupStateTeam           = myTeam
                           , startupStateTimeZone       = tz
                           , startupStateInitialHistory = hist
                           , startupStateSpellChecker   = spResult
                           }
      st = newState startupState & csChannels .~ clientChans

  loadFlaggedMessages (cr^.crUserPreferences.userPrefFlaggedPostList) st

  -- Trigger an initial websocket refresh
  writeBChan (cr^.crEventQueue) RefreshWebsocketEvent

  return st
