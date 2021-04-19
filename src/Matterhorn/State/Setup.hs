{-# LANGUAGE TypeFamilies #-}
module Matterhorn.State.Setup
  ( setupState
  )
where

import           Prelude ()
import           Matterhorn.Prelude

import           Brick.BChan ( newBChan )
import           Brick.Themes ( themeToAttrMap, loadCustomizations )
import qualified Control.Concurrent.STM as STM
import           Data.Either ( fromRight )
import qualified Data.Foldable as F
import qualified Data.HashMap.Strict as HM
import           Data.Maybe ( fromJust )
import qualified Data.Text as T
import           Data.Time.Clock ( getCurrentTime )
import qualified Graphics.Vty as Vty
import           Lens.Micro.Platform ( (.~) )
import           System.Exit ( exitFailure, exitSuccess )
import           System.FilePath ( (</>), isRelative, dropFileName )

import           Network.Mattermost.Endpoints
import           Network.Mattermost.Types

import           Matterhorn.Config
import           Matterhorn.InputHistory
import           Matterhorn.Login
import           Matterhorn.State.Flagging
import           Matterhorn.State.Teams ( buildTeamState )
import           Matterhorn.State.Setup.Threads
import           Matterhorn.Themes
import           Matterhorn.TimeUtils ( lookupLocalTimeZone, utcTimezone )
import           Matterhorn.Types
import           Matterhorn.Types.Common
import           Matterhorn.Emoji
import           Matterhorn.FilePaths ( userEmojiJsonPath, bundledEmojiJsonPath )


incompleteCredentials :: Config -> ConnectionInfo
incompleteCredentials config = ConnectionInfo
  { _ciHostname = fromMaybe "" (configHost config)
  , _ciPort     = configPort config
  , _ciUrlPath  = fromMaybe "" (configUrlPath config)
  , _ciUsername = fromMaybe "" (configUser config)
  , _ciPassword = case configPass config of
                    Just (PasswordString s) -> s
                    _                       -> ""
  , _ciAccessToken = case configToken config of
                       Just (TokenString s) -> s
                       _                    -> ""
  , _ciType     = configConnectionType config
  }

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

setupState :: IO Vty.Vty -> Maybe FilePath -> Config -> IO (ChatState, Vty.Vty)
setupState mkVty mLogLocation config = do
  initialVty <- mkVty

  eventChan <- newBChan 2500
  logMgr <- newLogManager eventChan (configLogMaxBufferSize config)

  -- If we got an initial log location, start logging there.
  case mLogLocation of
      Nothing -> return ()
      Just loc -> startLoggingToFile logMgr loc

  let logApiEvent ev = apiLogEventToLogMessage ev >>= sendLogMessage logMgr
      setLogger cd = cd `withLogger` logApiEvent

  (mLastAttempt, loginVty) <- interactiveGetLoginSession initialVty mkVty
                                                         setLogger
                                                         logMgr
                                                         (incompleteCredentials config)

  let shutdown vty = do
          Vty.shutdown vty
          exitSuccess

  (session, me, cd, mbTeam) <- case mLastAttempt of
      Nothing ->
          -- The user never attempted a connection and just chose to
          -- quit.
          shutdown loginVty
      Just (AttemptFailed {}) ->
          -- The user attempted a connection and failed, and then chose
          -- to quit.
          shutdown loginVty
      Just (AttemptSucceeded _ cd sess user mbTeam) ->
          -- The user attempted a connection and succeeded so continue
          -- with setup.
          return (sess, user, cd, mbTeam)

  teams <- F.toList <$> mmGetUsersTeams UserMe session
  when (null teams) $ do
      putStrLn "Error: your account is not a member of any teams"
      exitFailure

  let initialTeamId = fromMaybe (teamId $ head $ sortTeams teams) $ do
          tName <- mbTeam <|> configTeam config
          let matchingTeam = listToMaybe $ filter (matchesTeam tName) teams
          teamId <$> matchingTeam

  userStatusChan <- STM.newTChanIO
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
                         Vty.shutdown loginVty
                         putStrLn $ "Error loading theme customization from " <> show absPath <> ": " <> e
                         exitFailure
                     Right t -> return t

  requestChan <- STM.atomically STM.newTChan

  emoji <- either (const emptyEmojiCollection) id <$> do
      result1 <- loadEmoji =<< userEmojiJsonPath
      case result1 of
          Right e -> return $ Right e
          Left _ -> loadEmoji =<< bundledEmojiJsonPath

  let cr = ChatResources { _crSession             = session
                         , _crWebsocketThreadId   = Nothing
                         , _crConn                = cd
                         , _crRequestQueue        = requestChan
                         , _crEventQueue          = eventChan
                         , _crSubprocessLog       = slc
                         , _crWebsocketActionChan = wac
                         , _crTheme               = themeToAttrMap custTheme
                         , _crStatusUpdateChan    = userStatusChan
                         , _crConfiguration       = config
                         , _crFlaggedPosts        = mempty
                         , _crUserPreferences     = userPrefs
                         , _crSyntaxMap           = mempty
                         , _crLogManager          = logMgr
                         , _crEmoji               = emoji
                         }

  st <- initializeState cr initialTeamId teams me

  return (st, loginVty)

matchesTeam :: T.Text -> Team -> Bool
matchesTeam tName t =
    let normalizeUserText = normalize . sanitizeUserText
        normalize = T.strip . T.toLower
        urlName = normalizeUserText $ teamName t
        displayName = normalizeUserText $ teamDisplayName t
    in normalize tName `elem` [displayName, urlName]

initializeState :: ChatResources -> TeamId -> [Team] -> User -> IO ChatState
initializeState cr initialTeamId teams me = do
  let session = getResourceSession cr
      requestChan = cr^.crRequestQueue

  tz <- fromRight utcTimezone <$> lookupLocalTimeZone

  hist <- do
      result <- readHistory
      case result of
          Left _ -> return newHistory
          Right h -> return h

  --------------------------------------------------------------------
  -- Start background worker threads:
  --
  --  * Syntax definition loader
  startSyntaxMapLoaderThread (cr^.crConfiguration) (cr^.crEventQueue)

  --  * Main async queue worker thread
  startAsyncWorkerThread (cr^.crConfiguration) (cr^.crRequestQueue) (cr^.crEventQueue)

  --  * User status thread
  startUserStatusUpdateThread (cr^.crStatusUpdateChan) session requestChan

  --  * Refresher for users who are typing currently
  when (configShowTypingIndicator (cr^.crConfiguration)) $
    startTypingUsersRefreshThread requestChan

  --  * Timezone change monitor
  startTimezoneMonitorThread tz requestChan

  --  * Subprocess logger
  startSubprocessLoggerThread (cr^.crSubprocessLog) requestChan

  -- End thread startup ----------------------------------------------

  -- For each team, build a team state and load the last-run state.
  (teamStates, chanLists) <- unzip <$> mapM (buildTeamState cr me) teams

  let startupState =
          StartupStateInfo { startupStateResources      = cr
                           , startupStateConnectedUser  = me
                           , startupStateTimeZone       = tz
                           , startupStateInitialHistory = hist
                           , startupStateInitialTeam    = initialTeamId
                           , startupStateTeams          = teamMap
                           }
      clientChans = mconcat chanLists
      st = newState startupState & csChannels .~ clientChans
      teamMap = HM.fromList $ (\ts -> (teamId $ _tsTeam ts, ts)) <$> F.toList teamStates

  loadFlaggedMessages (cr^.crUserPreferences.userPrefFlaggedPostList) st

  -- Trigger an initial websocket refresh
  writeBChan (cr^.crEventQueue) RefreshWebsocketEvent

  return st
