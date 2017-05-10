module State.Setup where

import           Prelude ()
import           Prelude.Compat

import           Brick.BChan
import           Brick.Widgets.List (list)
import           Control.Concurrent (threadDelay, forkIO)
import qualified Control.Concurrent.STM as STM
import           Control.Concurrent.MVar (newEmptyMVar)
import           Control.Exception (SomeException, catch, try)
import           Control.Monad (forM, forever, when, void)
import           Control.Monad.IO.Class (liftIO)
import qualified Data.Text as T
import qualified Data.Foldable as F
import qualified Data.HashMap.Strict as HM
import           Data.List (sort)
import           Data.Maybe (listToMaybe, maybeToList, fromJust)
import           Data.Monoid ((<>))
import qualified Data.Sequence as Seq
import           Data.Time.LocalTime ( TimeZone(..), getCurrentTimeZone )
import           Lens.Micro.Platform
import           System.Exit (exitFailure)
import           System.IO (Handle, hPutStrLn, hFlush)
import           System.IO.Temp (withSystemTempFile)

import           Network.Mattermost
import           Network.Mattermost.Lenses
import           Network.Mattermost.Logging (mmLoggerDebug)

import           Config
import           InputHistory
import           Login
import           State.Common
import           TeamSelect
import           Themes
import           Types
import           Zipper (Zipper)
import qualified Zipper as Z

fetchUserStatuses :: Session -> IO (MH ())
fetchUserStatuses session = do
  statusMap <- mmGetStatuses session
  return $ do
    let updateUser u = u & uiStatus .~ (case HM.lookup (u^.uiId) statusMap of
                                          Nothing -> Offline
                                          Just t  -> statusFromText t)
    usrMap.each %= updateUser

userRefresh :: Session -> RequestChan -> IO ()
userRefresh session requestChan = void $ forkIO $ forever refresh
  where refresh = do
          let seconds = (* (1000 * 1000))
          threadDelay (seconds 30)
          STM.atomically $ STM.writeTChan requestChan $ do
            rs <- try $ fetchUserStatuses session
            case rs of
              Left (_ :: SomeException) -> return (return ())
              Right upd -> return upd

startSubprocessLogger :: STM.TChan ProgramOutput -> RequestChan -> IO ()
startSubprocessLogger logChan requestChan = do
    let logMonitor logPath logHandle = do
          ProgramOutput progName args out err ec <-
              STM.atomically $ STM.readTChan logChan

          -- If either stdout or stderr is non-empty, log it and
          -- notify the user.
          let emptyOutput s = null s || s == "\n"

          case emptyOutput out && emptyOutput err of
              True -> logMonitor logPath logHandle
              False -> do
                  hPutStrLn logHandle $
                      unlines [ "Program: " <> progName
                              , "Arguments: " <> show args
                              , "Exit code: " <> show ec
                              , "Stdout:"
                              , out
                              , "Stderr:"
                              , err
                              ]
                  hFlush logHandle

                  STM.atomically $ STM.writeTChan requestChan $ do
                      return $ do
                          let msg = T.pack $ "Program " <> show progName <>
                                             " produced unexpected output; see " <>
                                             logPath <> " for details."
                          postErrorMessage msg

                  logMonitor logPath logHandle

    void $ forkIO $ withSystemTempFile "matterhorn.log" logMonitor

startTimezoneMonitor :: TimeZone -> RequestChan -> IO ()
startTimezoneMonitor tz requestChan = do
  -- Start the timezone monitor thread
  let timezoneMonitorSleepInterval = minutes 5
      minutes = (* (seconds 60))
      seconds = (* (1000 * 1000))
      timezoneMonitor prevTz = do
        threadDelay timezoneMonitorSleepInterval

        newTz <- getCurrentTimeZone
        when (newTz /= prevTz) $
            STM.atomically $ STM.writeTChan requestChan $ do
                return $ timeZone .= newTz

        timezoneMonitor newTz

  void $ forkIO (timezoneMonitor tz)

mkChanNames :: User -> HM.HashMap UserId User -> Seq.Seq Channel -> MMNames
mkChanNames myUser users chans = MMNames
  { _cnChans = sort
               [ preferredChannelName c
               | c <- F.toList chans, channelType c /= Direct ]
  , _cnDMs = sort
             [ channelName c
             | c <- F.toList chans, channelType c == Direct ]
  , _cnToChanId = HM.fromList $
                  [ (preferredChannelName c, channelId c) | c <- F.toList chans ] ++
                  [ (userUsername u, c)
                  | u <- HM.elems users
                  , c <- lookupChan (getDMChannelName (getId myUser) (getId u))
                  ]
  , _cnUsers = sort (map userUsername (HM.elems users))
  , _cnToUserId = HM.fromList
                  [ (userUsername u, getId u) | u <- HM.elems users ]
  }
  where lookupChan n = [ c^.channelIdL
                       | c <- F.toList chans, c^.channelNameL == n
                       ]

newState :: ChatResources
         -> Zipper ChannelId
         -> User
         -> Team
         -> TimeZone
         -> InputHistory
         -> ChatState
newState rs i u m tz hist = ChatState
  { _csResources                   = rs
  , _csFocus                       = i
  , _csMe                          = u
  , _csMyTeam                      = m
  , _csNames                       = emptyMMNames
  , _msgMap                        = HM.empty
  , _csPostMap                     = HM.empty
  , _usrMap                        = HM.empty
  , _timeZone                      = tz
  , _csEditState                   = emptyEditState hist
  , _csMode                        = Main
  , _csShowMessagePreview          = configShowMessagePreview $ rs^.crConfiguration
  , _csChannelSelectString         = ""
  , _csChannelSelectChannelMatches = mempty
  , _csChannelSelectUserMatches    = mempty
  , _csRecentChannel               = Nothing
  , _csUrlList                     = list UrlList mempty 2
  , _csConnectionStatus            = Connected
  , _csJoinChannelList             = Nothing
  , _csMessageSelect               = MessageSelectState Nothing
  }

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

        putStrLn "Authenticating..."

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
      cr = ChatResources
             { _crSession       = session
             , _crConn          = cd
             , _crRequestQueue  = requestChan
             , _crEventQueue    = eventChan
             , _crTheme         = theme
             , _crQuitCondition = quitCondition
             , _crConfiguration = config
             , _crSubprocessLog = slc
             }
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
  let ChatResources session _ requestChan _ _ _ _ _ = cr
  let myTeamId = getId myTeam

  STM.atomically $ STM.writeTChan requestChan $ fetchUserStatuses session

  userRefresh session requestChan

  putStrLn $ "Loading channels for team " <> show (teamName myTeam) <> "..."
  chans <- mmGetChannels session myTeamId

  msgs <- fmap (HM.fromList . F.toList) $ forM (F.toList chans) $ \c -> do
      let cChannel = ClientChannel
                       { _ccContents = emptyChannelContents
                       , _ccInfo     = initialChannelInfo c & cdCurrentState .~ state
                       }

          state = if c^.channelNameL == "town-square"
                  then ChanLoadPending
                  else ChanUnloaded

      return (getId c, cChannel)

  teamUsers <- mmGetProfiles session myTeamId 0 10000
  users <- loadAllUsers session
  let mkUser u = userInfoFromUser u (HM.member (u^.userIdL) teamUsers)
  tz    <- getCurrentTimeZone
  hist  <- do
      result <- readHistory
      case result of
          Left _ -> return newHistory
          Right h -> return h

  startTimezoneMonitor tz requestChan

  startSubprocessLogger (cr^.crSubprocessLog) requestChan

  let chanNames = mkChanNames myUser users chans
      Just townSqId = chanNames ^. cnToChanId . at "town-square"
      chanIds = [ (chanNames ^. cnToChanId) HM.! i
                | i <- chanNames ^. cnChans ] ++
                [ c
                | i <- chanNames ^. cnUsers
                , c <- maybeToList (HM.lookup i (chanNames ^. cnToChanId)) ]
      chanZip = Z.findRight (== townSqId) (Z.fromList chanIds)
      st = newState cr chanZip myUser myTeam tz hist
             & usrMap .~ fmap mkUser users
             & msgMap .~ msgs
             & csNames .~ chanNames

  -- Fetch town-square asynchronously, but put it in the queue early.
  case F.find ((== townSqId) . getId) chans of
      Nothing -> return ()
      Just c -> doAsyncWithIO Preempt st $ do
          cwd <- liftIO $ mmGetChannel session myTeamId (getId c)
          return $ do
              csChannel(getId c).ccInfo %= channelInfoFromChannelWithData cwd
              asyncFetchScrollback Preempt (getId c)

  -- It's important to queue up these channel metadata fetches first so
  -- that by the time the scrollback requests are processed, we have the
  -- latest metadata.
  --
  -- First we queue up fetches for non-DM channels:
  F.forM_ chans $ \c ->
      when (getId c /= townSqId && c^.channelTypeL /= Direct) $
          doAsyncWithIO Normal st $ do
              cwd <- liftIO $ mmGetChannel session myTeamId (getId c)
              return $ do
                  csChannel(getId c).ccInfo %= channelInfoFromChannelWithData cwd

  -- Then we queue up fetches for DM channels:
  F.forM_ chans $ \c ->
      when (c^.channelTypeL == Direct) $
          doAsyncWithIO Normal st $ do
              cwd <- liftIO $ mmGetChannel session myTeamId (getId c)
              return $ do
                  csChannel(getId c).ccInfo %= channelInfoFromChannelWithData cwd

  updateViewedIO st
