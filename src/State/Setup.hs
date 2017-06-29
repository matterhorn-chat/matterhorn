{-# LANGUAGE TypeFamilies #-}

module State.Setup where

import           Prelude ()
import           Prelude.Compat

import           Brick.BChan
import           Brick.Widgets.List (list)
import           Control.Concurrent (threadDelay, forkIO)
import qualified Control.Concurrent.STM as STM
import           Control.Concurrent.STM.Delay
import           Control.Concurrent.MVar (newEmptyMVar)
import           Control.Exception (SomeException, catch, try)
import           Control.Monad (forM, forever, when, void)
import qualified Data.Text as T
import qualified Data.Foldable as F
import qualified Data.HashMap.Strict as HM
import           Data.List (sort)
import           Data.Maybe (listToMaybe, maybeToList, fromJust, catMaybes)
import           Data.Monoid ((<>))
import qualified Data.Sequence as Seq
import           Data.Time.LocalTime ( TimeZone(..), getCurrentTimeZone )
import           Lens.Micro.Platform
import           System.Exit (exitFailure, ExitCode(ExitSuccess))
import           System.IO (Handle, hPutStrLn, hFlush)
import           System.IO.Temp (openTempFile)
import           System.Directory (getTemporaryDirectory)
import           Text.Aspell (Aspell, AspellOption(..), startAspell)

import           Network.Mattermost
import           Network.Mattermost.Lenses
import           Network.Mattermost.Logging (mmLoggerDebug)

import           Config
import           InputHistory
import           Login
import           State.Common
import           State.Editing (requestSpellCheck)
import           TeamSelect
import           Themes
import           Types
import           Types.Channels
import           Types.Users
import           Zipper (Zipper)
import qualified Zipper as Z

updateUserStatuses :: Session -> IO (MH ())
updateUserStatuses session = do
  statusMap <- mmGetStatuses session
  return $ do
    let setStatus u = u & uiStatus .~ (newsts u)
        newsts u = (statusMap^.at(u^.uiId) & _Just %~ statusFromText) ^. non Offline
    csUsers . mapped %= setStatus

userRefresh :: Session -> RequestChan -> IO ()
userRefresh session requestChan = void $ forkIO $ forever refresh
  where refresh = do
          let seconds = (* (1000 * 1000))
          threadDelay (seconds 30)
          STM.atomically $ STM.writeTChan requestChan $ do
            rs <- try $ updateUserStatuses session
            case rs of
              Left (_ :: SomeException) -> return (return ())
              Right upd -> return upd

startSubprocessLogger :: STM.TChan ProgramOutput -> RequestChan -> IO ()
startSubprocessLogger logChan requestChan = do
    let logMonitor mPair = do
          ProgramOutput progName args out err ec <-
              STM.atomically $ STM.readTChan logChan

          -- If either stdout or stderr is non-empty or there was an exit
          -- failure, log it and notify the user.
          let emptyOutput s = null s || s == "\n"

          case ec == ExitSuccess && emptyOutput out && emptyOutput err of
              -- the "good" case, no output and exit sucess
              True -> logMonitor mPair
              False -> do
                  (logPath, logHandle) <- case mPair of
                      Just p ->
                          return p
                      Nothing -> do
                          tmp <- getTemporaryDirectory
                          openTempFile tmp "matterhorn-subprocess.log"

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

                  logMonitor (Just (logPath, logHandle))

    void $ forkIO $ logMonitor Nothing

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
         -> Maybe Aspell
         -> IO ()
         -> ChatState
newState rs i u m tz hist sp resetTimer = ChatState
  { _csResources                   = rs
  , _csFocus                       = i
  , _csMe                          = u
  , _csMyTeam                      = m
  , _csNames                       = emptyMMNames
  , _csChannels                    = noChannels
  , _csPostMap                     = HM.empty
  , _csUsers                       = noUsers
  , _timeZone                      = tz
  , _csEditState                   = emptyEditState hist sp resetTimer
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

  STM.atomically $ STM.writeTChan requestChan $ updateUserStatuses session

  userRefresh session requestChan

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

  startTimezoneMonitor tz requestChan

  startSubprocessLogger (cr^.crSubprocessLog) requestChan

  -- Start the spell check timer thread.
  let spellCheckerTimeout = 500 * 1000 -- 500k us = 500ms
  resetSCChan <- startSpellCheckerThread (cr^.crEventQueue) spellCheckerTimeout
  let resetSCTimer = STM.atomically $ STM.writeTChan resetSCChan ()

  sp <- case configEnableAspell $ cr^.crConfiguration of
      False -> return Nothing
      True ->
          let aspellOpts = catMaybes [ UseDictionary <$> (configAspellDictionary $ cr^.crConfiguration)
                                     ]
          in either (const Nothing) Just <$> startAspell aspellOpts

  let chanNames = mkChanNames myUser users chans
      Just townSqId = chanNames ^. cnToChanId . at "town-square"
      chanIds = [ (chanNames ^. cnToChanId) HM.! i
                | i <- chanNames ^. cnChans ] ++
                [ c
                | i <- chanNames ^. cnUsers
                , c <- maybeToList (HM.lookup i (chanNames ^. cnToChanId)) ]
      chanZip = Z.findRight (== townSqId) (Z.fromList chanIds)
      st = newState cr chanZip myUser myTeam tz hist sp resetSCTimer
             & csUsers %~ flip (foldr (uncurry addUser)) (fmap mkUser users)
             & csChannels %~ flip (foldr (uncurry addChannel)) msgs
             & csNames .~ chanNames

  return st

-- Start the background spell checker delay thread.
--
-- The purpose of this thread is to postpone the spell checker query
-- while the user is actively typing and only wait until they have
-- stopped typing before bothering with a query. This is to avoid spell
-- checker queries when the editor contents are changing rapidly.
-- Avoiding such queries reduces system load and redraw frequency.
--
-- We do this by starting a thread whose job is to wait for the event
-- loop to tell it to schedule a spell check. Spell checks are scheduled
-- by writing to the channel returned by this function. The scheduler
-- thread reads from that channel and then works with another worker
-- thread as follows:
--
-- A wakeup of the main spell checker thread causes it to determine
-- whether the worker thread is already waiting on a timer. When that
-- timer goes off, a spell check will be requested. If there is already
-- an active timer that has not yet expired, the timer's expiration is
-- extended. This is the case where typing is occurring and we want to
-- continue postponing the spell check. If there is not an active timer
-- or the active timer has expired, we create a new timer and send it to
-- the worker thread for waiting.
--
-- The worker thread works by reading a timer from its queue, waiting
-- until the timer expires, and then injecting an event into the main
-- event loop to request a spell check.
startSpellCheckerThread :: BChan MHEvent
                        -- ^ The main event loop's event channel.
                        -> Int
                        -- ^ The number of microseconds to wait before
                        -- requesting a spell check.
                        -> IO (STM.TChan ())
startSpellCheckerThread eventChan spellCheckTimeout = do
  delayWakeupChan <- STM.atomically STM.newTChan
  delayWorkerChan <- STM.atomically STM.newTChan
  delVar <- STM.atomically $ STM.newTVar Nothing

  -- The delay worker actually waits on the delay to expire and then
  -- requests a spell check.
  void $ forkIO $ forever $ do
    STM.atomically $ waitDelay =<< STM.readTChan delayWorkerChan
    writeBChan eventChan (RespEvent requestSpellCheck)

  -- The delay manager waits for requests to start a delay timer and
  -- signals the worker to begin waiting.
  void $ forkIO $ forever $ do
    () <- STM.atomically $ STM.readTChan delayWakeupChan

    oldDel <- STM.atomically $ STM.readTVar delVar
    mNewDel <- case oldDel of
        Nothing -> Just <$> newDelay spellCheckTimeout
        Just del -> do
            -- It's possible that between this check for expiration and
            -- the updateDelay below, the timer will expire -- at which
            -- point this will mean that we won't extend the timer as
            -- originally desired. But that's alright, because future
            -- keystroke will trigger another timer anyway.
            expired <- tryWaitDelayIO del
            case expired of
                True -> Just <$> newDelay spellCheckTimeout
                False -> do
                    updateDelay del spellCheckTimeout
                    return Nothing

    case mNewDel of
        Nothing -> return ()
        Just newDel -> STM.atomically $ do
            STM.writeTVar delVar $ Just newDel
            STM.writeTChan delayWorkerChan newDel

  return delayWakeupChan
