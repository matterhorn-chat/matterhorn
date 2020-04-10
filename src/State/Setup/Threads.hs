{-# LANGUAGE OverloadedStrings #-}
module State.Setup.Threads
  ( startUserStatusUpdateThread
  , startTypingUsersRefreshThread
  , startSubprocessLoggerThread
  , startTimezoneMonitorThread
  , maybeStartSpellChecker
  , startAsyncWorkerThread
  , startSyntaxMapLoaderThread
  , module State.Setup.Threads.Logging
  )
where

import           Prelude ()
import           Prelude.MH

import           Brick.BChan ( BChan )
import           Brick.Main ( invalidateCache )
import           Control.Concurrent ( threadDelay, forkIO )
import qualified Control.Concurrent.STM as STM
import           Control.Concurrent.STM.Delay
import           Control.Exception ( SomeException, try, fromException )
import           Data.List ( isInfixOf )
import qualified Data.Map as M
import qualified Data.Sequence as Seq
import qualified Data.Text as T
import           Data.Time ( getCurrentTime, addUTCTime )
import           Lens.Micro.Platform ( (.=), (%=), (%~), mapped )
import           Skylighting.Loader ( loadSyntaxesFromDir )
import           System.Directory ( getTemporaryDirectory )
import           System.Exit ( ExitCode(ExitSuccess) )
import           System.IO ( hPutStrLn, hFlush )
import           System.IO.Temp ( openTempFile )
import           System.Timeout ( timeout )
import           Text.Aspell ( Aspell, AspellOption(..), startAspell )

import           Network.Mattermost.Endpoints
import           Network.Mattermost.Types

import           Constants
import           State.Editing ( requestSpellCheck )
import           State.Setup.Threads.Logging
import           TimeUtils ( lookupLocalTimeZone )
import           Types


updateUserStatuses :: [UserId] -> Session -> IO (Maybe (MH ()))
updateUserStatuses uIds session =
    case null uIds of
        False -> do
            statuses <- mmGetUserStatusByIds (Seq.fromList uIds) session
            return $ Just $ do
                forM_ statuses $ \s ->
                    setUserStatus (statusUserId s) (statusStatus s)
        True -> return Nothing

startUserStatusUpdateThread :: STM.TChan [UserId] -> Session -> RequestChan -> IO ()
startUserStatusUpdateThread zipperChan session requestChan = void $ forkIO body
  where
      seconds = (* (1000 * 1000))
      userRefreshInterval = 30
      body = refresh []
      refresh prev = do
          result <- timeout (seconds userRefreshInterval) (STM.atomically $ STM.readTChan zipperChan)
          let (uIds, update) = case result of
                  Nothing -> (prev, True)
                  Just ids -> (ids, ids /= prev)

          when update $ do
              STM.atomically $ STM.writeTChan requestChan $ do
                  rs <- try $ updateUserStatuses uIds session
                  case rs of
                      Left (_ :: SomeException) -> return Nothing
                      Right upd -> return upd

          refresh uIds

-- This thread refreshes the map of typing users every second, forever,
-- to expire users who have stopped typing. Expiry time is 3 seconds.
startTypingUsersRefreshThread :: RequestChan -> IO ()
startTypingUsersRefreshThread requestChan = void $ forkIO $ forever refresh
  where
    seconds = (* (1000 * 1000))
    refreshIntervalMicros = ceiling $ seconds $ userTypingExpiryInterval / 2
    refresh = do
      STM.atomically $ STM.writeTChan requestChan $ return $ Just $ do
        now <- liftIO getCurrentTime
        let expiry = addUTCTime (- userTypingExpiryInterval) now
        let expireUsers c = c & ccInfo.cdTypingUsers %~ expireTypingUsers expiry
        csChannels . mapped %= expireUsers

      threadDelay refreshIntervalMicros

startSubprocessLoggerThread :: STM.TChan ProgramOutput -> RequestChan -> IO ()
startSubprocessLoggerThread logChan requestChan = do
    let logMonitor mPair = do
          ProgramOutput progName args out stdoutOkay err ec <-
              STM.atomically $ STM.readTChan logChan

          -- If either stdout or stderr is non-empty or there was an exit
          -- failure, log it and notify the user.
          let emptyOutput s = null s || s == "\n"

          case ec == ExitSuccess && (emptyOutput out || stdoutOkay) && emptyOutput err of
              -- the "good" case, no output and exit sucess
              True -> logMonitor mPair
              False -> do
                  (logPath, logHandle) <- case mPair of
                      Just p ->
                          return p
                      Nothing -> do
                          tmp <- getTemporaryDirectory
                          openTempFile tmp "matterhorn-subprocess.log"

                  let unexpectedStdout = ec == ExitSuccess && (not $ emptyOutput out) && not stdoutOkay

                  hPutStrLn logHandle $
                      unlines [ "Program: " <> progName
                              , "Arguments: " <> show args
                              , "Exit code: " <> show ec
                              , if unexpectedStdout
                                then "Stdout (unexpected due to successful exit):"
                                else "Stdout:"
                              , out
                              , "Stderr:"
                              , err
                              ]
                  hFlush logHandle

                  STM.atomically $ STM.writeTChan requestChan $ do
                      return $ Just $ mhError $ ProgramExecutionFailed (T.pack progName) (T.pack logPath)

                  logMonitor (Just (logPath, logHandle))

    void $ forkIO $ logMonitor Nothing

startTimezoneMonitorThread :: TimeZoneSeries -> RequestChan -> IO ()
startTimezoneMonitorThread tz requestChan = do
  -- Start the timezone monitor thread
  let timezoneMonitorSleepInterval = minutes 5
      minutes = (* (seconds 60))
      seconds = (* (1000 * 1000))
      timezoneMonitor prevTz = do
        threadDelay timezoneMonitorSleepInterval

        newTz <- lookupLocalTimeZone
        when (newTz /= prevTz) $
            STM.atomically $ STM.writeTChan requestChan $ do
                return $ Just $ do
                    timeZone .= newTz
                    mh invalidateCache

        timezoneMonitor newTz

  void $ forkIO (timezoneMonitor tz)

maybeStartSpellChecker :: Config -> BChan MHEvent -> IO (Maybe (Aspell, IO ()))
maybeStartSpellChecker config eventQueue = do
  case configEnableAspell config of
      False -> return Nothing
      True -> do
          let aspellOpts = catMaybes [ UseDictionary <$> (configAspellDictionary config)
                                     ]
              spellCheckerTimeout = 500 * 1000 -- 500k us = 500ms
          asResult <- either (const Nothing) Just <$> startAspell aspellOpts
          case asResult of
              Nothing -> return Nothing
              Just as -> do
                  resetSCChan <- startSpellCheckerThread eventQueue spellCheckerTimeout
                  let resetSCTimer = STM.atomically $ STM.writeTChan resetSCChan ()
                  return $ Just (as, resetSCTimer)

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
            -- keystrokes will trigger another timer anyway.
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

startSyntaxMapLoaderThread :: Config -> BChan MHEvent -> IO ()
startSyntaxMapLoaderThread config eventChan = void $ forkIO $ do
    -- Iterate over the configured syntax directories, loading syntax
    -- maps. Ensure that entries loaded in earlier directories in the
    -- sequence take precedence over entries loaded later.
    mMaps <- forM (configSyntaxDirs config) $ \dir -> do
        result <- try $ loadSyntaxesFromDir dir
        case result of
            Left (_::SomeException) -> return Nothing
            Right (Left _)          -> return Nothing
            Right (Right m)         -> return $ Just m

    let maps = catMaybes mMaps
        finalMap = foldl M.union mempty maps

    writeBChan eventChan $ RespEvent $ do
        csResources.crSyntaxMap .= finalMap
        mh invalidateCache

-------------------------------------------------------------------
-- Async worker thread

startAsyncWorkerThread :: Config -> STM.TChan (IO (Maybe (MH ()))) -> BChan MHEvent -> IO ()
startAsyncWorkerThread c r e = void $ forkIO $ asyncWorker c r e

asyncWorker :: Config -> STM.TChan (IO (Maybe (MH ()))) -> BChan MHEvent -> IO ()
asyncWorker c r e = forever $ doAsyncWork c r e

doAsyncWork :: Config -> STM.TChan (IO (Maybe (MH ()))) -> BChan MHEvent -> IO ()
doAsyncWork config requestChan eventChan = do
    startWork <- case configShowBackground config of
        Disabled -> return $ return ()
        Active -> do chk <- STM.atomically $ STM.tryPeekTChan requestChan
                     case chk of
                       Nothing -> do writeBChan eventChan BGIdle
                                     return $ writeBChan eventChan $ BGBusy Nothing
                       _ -> return $ return ()
        ActiveCount -> do
          chk <- STM.atomically $ do
            chanCopy <- STM.cloneTChan requestChan
            let cntMsgs = do m <- STM.tryReadTChan chanCopy
                             case m of
                               Nothing -> return 0
                               Just _ -> (1 +) <$> cntMsgs
            cntMsgs
          case chk of
            0 -> do writeBChan eventChan BGIdle
                    return (writeBChan eventChan $ BGBusy (Just 1))
            _ -> do writeBChan eventChan $ BGBusy (Just chk)
                    return $ return ()

    req <- STM.atomically $ STM.readTChan requestChan
    startWork
    res <- try req
    case res of
      Left e -> do
          when (not $ shouldIgnore e) $ do
              let err = case fromException e of
                    Nothing -> AsyncErrEvent e
                    Just mmErr -> ServerError mmErr
              writeBChan eventChan $ IEvent $ DisplayError err
      Right upd ->
          case upd of
              Nothing -> return ()
              Just action -> writeBChan eventChan (RespEvent action)

-- Filter for exceptions that we don't want to report to the user,
-- probably because they are not actionable and/or contain no useful
-- information.
--
-- E.g.
-- https://github.com/matterhorn-chat/matterhorn/issues/391
shouldIgnore :: SomeException -> Bool
shouldIgnore e =
    let eStr = show e
    in or [ "getAddrInfo" `isInfixOf` eStr
          , "Network.Socket.recvBuf" `isInfixOf` eStr
          , "Network.Socket.sendBuf" `isInfixOf` eStr
          , "resource vanished" `isInfixOf` eStr
          , "timeout" `isInfixOf` eStr
          ]
