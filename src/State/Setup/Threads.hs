module State.Setup.Threads
  ( startUserRefreshThread
  , updateUserStatuses
  , startSubprocessLoggerThread
  , startTimezoneMonitorThread
  , maybeStartSpellChecker
  )
where

import           Prelude ()
import           Prelude.Compat

import           Brick.BChan
import           Control.Concurrent (threadDelay, forkIO, MVar, putMVar, tryTakeMVar)
import qualified Control.Concurrent.STM as STM
import           Control.Concurrent.STM.Delay
import           Control.Exception (SomeException, try, finally)
import           Control.Monad (forever, when, void)
import qualified Data.Text as T
import           Data.Maybe (catMaybes)
import           Data.Monoid ((<>))
import           Data.Time.LocalTime ( TimeZone(..), getCurrentTimeZone )
import           Lens.Micro.Platform
import           System.Exit (ExitCode(ExitSuccess))
import           System.IO (hPutStrLn, hFlush)
import           System.IO.Temp (openTempFile)
import           System.Directory (getTemporaryDirectory)
import           Text.Aspell (Aspell, AspellOption(..), startAspell)

import           Network.Mattermost

import           State.Common
import           State.Editing (requestSpellCheck)
import           Types
import           Types.Users

updateUserStatuses :: MVar () -> Session -> IO (MH ())
updateUserStatuses lock session = do
  lockResult <- tryTakeMVar lock

  case lockResult of
      Nothing -> return $ return ()
      Just () -> do
          statusMap <- mmGetStatuses session `finally` putMVar lock ()
          return $ do
            let setStatus u = u & uiStatus .~ (newsts u)
                newsts u = (statusMap^.at(u^.uiId) & _Just %~ statusFromText) ^. non Offline
            csUsers . mapped %= setStatus

startUserRefreshThread :: MVar () -> Session -> RequestChan -> IO ()
startUserRefreshThread lock session requestChan = void $ forkIO $ forever refresh
  where
      seconds = (* (1000 * 1000))
      userRefreshInterval = 30
      refresh = do
          STM.atomically $ STM.writeTChan requestChan $ do
            rs <- try $ updateUserStatuses lock session
            case rs of
              Left (_ :: SomeException) -> return (return ())
              Right upd -> return upd
          threadDelay (seconds userRefreshInterval)

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
                          let msg = T.pack $
                                "An error occurred when running " <> show progName <>
                                "; see " <> logPath <> " for details."
                          postErrorMessage msg

                  logMonitor (Just (logPath, logHandle))

    void $ forkIO $ logMonitor Nothing

startTimezoneMonitorThread :: TimeZone -> RequestChan -> IO ()
startTimezoneMonitorThread tz requestChan = do
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
