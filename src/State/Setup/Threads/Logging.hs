{-# LANGUAGE RecordWildCards #-}
-- | This module implements the logging thread. This thread can
-- optionally write logged messages to an output file. When the thread
-- is created, it is initially not writing to any output file and
-- must be told to do so by issuing a LogCommand using the LogManager
-- interface.
--
-- The logging thread has an internal bounded log message buffer. Logged
-- messages always get written to the buffer (potentially evicting old
-- messages to maintain the size bound). If the thread is also writing
-- to a file, such messages also get written to the file. When the
-- thread begins logging to a file, the entire buffer is written to the
-- file so that a historical snapshot of log activity can be saved in
-- cases where logging is turned on at runtime only once a problematic
-- behavior is observed.
module State.Setup.Threads.Logging
  ( newLogManager
  , shutdownLogManager
  )
where

import           Prelude ()
import           Prelude.MH

import           Brick.BChan
import           Control.Concurrent.Async ( Async, async, wait )
import qualified Control.Concurrent.STM as STM
import           Control.Exception ( SomeException, try )
import           Control.Monad.State.Strict
import qualified Data.Sequence as Seq
import qualified Data.Text as T
import           Data.Time ( getCurrentTime )
import           System.IO ( Handle, IOMode(AppendMode), hPutStr, hPutStrLn
                           , hFlush, openFile, hClose )

import           Types


-- | The state of the logging thread.
data LogThreadState =
    LogThreadState { logThreadDestination :: Maybe (FilePath, Handle)
                   -- ^ The logging thread's active logging destination.
                   -- Nothing means log messages are not being written
                   -- anywhere except the internal buffer.
                   , logThreadEventChan :: BChan MHEvent
                   -- ^ The application event channel that we'll use to
                   -- notify of logging events.
                   , logThreadCommandChan :: STM.TChan LogCommand
                   -- ^ The channel on which the logging thread will
                   -- wait for logging commands.
                   , logThreadMessageBuffer :: Seq.Seq LogMessage
                   -- ^ The internal bounded log message buffer.
                   , logThreadMaxBufferSize :: Int
                   -- ^ The size bound of the logThreadMessageBuffer.
                   }

-- | Create a new log manager and start a logging thread for it.
newLogManager :: BChan MHEvent -> Int -> IO LogManager
newLogManager eventChan maxBufferSize = do
    chan <- STM.newTChanIO
    self <- startLoggingThread eventChan chan maxBufferSize
    let mgr = LogManager { logManagerCommandChannel = chan
                         , logManagerHandle = self
                         }
    return mgr

-- | Shuts down the log manager and blocks until shutdown is complete.
shutdownLogManager :: LogManager -> IO ()
shutdownLogManager mgr = do
    STM.atomically $ STM.writeTChan (logManagerCommandChannel mgr) ShutdownLogging
    wait $ logManagerHandle mgr

-- | The logging thread.
startLoggingThread :: BChan MHEvent -> STM.TChan LogCommand -> Int -> IO (Async ())
startLoggingThread eventChan logChan maxBufferSize = do
    let initialState = LogThreadState { logThreadDestination = Nothing
                                      , logThreadEventChan = eventChan
                                      , logThreadCommandChan = logChan
                                      , logThreadMessageBuffer = mempty
                                      , logThreadMaxBufferSize = maxBufferSize
                                      }
    async $ void $ runStateT logThreadBody initialState

logThreadBody :: StateT LogThreadState IO ()
logThreadBody = do
    cmd <- nextLogCommand
    continue <- handleLogCommand cmd
    when continue logThreadBody

-- | Get the next pending log thread command.
nextLogCommand :: StateT LogThreadState IO LogCommand
nextLogCommand = do
    chan <- gets logThreadCommandChan
    liftIO $ STM.atomically $ STM.readTChan chan

putMarkerMessage :: String -> Handle -> IO ()
putMarkerMessage msg h = do
    now <- getCurrentTime
    hPutStrLn h $ "[" <> show now <> "] " <> msg

-- | Emit a log stop marker to the file.
putLogEndMarker :: Handle -> IO ()
putLogEndMarker = putMarkerMessage "<<< Logging end >>>"

-- | Emit a log start marker to the file.
putLogStartMarker :: Handle -> IO ()
putLogStartMarker = putMarkerMessage "<<< Logging start >>>"

-- | Emit a log stop marker to the file and close it, then notify the
-- application that we have stopped logging.
finishLog :: BChan MHEvent -> FilePath -> Handle -> StateT LogThreadState IO ()
finishLog eventChan oldPath oldHandle = do
    liftIO $ do
        putLogEndMarker oldHandle
        hClose oldHandle
        writeBChan eventChan $ IEvent $ LoggingStopped oldPath
    modify $ \s -> s { logThreadDestination = Nothing }

stopLogging :: StateT LogThreadState IO ()
stopLogging = do
    oldDest <- gets logThreadDestination
    case oldDest of
        Nothing -> return ()
        Just (oldPath, oldHandle) -> do
            eventChan <- gets logThreadEventChan
            finishLog eventChan oldPath oldHandle

-- | Handle a single logging command.
handleLogCommand :: LogCommand -> StateT LogThreadState IO Bool
handleLogCommand (LogSnapshot path) = do
    -- LogSnapshot: write the current log message buffer to the
    -- specified path. Ignore the request if it is for the path that we
    -- are already logging to.
    eventChan <- gets logThreadEventChan
    dest <- gets logThreadDestination

    let shouldWrite = case dest of
          Nothing -> True
          Just (curPath, _) -> curPath /= path

    when shouldWrite $ do
        result <- liftIO $ try $ openFile path AppendMode
        case result of
            Left (e::SomeException) -> do
                liftIO $ writeBChan eventChan $ IEvent $ LogSnapshotFailed path (show e)
            Right handle -> do
                flushLogMessageBuffer handle
                liftIO $ hClose handle
                liftIO $ writeBChan eventChan $ IEvent $ LogSnapshotSucceeded path

    return True
handleLogCommand GetLogDestination = do
    -- GetLogDestination: the application asked us to provide the
    -- current log destination.
    dest <- gets logThreadDestination
    eventChan <- gets logThreadEventChan
    liftIO $ writeBChan eventChan $ IEvent $ LogDestination $ fst <$> dest
    return True
handleLogCommand ShutdownLogging = do
    -- ShutdownLogging: if we were logging to a file, close it. Then
    -- unlock the shutdown lock.
    stopLogging
    return False
handleLogCommand StopLogging = do
    -- StopLogging: if we were logging to a file, close it and notify
    -- the application. Otherwise do nothing.
    stopLogging
    return True
handleLogCommand (LogToFile newPath) = do
    -- LogToFile: if we were logging to a file, close that file, notify
    -- the application, then attempt to open the new file. If that
    -- failed, notify the application of the error. If it succeeded,
    -- start logging and notify the application.
    eventChan <- gets logThreadEventChan
    oldDest <- gets logThreadDestination

    shouldChange <- case oldDest of
        Nothing ->
            return True
        Just (oldPath, _) ->
            return (oldPath /= newPath)

    when shouldChange $ do
        result <- liftIO $ try $ openFile newPath AppendMode
        case result of
            Left (e::SomeException) -> liftIO $ do
                let msg = "Error in log thread: could not open " <> show newPath <>
                          ": " <> show e
                writeBChan eventChan $ IEvent $ LogStartFailed newPath msg
            Right handle -> do
                stopLogging

                modify $ \s -> s { logThreadDestination = Just (newPath, handle) }
                flushLogMessageBuffer handle
                liftIO $ putLogStartMarker handle
                liftIO $ writeBChan eventChan $ IEvent $ LoggingStarted newPath

    return True
handleLogCommand (LogAMessage lm) = do
    -- LogAMessage: log a single message. Write the message to the
    -- bounded internal buffer (which may cause an older message to be
    -- evicted). Then, if we are actively logging to a file, write the
    -- message to that file and flush the output stream.
    maxBufSize <- gets logThreadMaxBufferSize

    let addMessageToBuffer s =
            -- Ensure that newSeq is always at most maxBufSize elements
            -- long.
            let newSeq = s Seq.|> lm
                toDrop = Seq.length s - maxBufSize
            in Seq.drop toDrop newSeq

    -- Append the message to the internal buffer, maintaining the bound
    -- on the internal buffer size.
    modify $ \s -> s { logThreadMessageBuffer = addMessageToBuffer (logThreadMessageBuffer s) }

    -- If we have an active log destination, write the message to the
    -- output file.
    dest <- gets logThreadDestination
    case dest of
        Nothing -> return ()
        Just (_, handle) -> liftIO $ do
            hPutLogMessage handle lm
            hFlush handle

    return True

-- | Write a single log message to the output handle.
hPutLogMessage :: Handle -> LogMessage -> IO ()
hPutLogMessage handle (LogMessage {..}) = do
    hPutStr handle $ "[" <> show logMessageTimestamp <> "] "
    hPutStr handle $ "[" <> show logMessageCategory <> "] "
    case logMessageContext of
        Nothing -> hPutStr handle "[No context] "
        Just c  -> hPutStr handle $ "[" <> show c <> "] "
    hPutStrLn handle $ T.unpack logMessageText

-- | Flush the contents of the internal log message buffer.
flushLogMessageBuffer :: Handle -> StateT LogThreadState IO ()
flushLogMessageBuffer handle = do
    buf <- gets logThreadMessageBuffer
    when (Seq.length buf > 0) $ do
        liftIO $ do
            let firstLm = Seq.index buf 0
                lastLm = Seq.index buf (Seq.length buf - 1)
                mkMsg t m = "[" <> show t <> "] " <> m

            hPutStrLn handle $ mkMsg (logMessageTimestamp firstLm)
                                     "<<< Log message buffer begin >>>"

            forM_ buf (hPutLogMessage handle)

            hPutStrLn handle $ mkMsg (logMessageTimestamp lastLm)
                                     "<<< Log message buffer end >>>"

            hFlush handle
