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
  ( startLoggingThread
  )
where

import           Prelude ()
import           Prelude.MH

import           Brick.BChan
import           Control.Concurrent ( forkIO )
import qualified Control.Concurrent.STM as STM
import           Control.Exception ( SomeException, try )
import qualified Control.Monad.State.Strict as St
import qualified Data.Text as T
import           Data.Time ( getCurrentTime )
import qualified Data.Sequence as Seq
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

-- | The logging thread.
startLoggingThread :: BChan MHEvent -> LogManager -> Int -> IO ()
startLoggingThread eventChan mgr maxBufferSize = do
    let initialState = LogThreadState { logThreadDestination = Nothing
                                      , logThreadEventChan = eventChan
                                      , logThreadCommandChan = logManagerCommandChannel mgr
                                      , logThreadMessageBuffer = mempty
                                      , logThreadMaxBufferSize = maxBufferSize
                                      }
    void $ forkIO $
        void $ St.runStateT logThreadBody initialState

logThreadBody :: St.StateT LogThreadState IO ()
logThreadBody = forever $ nextLogCommand >>= handleLogCommand

-- | Get the next pending log thread command.
nextLogCommand :: St.StateT LogThreadState IO LogCommand
nextLogCommand = do
    chan <- St.gets logThreadCommandChan
    liftIO $ STM.atomically $ STM.readTChan chan

-- | Emit a log stop marker to the file.
putLogEndMarker :: Handle -> IO ()
putLogEndMarker h = do
    now <- getCurrentTime
    hPutStrLn h $ "[" <> show now <> "] <<< Logging end >>>"

-- | Emit a log start marker to the file.
putLogStartMarker :: Handle -> IO ()
putLogStartMarker h = do
    now <- getCurrentTime
    hPutStrLn h $ "[" <> show now <> "] <<< Logging start >>>"

-- | Emit a log stop marker to the file and close it, then notify the
-- application that we have stopped logging.
finishLog :: BChan MHEvent -> FilePath -> Handle -> IO ()
finishLog eventChan oldPath oldHandle = do
    putLogEndMarker oldHandle
    hClose oldHandle
    writeBChan eventChan $ IEvent $ LoggingStopped oldPath

-- | Handle a single logging command.
handleLogCommand :: LogCommand -> St.StateT LogThreadState IO ()
handleLogCommand StopLogging = do
    -- StopLogging: if we were logging to a file, close it and notify
    -- the application. Otherwise do nothing.
    oldDest <- St.gets logThreadDestination
    case oldDest of
        Nothing -> return ()
        Just (oldPath, oldHandle) -> do
            eventChan <- St.gets logThreadEventChan
            liftIO $ finishLog eventChan oldPath oldHandle
            St.modify $ \s -> s { logThreadDestination = Nothing
                                }
handleLogCommand (LogToFile newPath) = do
    -- LogToFile: if we were logging to a file, close that file, notify
    -- the application, then attempt to open the new file. If that
    -- failed, notify the application of the error. If it succeeded,
    -- start logging and notify the application.
    eventChan <- St.gets logThreadEventChan
    oldDest <- St.gets logThreadDestination

    shouldChange <- case oldDest of
        Nothing -> return True
        Just (oldPath, oldHandle) ->
            if oldPath == newPath
            then return False
            else do
                liftIO $ finishLog eventChan oldPath oldHandle
                return True

    when shouldChange $ do
        result <- liftIO $ try $ openFile newPath AppendMode
        case result of
            Left (e::SomeException) -> liftIO $ do
                let msg = "Error in log thread: could not open " <> show newPath <> ": " <> show e
                writeBChan eventChan $ IEvent $ LogStartFailed newPath msg
            Right handle -> do
                St.modify $ \s -> s { logThreadDestination = Just (newPath, handle)
                                    }
                flushLogMessageBuffer handle
                liftIO $ putLogStartMarker handle
                liftIO $ writeBChan eventChan $ IEvent $ LoggingStarted newPath
handleLogCommand (LogAMessage lm) = do
    -- LogAMessage: log a single message. Write the message to the
    -- bounded internal buffer (which may cause an older message to be
    -- evicted). Then, if we are actively logging to a file, write the
    -- message to that file and flush the output stream.
    maxBufSize <- St.gets logThreadMaxBufferSize

    let addMessageToBuffer s =
            let newSeq = if Seq.length s >= maxBufSize
                         then Seq.drop 1 s
                         else s
            in newSeq Seq.|> lm

    -- Append the message to the internal buffer, maintaining the bound
    -- on the internal buffer size.
    St.modify $ \s -> s { logThreadMessageBuffer = addMessageToBuffer (logThreadMessageBuffer s)
                        }

    -- If we have an active log destination, write the message to the
    -- output file.
    dest <- St.gets logThreadDestination
    case dest of
        Nothing -> return ()
        Just (_, handle) -> liftIO $ do
            hPutLogMessage handle lm
            hFlush handle

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
flushLogMessageBuffer :: Handle -> St.StateT LogThreadState IO ()
flushLogMessageBuffer handle = do
    buf <- St.gets logThreadMessageBuffer
    when (Seq.length buf > 0) $ do
        liftIO $ do
            hPutStrLn handle "<<< Log message buffer begin >>>"
            forM_ buf (hPutLogMessage handle)
            hPutStrLn handle "<<< Log message buffer end >>>"
            hFlush handle
