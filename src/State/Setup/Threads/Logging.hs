{-# LANGUAGE RecordWildCards #-}
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
import           System.IO ( Handle, IOMode(AppendMode), hPutStr, hPutStrLn, hFlush, openFile, hClose )

import           Types


data LogThreadState =
    LogThreadState { logThreadDestination :: Maybe (FilePath, Handle)
                   , logThreadEventChan :: BChan MHEvent
                   , logThreadCommandChan :: STM.TChan LogCommand
                   , logThreadMessageBuffer :: Seq.Seq LogMessage
                   , logThreadMaxBufferSize :: Int
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

nextLogCommand :: St.StateT LogThreadState IO LogCommand
nextLogCommand = do
    chan <- St.gets logThreadCommandChan
    liftIO $ STM.atomically $ STM.readTChan chan

putLogEndMarker :: Handle -> IO ()
putLogEndMarker h = do
    now <- getCurrentTime
    hPutStrLn h $ "[" <> show now <> "] <<< Logging end >>>"

putLogStartMarker :: Handle -> IO ()
putLogStartMarker h = do
    now <- getCurrentTime
    hPutStrLn h $ "[" <> show now <> "] <<< Logging start >>>"

finishLog :: BChan MHEvent -> FilePath -> Handle -> IO ()
finishLog eventChan oldPath oldHandle = do
    putLogEndMarker oldHandle
    hClose oldHandle
    writeBChan eventChan $ IEvent $ LoggingStopped oldPath

handleLogCommand :: LogCommand -> St.StateT LogThreadState IO ()
handleLogCommand StopLogging = do
    oldDest <- St.gets logThreadDestination
    case oldDest of
        Nothing -> return ()
        Just (oldPath, oldHandle) -> do
            eventChan <- St.gets logThreadEventChan
            liftIO $ finishLog eventChan oldPath oldHandle
            St.modify $ \s -> s { logThreadDestination = Nothing
                                }
handleLogCommand (LogToFile newPath) = do
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

hPutLogMessage :: Handle -> LogMessage -> IO ()
hPutLogMessage handle (LogMessage {..}) = do
    hPutStr handle $ "[" <> show logMessageTimestamp <> "] "
    hPutStr handle $ "[" <> show logMessageCategory <> "] "
    case logMessageContext of
        Nothing -> hPutStr handle "[No context] "
        Just c  -> hPutStr handle $ "[" <> show c <> "] "
    hPutStrLn handle $ T.unpack logMessageText

flushLogMessageBuffer :: Handle -> St.StateT LogThreadState IO ()
flushLogMessageBuffer handle = do
    buf <- St.gets logThreadMessageBuffer
    when (Seq.length buf > 0) $ do
        liftIO $ do
            hPutStrLn handle "<<< Log message buffer begin >>>"
            forM_ buf (hPutLogMessage handle)
            hPutStrLn handle "<<< Log message buffer end >>>"
            hFlush handle
        St.modify $ \s -> s { logThreadMessageBuffer = mempty }

