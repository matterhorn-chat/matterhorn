module AsyncThread
  ( startAsyncWorkerThread
  )
where

import           Prelude ()
import           Prelude.Compat

import           Brick.BChan
import           Control.Concurrent (forkIO)
import qualified Control.Concurrent.STM as STM
import           Control.Exception (SomeException, try)
import           Control.Monad (forever, void, when)
import           Data.List (isInfixOf)

import           Config
import           Types

startAsyncWorkerThread :: Config -> STM.TChan (IO (MH ())) -> BChan MHEvent -> IO ()
startAsyncWorkerThread c r e = void $ forkIO $ asyncWorker c r e

asyncWorker :: Config -> STM.TChan (IO (MH ())) -> BChan MHEvent -> IO ()
asyncWorker c r e = forever $ doAsyncWork c r e

doAsyncWork :: Config -> STM.TChan (IO (MH ())) -> BChan MHEvent -> IO ()
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
      Left e    -> when (not $ shouldIgnore e) $
                   writeBChan eventChan (AsyncErrEvent e)
      Right upd -> writeBChan eventChan (RespEvent upd)

-- Filter for exceptions that we don't want to report to the user,
-- probably because they are not actionable and/or contain no useful
-- information.
shouldIgnore :: SomeException -> Bool
shouldIgnore e = "resource vanished" `isInfixOf` show e
