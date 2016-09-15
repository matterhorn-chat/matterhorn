module Connection where

import           Control.Concurrent (forkIO, threadDelay)
import qualified Control.Concurrent.Chan as Chan
import           Control.Concurrent.MVar
import           Control.Exception (SomeException, catch)
import           Control.Monad (void)
import           Lens.Micro.Platform

import           Network.Mattermost.WebSocket

import           Types

connectWebsockets :: ChatState -> IO ()
connectWebsockets st = do
  let shunt e = Chan.writeChan (st^.csResources.crEventQueue) (WSEvent e)
  let runWS = mmWithWebSocket (st^.csConn) (st^.csTok) shunt $ \ _ -> do
                Chan.writeChan (st^.csResources.crEventQueue) WebsocketConnect
                waitAndQuit st
  void $ forkIO $ runWS `catch` handleTimeout 1 st
                        `catch` handleError 5 st

waitAndQuit :: ChatState -> IO ()
waitAndQuit st =
  void (takeMVar (st^.csResources.crQuitCondition))

handleTimeout :: Int -> ChatState -> MMWebSocketTimeoutException -> IO ()
handleTimeout seconds st _ = reconnectAfter seconds st

handleError :: Int -> ChatState -> SomeException -> IO ()
handleError seconds st _ = reconnectAfter seconds st

reconnectAfter :: Int -> ChatState -> IO ()
reconnectAfter seconds st = do
  Chan.writeChan (st^.csResources.crEventQueue) WebsocketDisconnect
  threadDelay (seconds * 1000 * 1000)
  Chan.writeChan (st^.csResources.crEventQueue) RefreshWebsocketEvent
