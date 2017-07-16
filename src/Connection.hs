module Connection where

import           Prelude ()
import           Prelude.Compat

import           Brick.BChan
import           Control.Concurrent (forkIO, threadDelay)
import           Control.Concurrent.MVar
import           Control.Exception (SomeException, catch)
import           Control.Monad (void)
import           Lens.Micro.Platform

import           Network.Mattermost.WebSocket

import           Types

connectWebsockets :: ChatState -> IO ()
connectWebsockets st = do
  let shunt e = writeBChan (st^.csResources.crEventQueue) (WSEvent e)
  let runWS = mmWithWebSocket (st^.csResources.crSession) shunt $ \ _ -> do
                writeBChan (st^.csResources.crEventQueue) WebsocketConnect
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
  writeBChan (st^.csResources.crEventQueue) WebsocketDisconnect
  threadDelay (seconds * 1000 * 1000)
  writeBChan (st^.csResources.crEventQueue) RefreshWebsocketEvent
