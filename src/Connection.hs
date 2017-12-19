module Connection where

import           Prelude ()
import           Prelude.Compat

import           Brick.BChan
import           Control.Concurrent (forkIO, threadDelay)
import qualified Control.Concurrent.STM as STM
import           Control.Exception (SomeException, catch)
import           Control.Monad (void, forever)
import           Control.Monad.IO.Class (liftIO)
import           Data.Int (Int64)
import           Lens.Micro.Platform

import qualified Network.Mattermost.WebSocket as WS

import           Types

connectWebsockets :: MH ()
connectWebsockets = do
  st <- use id
  liftIO $ do
    let shunt (Left msg) = writeBChan (st^.csResources.crEventQueue) (WebsocketParseError msg)
        shunt (Right e) = writeBChan (st^.csResources.crEventQueue) (WSEvent e)
        runWS = WS.mmWithWebSocket (st^.csResources.crSession) shunt $ \ws -> do
                  writeBChan (st^.csResources.crEventQueue) WebsocketConnect
                  processWebsocketActions st ws 1
    void $ forkIO $ runWS `catch` handleTimeout 1 st
                          `catch` handleError 5 st

processWebsocketActions :: ChatState -> WS.MMWebSocket -> Int64 -> IO ()
processWebsocketActions st ws s = forever $ do
  action <- STM.atomically $ STM.readTChan (st^.csWebsocketActionChan)
  WS.mmSendWSAction (st^.csResources.crConn) ws $ convert action
  where
    convert (UserTyping cId pId) = WS.UserTyping s cId pId

handleTimeout :: Int -> ChatState -> WS.MMWebSocketTimeoutException -> IO ()
handleTimeout seconds st _ = reconnectAfter seconds st

handleError :: Int -> ChatState -> SomeException -> IO ()
handleError seconds st _ = reconnectAfter seconds st

reconnectAfter :: Int -> ChatState -> IO ()
reconnectAfter seconds st = do
  writeBChan (st^.csResources.crEventQueue) WebsocketDisconnect
  threadDelay (seconds * 1000 * 1000)
  writeBChan (st^.csResources.crEventQueue) RefreshWebsocketEvent
