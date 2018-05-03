module Connection where

import           Prelude ()
import           Prelude.MH

import           Brick.BChan
import           Control.Concurrent ( forkIO, threadDelay )
import qualified Control.Concurrent.STM as STM
import           Control.Exception ( SomeException, catch )
import qualified Data.HashMap.Strict as HM
import           Data.Int (Int64)
import           Data.Semigroup ( Max(..) )
import qualified Data.Text as T
import           Data.Time ( UTCTime(..), secondsToDiffTime, getCurrentTime
                           , diffUTCTime )
import           Data.Time.Calendar ( Day(..) )

import           Network.Mattermost.Types ( ChannelId )
import qualified Network.Mattermost.WebSocket as WS

import           Constants
import           Types


connectWebsockets :: MH ()
connectWebsockets = do
  st <- use id
  session <- getSession
  logger <- mhGetIOLogger
  liftIO $ do
    let shunt (Left msg) = writeBChan (st^.csResources.crEventQueue) (WebsocketParseError msg)
        shunt (Right e) = writeBChan (st^.csResources.crEventQueue) (WSEvent e)
        runWS = WS.mmWithWebSocket session shunt $ \ws -> do
                  writeBChan (st^.csResources.crEventQueue) WebsocketConnect
                  processWebsocketActions st ws 1 HM.empty
    void $ forkIO $ runWS `catch` handleTimeout logger 1 st
                          `catch` handleError logger 5 st

-- | Take websocket actions from the websocket action channel in the ChatState and
-- | send them to the server over the websocket.
-- | Takes and propagates the action sequence number which in incremented for
-- | each successful send.
-- | Keeps and propagates a map of channel id to last user_typing notification send time
-- | so that the new user_typing actions are throttled to be send only once in two seconds.
processWebsocketActions :: ChatState -> WS.MMWebSocket -> Int64 -> HashMap ChannelId (Max UTCTime) -> IO ()
processWebsocketActions st ws s userTypingLastNotifTimeMap = do
  action <- STM.atomically $ STM.readTChan (st^.csResources.crWebsocketActionChan)
  if (shouldSendAction action)
    then do
      WS.mmSendWSAction (st^.csResources.crConn) ws $ convert action
      now <- getCurrentTime
      processWebsocketActions st ws (s + 1) $ userTypingLastNotifTimeMap' action now
    else do
      processWebsocketActions st ws s userTypingLastNotifTimeMap
  where
    convert (UserTyping _ cId pId) = WS.UserTyping s cId pId

    shouldSendAction (UserTyping ts cId _) =
      diffUTCTime ts (userTypingLastNotifTime cId) >= (userTypingExpiryInterval / 2 - 0.5)

    userTypingLastNotifTime cId = getMax $ HM.lookupDefault (Max zeroTime) cId userTypingLastNotifTimeMap

    zeroTime = UTCTime (ModifiedJulianDay 0) (secondsToDiffTime 0)

    userTypingLastNotifTimeMap' (UserTyping _ cId _) now =
      HM.insertWith (<>) cId (Max now) userTypingLastNotifTimeMap

handleTimeout :: (LogCategory -> Text -> IO ()) -> Int -> ChatState -> WS.MMWebSocketTimeoutException -> IO ()
handleTimeout logger seconds st e = do
    logger LogWebsocket $ T.pack $ "Websocket timeout exception: " <> show e
    reconnectAfter seconds st

handleError :: (LogCategory -> Text -> IO ()) -> Int -> ChatState -> SomeException -> IO ()
handleError logger seconds st e = do
    logger LogWebsocket $ T.pack $ "Websocket error: " <> show e
    reconnectAfter seconds st

reconnectAfter :: Int -> ChatState -> IO ()
reconnectAfter seconds st = do
  writeBChan (st^.csResources.crEventQueue) WebsocketDisconnect
  threadDelay (seconds * 1000 * 1000)
  writeBChan (st^.csResources.crEventQueue) RefreshWebsocketEvent
