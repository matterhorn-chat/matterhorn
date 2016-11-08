module State.Common where

import           Brick (EventM)
import           Brick.Main (viewportScroll, vScrollToEnd)
import qualified Control.Concurrent.Chan as Chan
import           Control.Exception (try)
import           Control.Monad.IO.Class (MonadIO, liftIO)
import qualified Data.Sequence as Seq
import qualified Data.Text as T
import           Data.Time.Clock (getCurrentTime)
import           Lens.Micro.Platform

import           Network.Mattermost.Exceptions

import           Types

-- * MatterMost API

-- | Try to run a computation, posting an informative error
--   message if it fails with a 'MattermostServerError'.
tryMM :: (MonadIO m)
      => IO a
      -- ^ The action to try (usually a MM API call)
      -> (a -> IO (ChatState -> m ChatState))
      -- ^ What to do on success
      -> IO (ChatState -> m ChatState)
tryMM act onSuccess = do
    result <- liftIO $ try act
    case result of
        Left (MattermostServerError msg) -> return $ postErrorMessage msg
        Right value                      -> liftIO $ onSuccess value

-- * Background Computation

-- | Run a computation in the background, ignoring any results
--   from it.
doAsync :: ChatState -> IO () -> IO ()
doAsync st thunk = doAsyncWith st (thunk >> return return)

-- | Run a computation in the background, returning a computation
--   to be called on the 'ChatState' value.
doAsyncWith :: ChatState -> IO (ChatState -> EventM Name ChatState) -> IO ()
doAsyncWith st thunk =
  Chan.writeChan (st^.csRequestQueue) thunk

-- * Client Messages

-- | Create a new 'ClientMessage' value
newClientMessage :: (MonadIO m) => ClientMessageType -> T.Text -> m ClientMessage
newClientMessage ty msg = do
  now <- liftIO getCurrentTime
  return (ClientMessage msg now ty)

-- | Add a 'ClientMessage' to the current channel's message list
addClientMessage :: ClientMessage -> ChatState -> EventM Name ChatState
addClientMessage msg st = do
  let cid = st^.csCurrentChannelId
      st' = st & msgMap . ix cid . ccContents . cdMessages %~ (Seq.|> clientMessageToMessage msg)
  updateChannelScrollState st'

-- | Add a new 'ClientMessage' representing an error message to
--   the current channel's message list
postErrorMessage :: (MonadIO m) => T.Text -> ChatState -> m ChatState
postErrorMessage err st = do
    msg <- newClientMessage Error err
    liftIO $ doAsyncWith st (return $ addClientMessage msg)
    return st

-- | Scroll to the end of the current message list
updateChannelScrollState :: ChatState -> EventM Name ChatState
updateChannelScrollState st = do
  let cId = st^.csCurrentChannelId
  vScrollToEnd $ viewportScroll (ChannelMessages cId)
  return st
