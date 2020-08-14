module Matterhorn.State.Async
  ( AsyncPriority(..)
  , doAsync
  , doAsyncIO
  , doAsyncWith
  , doAsyncChannelMM
  , doAsyncWithIO
  , doAsyncMM
  , tryMM
  , endAsyncNOP
  )
where

import           Prelude ()
import           Matterhorn.Prelude

import qualified Control.Concurrent.STM as STM
import           Control.Exception ( try )

import           Network.Mattermost.Types

import           Matterhorn.Types


-- | Try to run a computation, posting an informative error
--   message if it fails with a 'MattermostServerError'.
tryMM :: IO a
      -- ^ The action to try (usually a MM API call)
      -> (a -> IO (Maybe (MH ())))
      -- ^ What to do on success
      -> IO (Maybe (MH ()))
tryMM act onSuccess = do
    result <- liftIO $ try act
    case result of
        Left e -> return $ Just $ mhError $ ServerError e
        Right value -> liftIO $ onSuccess value

-- * Background Computation

-- $background_computation
--
-- The main context for Matterhorn is the EventM context provided by
-- the 'Brick' library.  This context is normally waiting for user
-- input (or terminal resizing, etc.) which gets turned into an
-- MHEvent and the 'onEvent' event handler is called to process that
-- event, after which the display is redrawn as necessary and brick
-- awaits the next input.
--
-- However, it is often convenient to communicate with the Mattermost
-- server in the background, so that large numbers of
-- synchronously-blocking events (e.g. on startup) or refreshes can
-- occur whenever needed and without negatively impacting the UI
-- updates or responsiveness.  This is handled by a 'forkIO' context
-- that waits on an STM channel for work to do, performs the work, and
-- then sends brick an MHEvent containing the completion or failure
-- information for that work.
--
-- The /doAsyncWith/ family of functions here facilitates that
-- asynchronous functionality.  This is typically used in the
-- following fashion:
--
-- > doSomething :: MH ()
-- > doSomething = do
-- >    got <- something
-- >    doAsyncWith Normal $ do
-- >       r <- mmFetchR ....
-- >       return $ do
-- >          csSomething.here %= processed r
--
-- The second argument is an IO monad operation (because 'forkIO' runs
-- in the IO Monad context), but it returns an MH monad operation.
-- The IO monad has access to the closure of 'doSomething' (e.g. the
-- 'got' value), but it should be aware that the state of the MH monad
-- may have been changed by the time the IO monad runs in the
-- background, so the closure is a snapshot of information at the time
-- the 'doAsyncWith' was called.
--
-- Similarly, the returned MH monad operation is *not* run in the
-- context of the 'forkIO' background, but it is instead passed via an
-- MHEvent back to the main brick thread, where it is executed in an
-- EventM handler's MH monad context.  This operation therefore has
-- access to the combined closure of the pre- 'doAsyncWith' code and
-- the closure of the IO operation.  It is important that the final MH
-- monad operation should *re-obtain* state information from the MH
-- monad instead of using or setting the state obtained prior to the
-- 'doAsyncWith' call.

-- | Priority setting for asynchronous work items. Preempt means that
-- the queued item will be the next work item begun (i.e. it goes to the
-- front of the queue); normal means it will go last in the queue.
data AsyncPriority = Preempt | Normal

-- | Run a computation in the background, ignoring any results from it.
doAsync :: AsyncPriority -> IO () -> MH ()
doAsync prio act = doAsyncWith prio (act >> return Nothing)

-- | Run a computation in the background, returning a computation to be
-- called on the 'ChatState' value.
doAsyncWith :: AsyncPriority -> IO (Maybe (MH ())) -> MH ()
doAsyncWith prio act = do
    let putChan = case prio of
          Preempt -> STM.unGetTChan
          Normal  -> STM.writeTChan
    queue <- use (csResources.crRequestQueue)
    liftIO $ STM.atomically $ putChan queue act

doAsyncIO :: AsyncPriority -> ChatState -> IO () -> IO ()
doAsyncIO prio st act =
  doAsyncWithIO prio st (act >> return Nothing)

-- | Run a computation in the background, returning a computation to be
-- called on the 'ChatState' value.
doAsyncWithIO :: AsyncPriority -> ChatState -> IO (Maybe (MH ())) -> IO ()
doAsyncWithIO prio st act = do
    let putChan = case prio of
          Preempt -> STM.unGetTChan
          Normal  -> STM.writeTChan
    let queue = st^.csResources.crRequestQueue
    STM.atomically $ putChan queue act

-- | Performs an asynchronous IO operation. On completion, the final
-- argument a completion function is executed in an MH () context in the
-- main (brick) thread.
doAsyncMM :: AsyncPriority
          -- ^ the priority for this async operation
          -> (Session -> IO a)
          -- ^ the async MM channel-based IO operation
          -> (a -> Maybe (MH ()))
          -- ^ function to process the results in brick event handling
          -- context
          -> MH ()
doAsyncMM prio mmOp eventHandler = do
  session <- getSession
  doAsyncWith prio $ do
    r <- mmOp session
    return $ eventHandler r

-- | Helper type for a function to perform an asynchronous MM operation
-- on a channel and then invoke an MH completion event.
type DoAsyncChannelMM a =
    AsyncPriority
    -- ^ the priority for this async operation
    -> ChannelId
    -- ^ The channel
    -> (Session -> ChannelId -> IO a)
    -- ^ the asynchronous Mattermost channel-based IO operation
    -> (ChannelId -> a -> Maybe (MH ()))
    -- ^ function to process the results in brick event handling context
    -> MH ()

-- | Performs an asynchronous IO operation on a specific channel. On
-- completion, the final argument a completion function is executed in
-- an MH () context in the main (brick) thread.
doAsyncChannelMM :: DoAsyncChannelMM a
doAsyncChannelMM prio cId mmOp eventHandler =
  doAsyncMM prio (\s -> mmOp s cId) (eventHandler cId)

-- | Use this convenience function if no operation needs to be
-- performed in the MH state after an async operation completes.
endAsyncNOP :: ChannelId -> a -> Maybe (MH ())
endAsyncNOP _ _ = Nothing
