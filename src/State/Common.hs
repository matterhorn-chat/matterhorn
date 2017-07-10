module State.Common where

import           Prelude ()
import           Prelude.Compat

import qualified Control.Concurrent.STM as STM
import           Control.Exception (try)
import           Control.Monad.IO.Class (MonadIO, liftIO)
import qualified Data.Foldable as F
import qualified Data.HashMap.Strict as HM
import qualified Data.Map.Strict as Map
import           Data.Monoid ((<>))
import qualified Data.Sequence as Seq
import qualified Data.Set as Set
import qualified Data.Text as T
import           Data.Time.Clock (getCurrentTime)
import           Lens.Micro.Platform
import           System.Hclip (setClipboard, ClipboardException(..))

import           Network.Mattermost
import           Network.Mattermost.Lenses
import           Network.Mattermost.Exceptions

import           Types
import           Types.Channels
import           Types.Posts
import           Types.Messages

-- * Mattermost API

-- | Try to run a computation, posting an informative error
--   message if it fails with a 'MattermostServerError'.
tryMM :: IO a
      -- ^ The action to try (usually a MM API call)
      -> (a -> IO (MH ()))
      -- ^ What to do on success
      -> IO (MH ())
tryMM act onSuccess = do
    result <- liftIO $ try act
    case result of
        Left (MattermostServerError msg) -> return $ postErrorMessage msg
        Right value                      -> liftIO $ onSuccess value

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

-- | Run a computation in the background, ignoring any results
--   from it.
doAsync :: AsyncPriority -> IO () -> MH ()
doAsync prio thunk = doAsyncWith prio (thunk >> return (return ()))

-- | Run a computation in the background, returning a computation
--   to be called on the 'ChatState' value.
doAsyncWith :: AsyncPriority -> IO (MH ()) -> MH ()
doAsyncWith prio thunk = do
    let putChan = case prio of
          Preempt -> STM.unGetTChan
          Normal  -> STM.writeTChan
    queue <- use (csResources.crRequestQueue)
    liftIO $ STM.atomically $ putChan queue $ thunk

doAsyncIO :: AsyncPriority -> ChatState -> IO () -> IO ()
doAsyncIO prio st thunk =
  doAsyncWithIO prio st (thunk >> return (return ()))

-- | Run a computation in the background, returning a computation
--   to be called on the 'ChatState' value.
doAsyncWithIO :: AsyncPriority -> ChatState -> IO (MH ()) -> IO ()
doAsyncWithIO prio st thunk = do
    let putChan = case prio of
          Preempt -> STM.unGetTChan
          Normal  -> STM.writeTChan
    let queue = st^.csResources.crRequestQueue
    STM.atomically $ putChan queue $ thunk

-- | Performs an asynchronous IO operation.  On completion, the final
-- argument a completion function is executed in an MH () context in
-- the main (brick) thread.
doAsyncMM :: AsyncPriority                -- ^ the priority for this async operation
          -> (Session -> TeamId -> IO a)  -- ^ the async MM channel-based IO operation
          -> (a -> MH ())                 -- ^ function to process the results in
                                          -- brick event handling context
          -> MH ()
doAsyncMM prio mmOp thunk = do
  session <- use csSession
  myTeamId <- use (csMyTeam.teamIdL)
  doAsyncWith prio $ do
    r <- mmOp session myTeamId
    return $ thunk r

-- | Helper type for a function to perform an asynchronous MM
-- operation on a channel and then invoke an MH completion event.
type DoAsyncChannelMM a
  = AsyncPriority  -- ^ the priority for this async operation
  -> Maybe ChannelId -- ^ defaults to the "current" channel if Nothing
  -> (Session -> TeamId -> ChannelId -> IO a) -- ^ the asynchronous Mattermost
                                               -- channel-based IO operation
  -> (ChannelId -> a -> MH ()) -- ^ function to process the results in brick
                             -- event handling context
  -> MH ()

-- | Performs an asynchronous IO operation on a specific channel.  On
-- completion, the final argument a completion function is executed in
-- an MH () context in the main (brick) thread.
--
-- If no channel ID is provided on input, the current channel is used;
-- the completion function is always called with the channel ID upon
-- which the operation was performed.
doAsyncChannelMM :: DoAsyncChannelMM a
doAsyncChannelMM prio m_cId mmOp thunk = do
  ccId <- use csCurrentChannelId
  let cId = maybe ccId id m_cId
  doAsyncMM prio (\s t -> mmOp s t cId) (\r -> thunk cId r)

-- | Prefix function for calling doAsyncChannelMM that will set the
-- channel state to "pending" until the async operation completes.  If
-- the channel state is already in the pending state when this
-- function is called, no operations are performed (i.e., this request
-- is treated as a duplicate).
asPending :: DoAsyncChannelMM a -> DoAsyncChannelMM a
asPending asyncOp prio m_cId mmOp thunk = do
    ccId <- use csCurrentChannelId
    let cId = maybe ccId id m_cId
    withChannel cId $ \chan ->
        let origState = chan^.ccInfo.cdCurrentState
            (pendState, setDone) = pendingChannelState origState
        in if pendState == origState
           then return ()  -- this operation already pending; do not duplicate
           else do
             csChannel(cId).ccInfo.cdCurrentState .= pendState
             asyncOp prio m_cId mmOp $ \_ r ->
                 do csChannel(cId).ccInfo.cdCurrentState %= setDone
                    thunk cId r

-- | Helper to skip the first 3 arguments of a 4 argument function
___1 :: (a -> b -> c -> d -> e) -> d -> (a -> b -> c -> e)
___1 f a = \s t c -> f s t c a

-- | Helper to skip the first 3 arguments of a 5 argument function
___2 :: (a -> b -> c -> d -> e -> g) -> d -> e -> (a -> b -> c -> g)
___2 f a b = \s t c -> f s t c a b

-- | Helper to skip the first 3 arguments of a 5 argument function
___3 :: (a -> b -> c -> d -> e -> g -> h) -> d -> e -> g -> (a -> b -> c -> h)
___3 f a b d = \s t c -> f s t c a b d

-- | Use this convenience function if no operation needs to be
-- performed in the MH state after an async operation completes.
endAsyncNOP :: ChannelId -> a -> MH ()
endAsyncNOP _ _ = return ()

-- * Client Messages

-- | Create 'ChannelContents' from a 'Posts' value
fromPosts :: Posts -> MH ChannelContents
fromPosts ps = do
  msgs <- messagesFromPosts ps
  F.forM_ (ps^.postsPostsL) $
    asyncFetchAttachments
  return (ChannelContents msgs)

messagesFromPosts :: Posts -> MH Messages
messagesFromPosts p = do
  st <- use id
  flags <- use (csResources.crFlaggedPosts)
  csPostMap %= HM.union (postMap st)
  st' <- use id
  let msgs = postsToMessages (maybeFlag flags . clientPostToMessage st') (clientPost <$> ps)
      postsToMessages f = foldr (addMessage . f) noMessages
  return msgs
    where
        postMap :: ChatState -> HM.HashMap PostId Message
        postMap st = HM.fromList
          [ ( pId
            , clientPostToMessage st (toClientPost x Nothing)
            )
          | (pId, x) <- HM.toList (p^.postsPostsL)
          ]
        maybeFlag flagSet msg
          | Just pId <- msg^.mPostId, pId `Set.member` flagSet
            = msg & mFlagged .~ True
          | otherwise = msg
        -- n.b. postsOrder is most recent first
        ps   = findPost <$> (Seq.reverse $ postsOrder p)
        clientPost :: Post -> ClientPost
        clientPost x = toClientPost x (postId <$> parent x)
        parent x = do
            parentId <- x^.postParentIdL
            HM.lookup parentId (p^.postsPostsL)
        findPost pId = case HM.lookup pId (postsPosts p) of
            Nothing -> error $ "BUG: could not find post for post ID " <> show pId
            Just post -> post

asyncFetchAttachments :: Post -> MH ()
asyncFetchAttachments p = do
  let cId = (p^.postChannelIdL)
      pId = (p^.postIdL)
  session <- use csSession
  host    <- use (csResources.crConn.cdHostnameL)
  F.forM_ (p^.postFileIdsL) $ \fId -> doAsyncWith Normal $ do
    info <- mmGetFileInfo session fId
    let scheme = "https://"
        attUrl = scheme <> host <> urlForFile fId
        attachment = Attachment
                       { _attachmentName   = fileInfoName info
                       , _attachmentURL    = attUrl
                       , _attachmentFileId = fId
                       }
        addAttachment m
          | m^.mPostId == Just pId =
            m & mAttachments %~ (attachment Seq.<|)
          | otherwise              = m
    return $
      csChannel(cId).ccContents.cdMessages.traversed %= addAttachment

-- | Create a new 'ClientMessage' value
newClientMessage :: (MonadIO m) => ClientMessageType -> T.Text -> m ClientMessage
newClientMessage ty msg = do
  now <- liftIO getCurrentTime
  return (ClientMessage msg now ty)

-- | Add a 'ClientMessage' to the current channel's message list
addClientMessage :: ClientMessage -> MH ()
addClientMessage msg = do
  cid <- use csCurrentChannelId
  let addCMsg = ccContents.cdMessages %~ (addMessage $ clientMessageToMessage msg)
  csChannels %= modifyChannelById cid addCMsg

-- | Add a new 'ClientMessage' representing an error message to
--   the current channel's message list
postInfoMessage :: T.Text -> MH ()
postInfoMessage err = do
    msg <- newClientMessage Informative err
    doAsyncWith Normal (return $ addClientMessage msg)

-- | Add a new 'ClientMessage' representing an error message to
--   the current channel's message list
postErrorMessage :: T.Text -> MH ()
postErrorMessage err = do
    msg <- newClientMessage Error err
    doAsyncWith Normal (return $ addClientMessage msg)

postErrorMessageIO :: T.Text -> ChatState -> IO ChatState
postErrorMessageIO err st = do
  msg <- newClientMessage Error err
  let cId = st ^. csCurrentChannelId
      addEMsg = ccContents.cdMessages %~ (addMessage $ clientMessageToMessage msg)
  return $ st & csChannels %~ modifyChannelById cId addEMsg

numScrollbackPosts :: Int
numScrollbackPosts = 100

asyncFetchReactionsForPost :: ChannelId -> Post -> MH ()
asyncFetchReactionsForPost cId p
  | not (p^.postHasReactionsL) = return ()
  | otherwise = doAsyncChannelMM Normal (Just cId)
        (___1 mmGetReactionsForPost (p^.postIdL))
        addReactions

addReactions :: ChannelId -> [Reaction] -> MH ()
addReactions cId rs = csChannel(cId).ccContents.cdMessages %= fmap upd
  where upd msg = msg & mReactions %~ insertAll (msg^.mPostId)
        insert pId r
          | pId == Just (r^.reactionPostIdL) = Map.insertWith (+) (r^.reactionEmojiNameL) 1
          | otherwise = id
        insertAll pId msg = foldr (insert pId) msg rs

removeReaction :: Reaction -> ChannelId -> MH ()
removeReaction r cId = csChannel(cId).ccContents.cdMessages %= fmap upd
  where upd m | m^.mPostId == Just (r^.reactionPostIdL) =
                  m & mReactions %~ (Map.insertWith (+) (r^.reactionEmojiNameL) (-1))
              | otherwise = m

updateViewedChan :: ChannelId -> MH ()
updateViewedChan cId = do
  -- Only do this if we're connected to avoid triggering noisy exceptions.
  st <- use id
  case st^.csConnectionStatus of
      Connected -> do
          now <- liftIO getCurrentTime
          let pId = st^.csRecentChannel
          doAsyncWith Preempt $ do
            mmViewChannel
              (st^.csSession)
              (getId (st^.csMyTeam))
              cId
              pId
            return (csChannel(cId).ccInfo.cdViewed .= Just now)
      Disconnected -> return ()

copyToClipboard :: T.Text -> MH ()
copyToClipboard txt = do
  result <- liftIO (try (setClipboard (T.unpack txt)))
  case result of
    Left e -> do
      let errMsg = case e of
            UnsupportedOS _ ->
              "Matterhorn does not support yanking on this operating system."
            NoTextualData ->
              "Textual data is required to set the clipboard."
            MissingCommands cmds ->
              "Could not set clipboard due to missing one of the " <>
              "required program(s): " <> (T.pack $ show cmds)
      postErrorMessage errMsg
    Right () ->
      return ()
