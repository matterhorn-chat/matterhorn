module State.Common where

import           Prelude ()
import           Prelude.Compat

import qualified Control.Concurrent.STM as STM
import           Control.Exception (try)
import           Control.Monad.IO.Class (MonadIO, liftIO)
import qualified Data.Foldable as F
import qualified Data.HashMap.Strict as HM
import           Data.List (sort)
import qualified Data.Map.Strict as Map
import           Data.Monoid ((<>))
import qualified Data.Sequence as Seq
import qualified Data.Text as T
import           Data.Time.Clock (getCurrentTime)
import           Lens.Micro.Platform
import           System.Hclip (setClipboard, ClipboardException(..))

import           Network.Mattermost
import           Network.Mattermost.Lenses
import           Network.Mattermost.Exceptions

import           Types
import           Types.Posts
import           Types.Messages

-- * MatterMost API

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

-- * Client Messages

-- | Create 'ChannelContents' from a 'Posts' value
fromPosts :: Posts -> MH ChannelContents
fromPosts ps = do
  msgs <- messagesFromPosts ps
  F.forM_ (ps^.postsPostsL) $
    asyncFetchAttachments
  return (ChannelContents msgs)

getDMChannelName :: UserId -> UserId -> T.Text
getDMChannelName me you = cname
  where
  [loUser, hiUser] = sort $ idString <$> [ you, me ]
  cname = loUser <> "__" <> hiUser

messagesFromPosts :: Posts -> MH Messages
messagesFromPosts p = do -- (msgs, st')
  st <- use id
  csPostMap %= HM.union (postMap st)
  st' <- use id
  let msgs = postsToMessages (clientPostToMessage st') (clientPost <$> ps)
      postsToMessages f = foldr (addMessage . f) noMessages
  return msgs
    where
        postMap :: ChatState -> HM.HashMap PostId Message
        postMap st = HM.fromList [ ( pId
                                , clientPostToMessage st (toClientPost x Nothing)
                                )
                              | (pId, x) <- HM.toList (p^.postsPostsL)
                              ]
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
  msgMap.ix cid.ccContents.cdMessages %= (addMessage $ clientMessageToMessage msg)

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
  now <- liftIO getCurrentTime
  let msg = ClientMessage err now Error
      cId = st ^. csCurrentChannelId
  return $ st & msgMap.ix cId.ccContents.cdMessages %~ (addMessage $ clientMessageToMessage msg)

numScrollbackPosts :: Int
numScrollbackPosts = 100

-- | Fetch scrollback for a channel in the background
asyncFetchScrollback :: AsyncPriority -> ChannelId -> MH ()
asyncFetchScrollback prio cId = do
    session  <- use csSession
    myTeamId <- use (csMyTeam.teamIdL)
    Just viewTime <- preuse (msgMap.ix cId.ccInfo.cdViewed)
    doAsyncWith prio $ do
        posts <- mmGetPosts session myTeamId cId 0 numScrollbackPosts
        return $ do
            mapM_ (asyncFetchReactionsForPost cId) (posts^.postsPostsL)
            contents <- fromPosts posts
            -- We need to set the new message cutoff only if there are
            -- actually messages that came in after our last view time.
            let setCutoff = if hasNew
                            then const $ Just $ minimum (_mDate <$> newMessages)
                            else id
                hasNew = not $ null newMessages
                newMessages = case viewTime of
                    Nothing -> mempty
                    Just vt -> messagesAfter vt $ contents^.cdMessages
            csChannel(cId).ccContents .= contents
            csChannel(cId).ccInfo.cdCurrentState .= ChanLoaded
            csChannel(cId).ccInfo.cdNewMessageCutoff %= setCutoff

asyncFetchReactionsForPost :: ChannelId -> Post -> MH ()
asyncFetchReactionsForPost cId p
  | not (p^.postHasReactionsL) = return ()
  | otherwise = do
      session <- use csSession
      myTeamId <- use (csMyTeam.teamIdL)
      doAsyncWith Normal $ do
        reactions <- mmGetReactionsForPost session myTeamId cId (p^.postIdL)
        return $ do
          let insert r = Map.insertWith (+) (r^.reactionEmojiNameL) 1
              insertAll m = foldr insert m reactions
              upd m | m^.mPostId == Just (p^.postIdL) =
                        m & mReactions %~ insertAll
                    | otherwise = m
          csChannel(cId).ccContents.cdMessages %= fmap upd

addReaction :: Reaction -> ChannelId -> MH ()
addReaction r cId = csChannel(cId).ccContents.cdMessages %= fmap upd
  where upd m | m^.mPostId == Just (r^.reactionPostIdL) =
                  m & mReactions %~ (Map.insertWith (+) (r^.reactionEmojiNameL) 1)
              | otherwise = m

removeReaction :: Reaction -> ChannelId -> MH ()
removeReaction r cId = csChannel(cId).ccContents.cdMessages %= fmap upd
  where upd m | m^.mPostId == Just (r^.reactionPostIdL) =
                  m & mReactions %~ (Map.insertWith (+) (r^.reactionEmojiNameL) (-1))
              | otherwise = m

updateViewedIO :: ChatState -> IO ChatState
updateViewedIO st = do
  -- Only do this if we're connected to avoid triggering noisy exceptions.
  case st^.csConnectionStatus of
      Connected -> do
          now <- getCurrentTime
          let cId = st^.csCurrentChannelId
              pId = st^.csRecentChannel
          doAsyncWithIO Preempt st $ do
            mmViewChannel
              (st^.csSession)
              (getId (st^.csMyTeam))
              cId
              pId
            return (csChannel(cId).ccInfo.cdViewed .= Just now)
          return st
      Disconnected -> return st

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
