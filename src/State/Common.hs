module State.Common where

import           Brick (EventM)
import qualified Control.Concurrent.Chan as Chan
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

import           Network.Mattermost
import           Network.Mattermost.Lenses
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

-- | Create 'ChannelContents' from a 'Posts' value
fromPosts :: ChatState -> Posts -> IO (ChannelContents, ChatState)
fromPosts st ps = do
  let (msgs, st') = messagesFromPosts st ps
  F.forM_ (ps^.postsPostsL) $ \p ->
    asyncFetchAttachments p st
  return (ChannelContents msgs, st')

getDMChannelName :: UserId -> UserId -> T.Text
getDMChannelName me you = cname
  where
  [loUser, hiUser] = sort [ you, me ]
  cname = idString loUser <> "__" <> idString hiUser

messagesFromPosts :: ChatState -> Posts -> (Seq.Seq Message, ChatState)
messagesFromPosts st p = (msgs, st')
    where
        postMap :: HM.HashMap PostId Message
        postMap = HM.fromList [ ( pId
                                , clientPostToMessage st (toClientPost x Nothing)
                                )
                              | (pId, x) <- HM.toList (p^.postsPostsL)
                              ]
        st' = st & csPostMap %~ (HM.union postMap)
        msgs = clientPostToMessage st' <$> clientPost <$> ps
        ps   = findPost <$> (Seq.reverse $ postsOrder p)
        clientPost :: Post -> ClientPost
        clientPost x = toClientPost x (postId <$> parent x)
        parent x = do
            parentId <- x^.postParentIdL
            HM.lookup parentId (p^.postsPostsL)
        findPost pId = case HM.lookup pId (postsPosts p) of
            Nothing -> error $ "BUG: could not find post for post ID " <> show pId
            Just post -> post

asyncFetchAttachments :: Post -> ChatState -> IO ()
asyncFetchAttachments p st = do
  let cId = p^.postChannelIdL
      pId = p^.postIdL
  F.forM_ (p^.postFileIdsL) $ \fId -> doAsyncWith st $ do
    info <- mmGetFileInfo (st^.csConn) (st^.csTok) fId
    let scheme = "https://"
        host = st^.csResources.crConn.cdHostnameL
        attUrl = scheme <> host <> urlForFile fId
        attachment = Attachment
                       { _attachmentName = fileInfoName info
                       , _attachmentURL  = attUrl
                       }
        addAttachment m
          | m^.mPostId == Just pId =
            m & mAttachments %~ (attachment Seq.<|)
          | otherwise              = m
    return $ \st' -> do
      return (st' & csChannel(cId).ccContents.cdMessages.each %~ addAttachment)

-- | Create a new 'ClientMessage' value
newClientMessage :: (MonadIO m) => ClientMessageType -> T.Text -> m ClientMessage
newClientMessage ty msg = do
  now <- liftIO getCurrentTime
  return (ClientMessage msg now ty)

-- | Add a 'ClientMessage' to the current channel's message list
addClientMessage :: ClientMessage -> ChatState -> ChatState
addClientMessage msg st =
  let cid = st^.csCurrentChannelId
  in st & msgMap . ix cid . ccContents . cdMessages %~ (Seq.|> clientMessageToMessage msg)

-- | Add a new 'ClientMessage' representing an error message to
--   the current channel's message list
postErrorMessage :: (MonadIO m) => T.Text -> ChatState -> m ChatState
postErrorMessage err st = do
    msg <- newClientMessage Error err
    liftIO $ doAsyncWith st (return $ return . addClientMessage msg)
    return st

numScrollbackPosts :: Int
numScrollbackPosts = 100

-- | Fetch scrollback for a channel in the background
asyncFetchScrollback :: ChatState -> ChannelId -> IO ()
asyncFetchScrollback st cId =
    doAsyncWith st $ do
        posts <- mmGetPosts (st^.csConn) (st^.csTok) (st^.csMyTeam.teamIdL) cId 0 numScrollbackPosts
        return $ \st' -> do
            liftIO $ mapM_ (asyncFetchReactionsForPost st cId) (posts^.postsPostsL)
            (contents, st'') <- liftIO $ fromPosts st' posts
                -- We need to set the new message cutoff only if there
                -- are actually messages that came in after our last
                -- view time.
            let Just viewTime = st'^?msgMap.ix cId.ccInfo.cdViewed
                cutoff = if hasNew then Just viewTime else Nothing
                hasNew = not $ Seq.null $
                         Seq.filter (\m -> m^.mDate > viewTime) $
                         contents^.cdMessages
            return $
                st'' & csChannel(cId).ccContents .~ contents
                     & csChannel(cId).ccInfo.cdCurrentState .~ ChanLoaded
                     & csChannel(cId).ccInfo.cdNewMessageCutoff .~ cutoff


asyncFetchReactionsForPost :: ChatState -> ChannelId -> Post -> IO ()
asyncFetchReactionsForPost st cId p
  | not (p^.postHasReactionsL) = return ()
  | otherwise = doAsyncWith st $ do
      reactions <- mmGetReactionsForPost
                     (st^.csConn) (st^.csTok) (st^.csMyTeam.teamIdL)
                                              cId
                                              (p^.postIdL)
      return $ \st' -> do
        let insert r = Map.insertWith (+) (r^.reactionEmojiNameL) 1
            insertAll m = foldr insert m reactions
            upd m | m^.mPostId == Just (p^.postIdL) =
                      m & mReactions %~ insertAll
                  | otherwise = m
        return $ st' & csChannel(cId).ccContents.cdMessages %~ fmap upd

addReaction :: ChatState -> Reaction -> ChannelId -> ChatState
addReaction st r cId = st & csChannel(cId).ccContents.cdMessages %~ fmap upd
  where upd m | m^.mPostId == Just (r^.reactionPostIdL) =
                  m & mReactions %~ (Map.insertWith (+) (r^.reactionEmojiNameL) 1)
              | otherwise = m

removeReaction :: ChatState -> Reaction -> ChannelId -> ChatState
removeReaction st r cId = st & csChannel(cId).ccContents.cdMessages %~ fmap upd
  where upd m | m^.mPostId == Just (r^.reactionPostIdL) =
                  m & mReactions %~ (Map.insertWith (+) (r^.reactionEmojiNameL) (-1))
              | otherwise = m

updateViewedIO :: ChatState -> IO ChatState
updateViewedIO st = do
  now <- getCurrentTime
  let cId = st^.csCurrentChannelId
  doAsyncWith st $ do
    mmUpdateLastViewedAt
      (st^.csConn)
      (st^.csTok)
      (getId (st^.csMyTeam))
      cId
    return (\s -> return (s & csCurrentChannel.ccInfo.cdViewed .~ now))
  return st
