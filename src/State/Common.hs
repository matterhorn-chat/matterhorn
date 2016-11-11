module State.Common where

import           Brick (EventM)
import qualified Control.Concurrent.Chan as Chan
import           Control.Exception (try)
import           Control.Monad.IO.Class (MonadIO, liftIO)
import qualified Data.HashMap.Strict as HM
import           Data.List (sort)
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
fromPosts :: ChatState -> Posts -> ChannelContents
fromPosts st p = ChannelContents $ messagesFromPosts st p

getDMChannelName :: UserId -> UserId -> T.Text
getDMChannelName me you = cname
  where
  [loUser, hiUser] = sort [ you, me ]
  cname = idString loUser <> "__" <> idString hiUser

messagesFromPosts :: ChatState -> Posts -> Seq.Seq Message
messagesFromPosts st p = msgs
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
  return st'

-- | Add a new 'ClientMessage' representing an error message to
--   the current channel's message list
postErrorMessage :: (MonadIO m) => T.Text -> ChatState -> m ChatState
postErrorMessage err st = do
    msg <- newClientMessage Error err
    liftIO $ doAsyncWith st (return $ addClientMessage msg)
    return st

numScrollbackPosts :: Int
numScrollbackPosts = 100

-- | Fetch scrollback for a channel in the background
asyncFetchScrollback :: ChatState -> ChannelId -> IO ()
asyncFetchScrollback st cId =
    doAsyncWith st $ do
        posts <- mmGetPosts (st^.csConn) (st^.csTok) (st^.csMyTeam.teamIdL) cId 0 numScrollbackPosts
        return $ \st' -> do
            let contents = fromPosts st' posts
                -- We need to set the new message cutoff only if there
                -- are actually messages that came in after our last
                -- view time.
                Just viewTime = st'^?msgMap.ix cId.ccInfo.cdViewed
                cutoff = if hasNew then Just viewTime else Nothing
                hasNew = not $ Seq.null $
                         Seq.filter (\m -> m^.mDate > viewTime) $
                         contents^.cdMessages
            return $
                st' & csChannel(cId).ccContents .~ contents
                    & csChannel(cId).ccInfo.cdCurrentState .~ ChanLoaded
                    & csChannel(cId).ccInfo.cdNewMessageCutoff .~ cutoff

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
