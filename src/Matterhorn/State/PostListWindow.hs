module Matterhorn.State.PostListWindow
  ( enterFlaggedPostListMode
  , enterPinnedPostListMode
  , enterSearchResultPostListMode
  , postListJumpToCurrent
  , postListSelectUp
  , postListSelectDown
  , postListUnflagSelected
  , exitPostListMode
  )
where

import           GHC.Exts ( IsList(..) )
import           Prelude ()
import           Matterhorn.Prelude

import qualified Data.Foldable as F
import qualified Data.Text as T
import           Lens.Micro.Platform ( (.=) )
import           Network.Mattermost.Endpoints
import           Network.Mattermost.Types

import           Matterhorn.State.Messages ( jumpToPost )
import           Matterhorn.State.Common
import           Matterhorn.State.MessageSelect
import           Matterhorn.State.Messages ( addObtainedMessages
                                           , asyncFetchMessagesSurrounding )
import           Matterhorn.Types
import           Matterhorn.Types.DirectionalSeq (emptyDirSeq)


-- | Create a PostListWindow with the given content description and
-- with a specified list of messages.
enterPostListMode :: TeamId -> PostListContents -> Messages -> MH ()
enterPostListMode tId contents msgs = do
  csTeam(tId).tsPostListWindow.postListPosts .= msgs
  let mlatest = getLatestPostMsg msgs
      pId = mlatest >>= messagePostId
      cId = mlatest >>= \m -> m^.mChannelId
  csTeam(tId).tsPostListWindow.postListSelected .= pId
  pushMode tId $ PostListWindow contents
  case (pId, cId) of
    (Just p, Just c) -> asyncFetchMessagesSurrounding c p
    _ -> return ()

-- | Clear out the state of a PostListWindow
exitPostListMode :: TeamId -> MH ()
exitPostListMode tId = do
  csTeam(tId).tsPostListWindow.postListPosts .= emptyDirSeq
  csTeam(tId).tsPostListWindow.postListSelected .= Nothing
  popMode tId

createPostList :: TeamId -> PostListContents -> (Session -> IO Posts) -> MH ()
createPostList tId contentsType fetchOp = do
  session <- getSession
  doAsyncWith Preempt $ do
    posts <- fetchOp session
    return $ Just $ Work "createPostList" $ do
      messages <- installMessagesFromPosts (Just tId) posts
      -- n.b. do not use addNewPostedMessage because these messages
      -- are not new, and so no notifications or channel highlighting
      -- or other post-processing should be performed.
      let plist = F.toList $ postsPosts posts
          postsSpec p = Posts { postsPosts = fromList [(postId p, p)]
                              , postsOrder = fromList [postId p]
                              }
      mapM_ (\p -> addObtainedMessages (postChannelId p) 0 False $ postsSpec p) plist
      enterPostListMode tId contentsType messages


-- | Create a PostListWindow with flagged messages from the server.
enterFlaggedPostListMode :: TeamId -> MH ()
enterFlaggedPostListMode tId = do
    createPostList tId PostListFlagged $
        mmGetListOfFlaggedPosts UserMe defaultFlaggedPostsQuery

-- | Create a PostListWindow with pinned messages from the server for
-- the current channel.
enterPinnedPostListMode :: TeamId -> MH ()
enterPinnedPostListMode tId =
    withCurrentChannel tId $ \cId _ -> do
        createPostList tId (PostListPinned cId) $ mmGetChannelPinnedPosts cId

-- | Create a PostListWindow with post search result messages from the
-- server.
enterSearchResultPostListMode :: TeamId -> Text -> MH ()
enterSearchResultPostListMode tId terms
  | T.null (T.strip terms) = postInfoMessage "Search command requires at least one search term."
  | otherwise = do
      enterPostListMode tId (PostListSearch terms True) noMessages
      createPostList tId (PostListSearch terms False) $
        mmSearchForTeamPosts tId (SearchPosts terms False)


-- | Move the selection up in the PostListWindow, which corresponds
-- to finding a chronologically /newer/ message.
postListSelectDown :: TeamId -> MH ()
postListSelectDown tId = do
  selId <- use (csTeam(tId).tsPostListWindow.postListSelected)
  posts <- use (csTeam(tId).tsPostListWindow.postListPosts)
  let nextMsg = getNextMessage (MessagePostId <$> selId) posts
  case nextMsg of
    Nothing -> return ()
    Just m -> do
      let pId = m^.mMessageId >>= messageIdPostId
      csTeam(tId).tsPostListWindow.postListSelected .= pId
      case (m^.mChannelId, pId) of
        (Just c, Just p) -> asyncFetchMessagesSurrounding c p
        o -> mhLog LogError
             (T.pack $ "postListSelectDown" <>
              " unable to get channel or post ID: " <> show o)

-- | Move the selection down in the PostListWindow, which corresponds
-- to finding a chronologically /old/ message.
postListSelectUp :: TeamId -> MH ()
postListSelectUp tId = do
  selId <- use (csTeam(tId).tsPostListWindow.postListSelected)
  posts <- use (csTeam(tId).tsPostListWindow.postListPosts)
  let prevMsg = getPrevMessage (MessagePostId <$> selId) posts
  case prevMsg of
    Nothing -> return ()
    Just m -> do
      let pId = m^.mMessageId >>= messageIdPostId
      csTeam(tId).tsPostListWindow.postListSelected .= pId
      case (m^.mChannelId, pId) of
        (Just c, Just p) -> asyncFetchMessagesSurrounding c p
        o -> mhLog LogError
             (T.pack $ "postListSelectUp" <>
              " unable to get channel or post ID: " <> show o)

-- | Unflag the post currently selected in the PostListWindow, if any
postListUnflagSelected :: TeamId -> MH ()
postListUnflagSelected tId = do
  msgId <- use (csTeam(tId).tsPostListWindow.postListSelected)
  case msgId of
    Nothing  -> return ()
    Just pId -> flagMessage pId False


-- | Jumps to the specified message in the message's main channel
-- display and changes to MessageSelectState.
postListJumpToCurrent :: TeamId -> MH ()
postListJumpToCurrent tId = do
  msgId <- use (csTeam(tId).tsPostListWindow.postListSelected)
  case msgId of
    Nothing  -> return ()
    Just pId -> jumpToPost pId
