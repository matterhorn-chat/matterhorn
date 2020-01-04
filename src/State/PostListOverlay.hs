module State.PostListOverlay
  ( enterFlaggedPostListMode
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
import           Prelude.MH

import qualified Data.Foldable as F
import qualified Data.Text as T
import           Lens.Micro.Platform ( (.=) )
import           Network.Mattermost.Endpoints
import           Network.Mattermost.Types

import           State.Channels
import           State.Common
import           State.MessageSelect
import           State.Messages ( addObtainedMessages
                                , asyncFetchMessagesSurrounding )
import           Types
import           Types.DirectionalSeq (emptyDirSeq)


-- | Create a PostListOverlay with the given content description and
-- with a specified list of messages.
enterPostListMode ::  PostListContents -> Messages -> MH ()
enterPostListMode contents msgs = do
  csPostListOverlay.postListPosts .= msgs
  let mlatest = getLatestPostMsg msgs
      pId = mlatest >>= messagePostId
      cId = mlatest >>= \m -> m^.mChannelId
  csPostListOverlay.postListSelected .= pId
  setMode $ PostListOverlay contents
  case (pId, cId) of
    (Just p, Just c) -> asyncFetchMessagesSurrounding c p
    _ -> return ()

-- | Clear out the state of a PostListOverlay
exitPostListMode :: MH ()
exitPostListMode = do
  csPostListOverlay.postListPosts .= emptyDirSeq
  csPostListOverlay.postListSelected .= Nothing
  setMode Main


createPostList :: PostListContents -> (Session -> IO Posts) -> MH ()
createPostList contentsType fetchOp = do
  session <- getSession
  doAsyncWith Preempt $ do
    posts <- fetchOp session
    return $ Just $ do
      messages <- installMessagesFromPosts posts
      -- n.b. do not use addNewPostedMessage because these messages
      -- are not new, and so no notifications or channel highlighting
      -- or other post-processing should be performed.
      let plist = F.toList $ postsPosts posts
          postsSpec p = Posts { postsPosts = fromList [(postId p, p)]
                              , postsOrder = fromList [postId p]
                              }
      mapM_ (\p -> addObtainedMessages (postChannelId p) 0 False $ postsSpec p) plist
      enterPostListMode contentsType messages


-- | Create a PostListOverlay with flagged messages from the server.
enterFlaggedPostListMode :: MH ()
enterFlaggedPostListMode = createPostList PostListFlagged $
                           mmGetListOfFlaggedPosts UserMe defaultFlaggedPostsQuery


-- | Create a PostListOverlay with post search result messages from the
-- server.
enterSearchResultPostListMode :: Text -> MH ()
enterSearchResultPostListMode terms
  | T.null (T.strip terms) = postInfoMessage "Search command requires at least one search term."
  | otherwise = do
      enterPostListMode (PostListSearch terms True) noMessages
      tId <- gets focusedTeamId
      createPostList (PostListSearch terms False) $
        mmSearchForTeamPosts tId (SearchPosts terms False)


-- | Move the selection up in the PostListOverlay, which corresponds
-- to finding a chronologically /newer/ message.
postListSelectUp :: MH ()
postListSelectUp = do
  selId <- use (csPostListOverlay.postListSelected)
  posts <- use (csPostListOverlay.postListPosts)
  let nextMsg = getNextMessage (MessagePostId <$> selId) posts
  case nextMsg of
    Nothing -> return ()
    Just m -> do
      let pId = m^.mMessageId >>= messageIdPostId
      csPostListOverlay.postListSelected .= pId
      case (m^.mChannelId, pId) of
        (Just c, Just p) -> asyncFetchMessagesSurrounding c p
        o -> mhLog LogError
             (T.pack $ "postListSelectUp" <>
              " unable to get channel or post ID: " <> show o)

-- | Move the selection down in the PostListOverlay, which corresponds
-- to finding a chronologically /old/ message.
postListSelectDown :: MH ()
postListSelectDown = do
  selId <- use (csPostListOverlay.postListSelected)
  posts <- use (csPostListOverlay.postListPosts)
  let prevMsg = getPrevMessage (MessagePostId <$> selId) posts
  case prevMsg of
    Nothing -> return ()
    Just m -> do
      let pId = m^.mMessageId >>= messageIdPostId
      csPostListOverlay.postListSelected .= pId
      case (m^.mChannelId, pId) of
        (Just c, Just p) -> asyncFetchMessagesSurrounding c p
        o -> mhLog LogError
             (T.pack $ "postListSelectDown" <>
              " unable to get channel or post ID: " <> show o)

-- | Unflag the post currently selected in the PostListOverlay, if any
postListUnflagSelected :: MH ()
postListUnflagSelected = do
  msgId <- use (csPostListOverlay.postListSelected)
  case msgId of
    Nothing  -> return ()
    Just pId -> flagMessage pId False


-- | Jumps to the specified message in the message's main channel
-- display and changes to MessageSelectState.
postListJumpToCurrent :: MH ()
postListJumpToCurrent = do
  msgId <- use (csPostListOverlay.postListSelected)
  st <- use id
  case msgId of
    Nothing  -> return ()
    Just pId ->
      case getMessageForPostId st pId of
        Just msg ->
          case msg ^. mChannelId of
            Just cId -> do
              setFocus cId
              setMode MessageSelect
              csMessageSelect .= MessageSelectState (msg^.mMessageId)
            Nothing ->
              error "INTERNAL: selected Post ID not associated with a channel"
        Nothing ->
          error "INTERNAL: selected Post ID not added to global state!"
