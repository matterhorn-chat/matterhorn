module Matterhorn.State.PostListWindow
  ( enterFlaggedPostListMode
  , enterPinnedPostListMode
  , enterSearchResultPostListMode
  , postListJumpToCurrent
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
import           Matterhorn.State.MessageListing ( withListingSelectedMessage
                                                 , exitMessageSelect
                                                 )
import           Matterhorn.State.Messages ( addObtainedMessages
                                           , flagPost
                                           )
import           Matterhorn.Types


-- | Create a PostListWindow with the given content description and
-- with a specified list of messages.
enterPostListMode :: TeamId -> PostListContents -> Messages -> MH ()
enterPostListMode tId contents msgs = do
  let mlatest = getLatestPostMsg msgs
      mId = mlatest >>= _mMessageId

  csTeam(tId).tsPostListWindow.mlMessages .= msgs
  csTeam(tId).tsPostListWindow.mlMessageSelect .= MessageSelectState mId
  csTeam(tId).tsPostListWindow.mlMode .= MessageSelect

  pushMode tId $ PostListWindow contents

-- | Clear out the state of a PostListWindow
exitPostListMode :: TeamId -> MH ()
exitPostListMode tId = popMode tId

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
      createPostList tId (PostListSearch terms False) $
        mmSearchForTeamPosts tId (SearchPosts terms False)

-- | Unflag the post currently selected in the PostListWindow, if any
postListUnflagSelected :: TeamId -> MH ()
postListUnflagSelected tId =
    withListingSelectedMessage (csTeam(tId).tsPostListWindow) $ \msg -> do
        msgs <- use (csTeam(tId).tsPostListWindow.mlMessages)
        case postIdForMessageId msgs =<< (msg^.mMessageId) of
            Nothing -> return ()
            Just pId -> flagPost pId False

-- | Jumps to the specified message in the message's main channel
-- display and changes to MessageSelectState.
postListJumpToCurrent :: TeamId -> MH ()
postListJumpToCurrent tId =
    withListingSelectedMessage (csTeam(tId).tsPostListWindow) $ \msg -> do
        msgs <- use (csTeam(tId).tsPostListWindow.mlMessages)
        case postIdForMessageId msgs =<< (msg^.mMessageId) of
            Nothing -> return ()
            Just pId -> do
                exitMessageSelect (csTeam(tId).tsPostListWindow)
                exitPostListMode tId
                jumpToPost pId

postIdForMessageId :: Messages -> MessageId -> Maybe PostId
postIdForMessageId msgs mId = findMessage mId msgs >>= messagePostId
