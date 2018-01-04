module State.PostListOverlay where

import Data.Text (Text)
import Lens.Micro.Platform
import Network.Mattermost.Endpoints
import Network.Mattermost.Types

import State
import State.Common
import Types
import Types.Messages

-- | Create a PostListOverlay with the given content description and
-- with a specified list of messages.
enterPostListMode ::  PostListContents -> Messages -> MH ()
enterPostListMode contents msgs = do
  csPostListOverlay.postListPosts .= msgs
  csPostListOverlay.postListSelected .= getLatestPostId msgs
  csMode .= PostListOverlay contents

-- | Clear out the state of a PostListOverlay
exitPostListMode :: MH ()
exitPostListMode = do
  csPostListOverlay.postListPosts .= mempty
  csPostListOverlay.postListSelected .= Nothing
  csMode .= Main

-- | Create a PostListOverlay with flagged messages from the server.
enterFlaggedPostListMode :: MH ()
enterFlaggedPostListMode = do
  session <- use (csResources.crSession)
  doAsyncWith Preempt $ do
    posts <- mmGetListOfFlaggedPosts UserMe defaultFlaggedPostsQuery session
    return $ do
      messages <- messagesFromPosts posts
      enterPostListMode PostListFlagged messages

-- | Create a PostListOverlay with post search result messages from the
-- server.
enterSearchResultPostListMode :: Text -> MH ()
enterSearchResultPostListMode terms = do
  session <- use (csResources.crSession)
  tId <- teamId <$> use csMyTeam
  enterPostListMode (PostListSearch terms True) noMessages
  doAsyncWith Preempt $ do
    posts <- mmSearchForTeamPosts tId (SearchPosts terms False) session
    return $ do
      messages <- messagesFromPosts posts
      enterPostListMode (PostListSearch terms False) messages

-- | Move the selection up in the PostListOverlay, which corresponds
-- to finding a chronologically /newer/ message.
postListSelectUp :: MH ()
postListSelectUp = do
  msgId <- use (csPostListOverlay.postListSelected)
  posts <- use (csPostListOverlay.postListPosts)
  let nextId = (getNextPostId msgId posts)
  case nextId of
    Nothing -> return ()
    Just _ ->
      csPostListOverlay.postListSelected .= nextId

-- | Move the selection down in the PostListOverlay, which corresponds
-- to finding a chronologically /old/ message.
postListSelectDown :: MH ()
postListSelectDown = do
  msgId <- use (csPostListOverlay.postListSelected)
  posts <- use (csPostListOverlay.postListPosts)
  let prevId = (getPrevPostId msgId posts)
  case prevId of
    Nothing -> return ()
    Just _ ->
      csPostListOverlay.postListSelected .= prevId

-- | Unflag the post currently selected in the PostListOverlay, if any
postListUnflagSelected :: MH ()
postListUnflagSelected = do
  msgId <- use (csPostListOverlay.postListSelected)
  case msgId of
    Nothing  -> return ()
    Just pId -> flagMessage pId False
