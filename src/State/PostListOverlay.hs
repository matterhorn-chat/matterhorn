module State.PostListOverlay
  ( enterFlaggedPostListMode
  , enterSearchResultPostListMode
  , postListSelectUp
  , postListSelectDown
  , postListUnflagSelected
  , exitPostListMode
  )
where

import           Prelude ()
import           Prelude.MH

import qualified Data.Text as T
import           Lens.Micro.Platform ( (.=) )
import           Network.Mattermost.Endpoints
import           Network.Mattermost.Types

import           State.Common
import           State.MessageSelect
import           Types
import           Types.DirectionalSeq (emptyDirSeq)


-- | Create a PostListOverlay with the given content description and
-- with a specified list of messages.
enterPostListMode ::  PostListContents -> Messages -> MH ()
enterPostListMode contents msgs = do
  csPostListOverlay.postListPosts .= msgs
  csPostListOverlay.postListSelected .= (getLatestPostMsg msgs >>= messagePostId)
  setMode $ PostListOverlay contents

-- | Clear out the state of a PostListOverlay
exitPostListMode :: MH ()
exitPostListMode = do
  csPostListOverlay.postListPosts .= emptyDirSeq
  csPostListOverlay.postListSelected .= Nothing
  setMode Main

-- | Create a PostListOverlay with flagged messages from the server.
enterFlaggedPostListMode :: MH ()
enterFlaggedPostListMode = do
  session <- getSession
  doAsyncWith Preempt $ do
    posts <- mmGetListOfFlaggedPosts UserMe defaultFlaggedPostsQuery session
    return $ Just $ do
      messages <- fst <$> installMessagesFromPosts posts
      enterPostListMode PostListFlagged messages

-- | Create a PostListOverlay with post search result messages from the
-- server.
enterSearchResultPostListMode :: Text -> MH ()
enterSearchResultPostListMode terms = do
  session <- getSession
  tId <- gets myTeamId
  case T.null $ T.strip terms of
      True -> postInfoMessage "Search command requires at least one search term."
      False -> do
        enterPostListMode (PostListSearch terms True) noMessages
        doAsyncWith Preempt $ do
          posts <- mmSearchForTeamPosts tId (SearchPosts terms False) session
          return $ Just $ do
            messages <- fst <$> installMessagesFromPosts posts
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
