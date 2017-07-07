module State.PostListOverlay where

import Lens.Micro.Platform
import Network.Mattermost
import Network.Mattermost.Lenses

import State
import State.Common
import Types
import Types.Messages

enterPostListMode ::  PostListContents -> Messages -> MH ()
enterPostListMode contents msgs = do
  csPostListOverlay.postListPosts .= msgs
  csPostListOverlay.postListSelected .= getLatestPostId msgs
  csMode .= PostListOverlay contents

exitPostListMode :: MH ()
exitPostListMode = do
  csPostListOverlay.postListPosts .= mempty
  csPostListOverlay.postListSelected .= Nothing
  csMode .= Main

enterFlaggedPostListMode :: MH ()
enterFlaggedPostListMode = do
  session <- use csSession
  uId <- use (csMe.userIdL)
  doAsyncWith Preempt $ do
    posts <- mmGetFlaggedPosts session uId
    return $ do
      messages <- messagesFromPosts posts
      enterPostListMode PostListFlagged messages

postListSelectUp :: MH ()
postListSelectUp = do
  msgId <- use (csPostListOverlay.postListSelected)
  posts <- use (csPostListOverlay.postListPosts)
  let nextId = (getNextPostId msgId posts)
  case nextId of
    Nothing -> return ()
    Just _ ->
      csPostListOverlay.postListSelected .= nextId

postListSelectDown :: MH ()
postListSelectDown = do
  msgId <- use (csPostListOverlay.postListSelected)
  posts <- use (csPostListOverlay.postListPosts)
  let prevId = (getPrevPostId msgId posts)
  case prevId of
    Nothing -> return ()
    Just _ ->
      csPostListOverlay.postListSelected .= prevId

postListUnflagSelected :: MH ()
postListUnflagSelected = do
  msgId <- use (csPostListOverlay.postListSelected)
  case msgId of
    Nothing  -> return ()
    Just pId -> flagMessage pId False
