module State.PostListOverlay where

import Lens.Micro.Platform
import Network.Mattermost
import Network.Mattermost.Lenses

import State.Common
import Types
import Types.Messages

enterPostListMode ::  PostListContents -> Messages -> MH ()
enterPostListMode contents msgs = do
  csPostListOverlay.postListPosts .= msgs
  csPostListOverlay.postListSelected .= getLatestPostId msgs
  csMode .= PostListOverlay contents

enterFlaggedPostListMode :: MH ()
enterFlaggedPostListMode = do
  session <- use csSession
  uId <- use (csMe.userIdL)
  doAsyncWith Normal $ do
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
