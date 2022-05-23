module Matterhorn.State.Messages
  ( PostToAdd(..)
  , fetchVisibleIfNeeded
  , addMessageToState
  , sendMessage
  , jumpToPost
  , asyncFetchMessagesForGap
  )
where

import Prelude ()
import Matterhorn.Prelude
import Matterhorn.Types ( MH, AttachmentData, EditMode )
import Matterhorn.Types.Messages ( Message )
import Network.Mattermost.Types ( ChannelId, TeamId, Post, PostId )

data PostToAdd = OldPost Post | RecentPost Post Bool
data PostProcessMessageAdd

-- State.Channels needs this in setFocusWith, but that creates an
-- import cycle because several things in State.Messages need things in
-- State.Channels.
fetchVisibleIfNeeded :: TeamId -> MH ()
addMessageToState :: Bool -> Bool -> PostToAdd -> MH PostProcessMessageAdd
sendMessage :: ChannelId -> EditMode -> Text -> [AttachmentData] -> MH ()
jumpToPost :: PostId -> MH ()
asyncFetchMessagesForGap :: ChannelId -> Message -> MH ()
