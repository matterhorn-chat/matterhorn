module Matterhorn.State.Messages
  ( PostToAdd(..)
  , fetchVisibleIfNeeded
  , addMessageToState
  )
where

import Prelude ()
import Matterhorn.Prelude
import Matterhorn.Types ( MH )
import Network.Mattermost.Types ( TeamId, Post )

data PostToAdd = OldPost Post | RecentPost Post Bool
data PostProcessMessageAdd

-- State.Channels needs this in setFocusWith, but that creates an
-- import cycle because several things in State.Messages need things in
-- State.Channels.
fetchVisibleIfNeeded :: TeamId -> MH ()
addMessageToState :: Bool -> Bool -> PostToAdd -> MH PostProcessMessageAdd
