module Matterhorn.State.Messages
  ( fetchVisibleIfNeeded
  )
where

import Matterhorn.Types ( MH )
import Network.Mattermost.Types ( TeamId )

-- State.Channels needs this in setFocusWith, but that creates an
-- import cycle because several things in State.Messages need things in
-- State.Channels.
fetchVisibleIfNeeded :: TeamId -> MH ()
