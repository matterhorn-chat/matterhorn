module State.Messages
  ( fetchVisibleIfNeeded
  )
where

import Types ( MH )

-- State.Channels needs this in setFocusWith, but that creates an
-- import cycle because several things in State.Messages need things in
-- State.Channels.
fetchVisibleIfNeeded :: MH ()
