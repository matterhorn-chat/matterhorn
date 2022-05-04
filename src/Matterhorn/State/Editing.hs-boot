{-# LANGUAGE RankNTypes #-}
module Matterhorn.State.Editing
  ( Direction(..)
  , tabComplete
  )
where

import Network.Mattermost.Types ( TeamId )
import Matterhorn.Types ( MH, ChatState, EditState )
import Lens.Micro.Platform ( Lens' )

data Direction = Forwards | Backwards
tabComplete :: TeamId -> Lens' ChatState EditState -> Direction -> MH ()
