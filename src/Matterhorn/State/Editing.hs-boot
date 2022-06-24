{-# LANGUAGE RankNTypes #-}
module Matterhorn.State.Editing
  ( Direction(..)
  , tabComplete
  )
where

import Matterhorn.Types ( MH, ChatState, EditState, Name )
import Lens.Micro.Platform ( Traversal' )

data Direction = Forwards | Backwards
tabComplete :: Traversal' ChatState (EditState Name) -> Direction -> MH ()
