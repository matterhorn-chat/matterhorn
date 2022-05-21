{-# LANGUAGE RankNTypes #-}
module Matterhorn.State.Editing
  ( Direction(..)
  , tabComplete
  )
where

import Matterhorn.Types ( MH, ChatState, EditState, Name )
import Lens.Micro.Platform ( Lens' )

data Direction = Forwards | Backwards
tabComplete :: Lens' ChatState (EditState Name) -> Direction -> MH ()
