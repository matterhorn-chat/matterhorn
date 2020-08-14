module Matterhorn.State.Editing
  ( Direction(..)
  , tabComplete
  )
where

import Matterhorn.Types ( MH )

data Direction = Forwards | Backwards
tabComplete :: Direction -> MH ()
