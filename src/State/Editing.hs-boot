module State.Editing
  ( Direction(..)
  , tabComplete
  )
where

import Types ( MH )

data Direction = Forwards | Backwards
tabComplete :: Direction -> MH ()
