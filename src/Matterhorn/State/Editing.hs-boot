module Matterhorn.State.Editing
  ( Direction(..)
  , tabComplete
  )
where

import Network.Mattermost.Types ( TeamId )
import Matterhorn.Types ( MH )

data Direction = Forwards | Backwards
tabComplete :: TeamId -> Direction -> MH ()
