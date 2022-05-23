module Matterhorn.State.ThreadWindow
  ( closeThreadWindow
  )
where

import Prelude ()

import Network.Mattermost.Types (TeamId)

import Matterhorn.Types

closeThreadWindow :: TeamId -> MH ()
