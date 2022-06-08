module Matterhorn.State.ThreadWindow
  ( closeThreadWindow
  , openThreadWindow
  )
where

import Prelude ()

import Network.Mattermost.Types (TeamId, PostId, ChannelId)

import Matterhorn.Types

openThreadWindow :: TeamId -> ChannelId -> PostId -> MH ()
closeThreadWindow :: TeamId -> MH ()
