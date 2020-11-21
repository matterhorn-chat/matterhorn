module Matterhorn.State.ChannelTopicWindow
  ( openChannelTopicWindow
  )
where

import           Prelude ()
import           Matterhorn.Prelude

import           Lens.Micro.Platform ( (.=) )

import           Matterhorn.Types
import           Matterhorn.State.Channels ( getCurrentChannelTopic )


openChannelTopicWindow :: MH ()
openChannelTopicWindow = do
    t <- getCurrentChannelTopic
    csCurrentTeam.tsChannelTopicDialog .= newChannelTopicDialog t
    setMode ChannelTopicWindow
