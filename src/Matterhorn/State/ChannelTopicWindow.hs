module Matterhorn.State.ChannelTopicWindow
  ( openChannelTopicWindow
  )
where

import           Lens.Micro.Platform ( (.=) )

import           Matterhorn.Types
import           Matterhorn.State.Channels ( getCurrentChannelTopic )


openChannelTopicWindow :: MH ()
openChannelTopicWindow = do
    t <- getCurrentChannelTopic
    csChannelTopicDialog .= newChannelTopicDialog t
    setMode ChannelTopicWindow
