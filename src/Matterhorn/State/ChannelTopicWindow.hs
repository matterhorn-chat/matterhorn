module Matterhorn.State.ChannelTopicWindow
  ( openChannelTopicWindow
  )
where

import           Prelude ()
import           Matterhorn.Prelude

import           Lens.Micro.Platform ( (.=) )

import           Network.Mattermost.Lenses ( teamIdL )

import           Matterhorn.Types
import           Matterhorn.State.Channels ( getCurrentChannelTopic )


openChannelTopicWindow :: MH ()
openChannelTopicWindow = do
    t <- getCurrentChannelTopic
    tId <- use (csCurrentTeam.tsTeam.teamIdL)
    csCurrentTeam.tsChannelTopicDialog .= newChannelTopicDialog tId t
    setMode ChannelTopicWindow
