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
    tId <- use csCurrentTeamId
    csTeam(tId).tsChannelTopicDialog .= newChannelTopicDialog tId t
    setMode tId ChannelTopicWindow
