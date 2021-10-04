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
    tId <- use csCurrentTeamId
    t <- getCurrentChannelTopic tId
    csTeam(tId).tsChannelTopicDialog .= newChannelTopicDialog tId t
    setMode tId ChannelTopicWindow
