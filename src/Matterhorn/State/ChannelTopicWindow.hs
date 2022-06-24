module Matterhorn.State.ChannelTopicWindow
  ( openChannelTopicWindow
  )
where

import           Prelude ()
import           Matterhorn.Prelude

import           Lens.Micro.Platform ( (.=) )

import           Network.Mattermost.Types ( TeamId )

import           Matterhorn.Types
import           Matterhorn.State.Teams ( newChannelTopicDialog )
import           Matterhorn.State.Channels ( getCurrentChannelTopic )


openChannelTopicWindow :: TeamId -> MH ()
openChannelTopicWindow tId = do
    t <- getCurrentChannelTopic tId
    case t of
        Nothing -> return ()
        Just topic -> do
            csTeam(tId).tsChannelTopicDialog .= newChannelTopicDialog tId topic
            pushMode tId ChannelTopicWindow
