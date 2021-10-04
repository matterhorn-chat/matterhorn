module Matterhorn.Events.ChannelListOverlay
  ( onEventChannelListOverlay
  , channelListOverlayKeybindings
  , channelListOverlayKeyHandlers
  )
where

import           Prelude ()
import           Matterhorn.Prelude

import qualified Graphics.Vty as Vty

import           Network.Mattermost.Types ( TeamId )

import           Matterhorn.Events.Keybindings
import           Matterhorn.State.ChannelListOverlay
import           Matterhorn.State.ListOverlay
import           Matterhorn.Types


onEventChannelListOverlay :: TeamId -> Vty.Event -> MH ()
onEventChannelListOverlay tId =
    void . onEventListOverlay (csTeam(tId).tsChannelListOverlay) (channelListOverlayKeybindings tId)

-- | The keybindings we want to use while viewing a channel list overlay
channelListOverlayKeybindings :: TeamId -> KeyConfig -> KeyHandlerMap
channelListOverlayKeybindings tId = mkKeybindings (channelListOverlayKeyHandlers tId)

channelListOverlayKeyHandlers :: TeamId -> [KeyEventHandler]
channelListOverlayKeyHandlers tId =
    [ mkKb CancelEvent "Close the channel search list" (exitListOverlay (csTeam(tId).tsChannelListOverlay))
    , mkKb SearchSelectUpEvent "Select the previous channel" $ channelListSelectUp tId
    , mkKb SearchSelectDownEvent "Select the next channel" $ channelListSelectDown tId
    , mkKb PageDownEvent "Page down in the channel list" $ channelListPageDown tId
    , mkKb PageUpEvent "Page up in the channel list" $ channelListPageUp tId
    , mkKb ActivateListItemEvent "Join the selected channel" (listOverlayActivateCurrent (csTeam(tId).tsChannelListOverlay))
    ]
