module Matterhorn.Events.ChannelListOverlay
  ( onEventChannelListOverlay
  , channelListOverlayKeybindings
  , channelListOverlayKeyHandlers
  )
where

import           Prelude ()
import           Matterhorn.Prelude

import qualified Graphics.Vty as Vty

import           Matterhorn.Events.Keybindings
import           Matterhorn.State.ChannelListOverlay
import           Matterhorn.State.ListOverlay
import           Matterhorn.Types


onEventChannelListOverlay :: Vty.Event -> MH ()
onEventChannelListOverlay =
    void . onEventListOverlay (csCurrentTeam.tsChannelListOverlay) channelListOverlayKeybindings

-- | The keybindings we want to use while viewing a channel list overlay
channelListOverlayKeybindings :: KeyConfig -> KeyHandlerMap
channelListOverlayKeybindings = mkKeybindings channelListOverlayKeyHandlers

channelListOverlayKeyHandlers :: [KeyEventHandler]
channelListOverlayKeyHandlers =
    [ mkKb CancelEvent "Close the channel search list" (exitListOverlay (csCurrentTeam.tsChannelListOverlay))
    , mkKb SearchSelectUpEvent "Select the previous channel" channelListSelectUp
    , mkKb SearchSelectDownEvent "Select the next channel" channelListSelectDown
    , mkKb PageDownEvent "Page down in the channel list" channelListPageDown
    , mkKb PageUpEvent "Page up in the channel list" channelListPageUp
    , mkKb ActivateListItemEvent "Join the selected channel" (listOverlayActivateCurrent (csCurrentTeam.tsChannelListOverlay))
    ]
