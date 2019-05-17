module Events.ChannelListOverlay
  ( onEventChannelListOverlay
  , channelListOverlayKeybindings
  )
where

import qualified Graphics.Vty as Vty

import           Events.Keybindings
import           State.ChannelListOverlay
import           State.ListOverlay
import           Types


onEventChannelListOverlay :: Vty.Event -> MH ()
onEventChannelListOverlay =
    onEventListOverlay csChannelListOverlay channelListOverlayKeybindings

-- | The keybindings we want to use while viewing a channel list overlay
channelListOverlayKeybindings :: KeyConfig -> [Keybinding]
channelListOverlayKeybindings = mkKeybindings
    [ mkKb CancelEvent "Close the channel search list" (exitListOverlay csChannelListOverlay)
    , mkKb SearchSelectUpEvent "Select the previous channel" channelListSelectUp
    , mkKb SearchSelectDownEvent "Select the next channel" channelListSelectDown
    , mkKb PageDownEvent "Page down in the channel list" channelListPageDown
    , mkKb PageUpEvent "Page up in the channel list" channelListPageUp
    , mkKb ActivateListItemEvent "Join the selected channel" (listOverlayActivateCurrent csChannelListOverlay)
    ]
