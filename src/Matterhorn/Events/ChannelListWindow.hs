module Matterhorn.Events.ChannelListWindow
  ( onEventChannelListWindow
  , channelListWindowKeybindings
  , channelListWindowKeyHandlers
  )
where

import           Prelude ()
import           Matterhorn.Prelude

import qualified Graphics.Vty as Vty

import           Network.Mattermost.Types ( TeamId )

import           Matterhorn.Events.Keybindings
import           Matterhorn.State.ChannelListWindow
import           Matterhorn.State.ListWindow
import           Matterhorn.Types


onEventChannelListWindow :: TeamId -> Vty.Event -> MH ()
onEventChannelListWindow tId =
    void . onEventListWindow (csTeam(tId).tsChannelListWindow) (channelListWindowKeybindings tId)

-- | The keybindings we want to use while viewing a channel list window
channelListWindowKeybindings :: TeamId -> KeyConfig KeyEvent -> KeyHandlerMap
channelListWindowKeybindings tId = mkKeybindings (channelListWindowKeyHandlers tId)

channelListWindowKeyHandlers :: TeamId -> [KeyEventHandler]
channelListWindowKeyHandlers tId =
    [ mkKb CancelEvent "Close the channel search list" (exitListWindow tId (csTeam(tId).tsChannelListWindow))
    , mkKb SearchSelectUpEvent "Select the previous channel" $ channelListSelectUp tId
    , mkKb SearchSelectDownEvent "Select the next channel" $ channelListSelectDown tId
    , mkKb PageDownEvent "Page down in the channel list" $ channelListPageDown tId
    , mkKb PageUpEvent "Page up in the channel list" $ channelListPageUp tId
    , mkKb ActivateListItemEvent "Join the selected channel" (listWindowActivateCurrent tId (csTeam(tId).tsChannelListWindow))
    ]
