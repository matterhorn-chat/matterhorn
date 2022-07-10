module Matterhorn.Events.ChannelListWindow
  ( onEventChannelListWindow
  , channelListWindowKeybindings
  , channelListWindowKeyHandlers
  )
where

import           Prelude ()
import           Matterhorn.Prelude

import           Brick.Keybindings
import qualified Graphics.Vty as Vty

import           Network.Mattermost.Types ( TeamId )

import           Matterhorn.State.ChannelListWindow
import           Matterhorn.State.ListWindow
import           Matterhorn.Types


onEventChannelListWindow :: TeamId -> Vty.Event -> MH ()
onEventChannelListWindow tId =
    void . onEventListWindow (csTeam(tId).tsChannelListWindow) (channelListWindowKeybindings tId)

-- | The keybindings we want to use while viewing a channel list window
channelListWindowKeybindings :: TeamId -> KeyConfig KeyEvent -> KeyHandlerMap KeyEvent MH
channelListWindowKeybindings tId = mkKeybindings (channelListWindowKeyHandlers tId)

channelListWindowKeyHandlers :: TeamId -> [MHKeyEventHandler]
channelListWindowKeyHandlers tId =
    [ onEvent CancelEvent "Close the channel search list" (exitListWindow tId (csTeam(tId).tsChannelListWindow))
    , onEvent SearchSelectUpEvent "Select the previous channel" $ channelListSelectUp tId
    , onEvent SearchSelectDownEvent "Select the next channel" $ channelListSelectDown tId
    , onEvent PageDownEvent "Page down in the channel list" $ channelListPageDown tId
    , onEvent PageUpEvent "Page up in the channel list" $ channelListPageUp tId
    , onEvent ActivateListItemEvent "Join the selected channel" (listWindowActivateCurrent tId (csTeam(tId).tsChannelListWindow))
    ]
