module Matterhorn.Events.UserListWindow where

import           Prelude ()
import           Matterhorn.Prelude

import qualified Graphics.Vty as Vty

import           Network.Mattermost.Types ( TeamId )

import           Matterhorn.State.UserListWindow
import           Matterhorn.State.ListWindow
import           Matterhorn.Types
import           Matterhorn.Types.KeyEvents


onEventUserListWindow :: TeamId -> Vty.Event -> MH ()
onEventUserListWindow tId =
    void . onEventListWindow (csTeam(tId).tsUserListWindow) (userListWindowKeybindings tId)

-- | The keybindings we want to use while viewing a user list window
userListWindowKeybindings :: TeamId -> KeyConfig KeyEvent -> KeyHandlerMap KeyEvent MH
userListWindowKeybindings tId = mkKeybindings (userListWindowKeyHandlers tId)

userListWindowKeyHandlers :: TeamId -> [KeyEventHandler KeyEvent MH]
userListWindowKeyHandlers tId =
    [ mkKb CancelEvent "Close the user search list" (exitListWindow tId (csTeam(tId).tsUserListWindow))
    , mkKb SearchSelectUpEvent "Select the previous user" $ userListSelectUp tId
    , mkKb SearchSelectDownEvent "Select the next user" $ userListSelectDown tId
    , mkKb PageDownEvent "Page down in the user list" $ userListPageDown tId
    , mkKb PageUpEvent "Page up in the user list" $ userListPageUp tId
    , mkKb ActivateListItemEvent "Interact with the selected user" (listWindowActivateCurrent tId (csTeam(tId).tsUserListWindow))
    ]
