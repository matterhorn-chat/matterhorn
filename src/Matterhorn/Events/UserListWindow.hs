module Matterhorn.Events.UserListWindow where

import           Prelude ()
import           Matterhorn.Prelude

import           Brick.Keybindings
import qualified Graphics.Vty as Vty

import           Network.Mattermost.Types ( TeamId )

import           Matterhorn.State.UserListWindow
import           Matterhorn.State.ListWindow
import           Matterhorn.Types


onEventUserListWindow :: TeamId -> Vty.Event -> MH ()
onEventUserListWindow tId =
    void . onEventListWindow (csTeam(tId).tsUserListWindow) (userListWindowKeybindings tId)

-- | The keybindings we want to use while viewing a user list window
userListWindowKeybindings :: TeamId -> KeyConfig KeyEvent -> KeyDispatcher KeyEvent MH
userListWindowKeybindings tId kc = unsafeKeyDispatcher kc (userListWindowKeyHandlers tId)

userListWindowKeyHandlers :: TeamId -> [MHKeyEventHandler]
userListWindowKeyHandlers tId =
    [ onEvent CancelEvent "Close the user search list" (exitListWindow tId (csTeam(tId).tsUserListWindow))
    , onEvent SearchSelectUpEvent "Select the previous user" $ userListSelectUp tId
    , onEvent SearchSelectDownEvent "Select the next user" $ userListSelectDown tId
    , onEvent PageDownEvent "Page down in the user list" $ userListPageDown tId
    , onEvent PageUpEvent "Page up in the user list" $ userListPageUp tId
    , onEvent ActivateListItemEvent "Interact with the selected user" (listWindowActivateCurrent tId (csTeam(tId).tsUserListWindow))
    ]
