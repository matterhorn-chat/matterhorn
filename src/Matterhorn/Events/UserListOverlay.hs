module Matterhorn.Events.UserListOverlay where

import           Prelude ()
import           Matterhorn.Prelude

import qualified Graphics.Vty as Vty

import           Matterhorn.Events.Keybindings
import           Matterhorn.State.UserListOverlay
import           Matterhorn.State.ListOverlay
import           Matterhorn.Types


onEventUserListOverlay :: Vty.Event -> MH ()
onEventUserListOverlay =
    void . onEventListOverlay csUserListOverlay userListOverlayKeybindings

-- | The keybindings we want to use while viewing a user list overlay
userListOverlayKeybindings :: KeyConfig -> KeyHandlerMap
userListOverlayKeybindings = mkKeybindings userListOverlayKeyHandlers

userListOverlayKeyHandlers :: [KeyEventHandler]
userListOverlayKeyHandlers =
    [ mkKb CancelEvent "Close the user search list" (exitListOverlay csUserListOverlay)
    , mkKb SearchSelectUpEvent "Select the previous user" userListSelectUp
    , mkKb SearchSelectDownEvent "Select the next user" userListSelectDown
    , mkKb PageDownEvent "Page down in the user list" userListPageDown
    , mkKb PageUpEvent "Page up in the user list" userListPageUp
    , mkKb ActivateListItemEvent "Interact with the selected user" (listOverlayActivateCurrent csUserListOverlay)
    ]
