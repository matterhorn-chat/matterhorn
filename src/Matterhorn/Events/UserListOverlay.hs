module Matterhorn.Events.UserListOverlay where

import           Prelude ()
import           Matterhorn.Prelude

import qualified Graphics.Vty as Vty

import           Network.Mattermost.Types ( TeamId )

import           Matterhorn.Events.Keybindings
import           Matterhorn.State.UserListOverlay
import           Matterhorn.State.ListOverlay
import           Matterhorn.Types


onEventUserListOverlay :: TeamId -> Vty.Event -> MH ()
onEventUserListOverlay tId =
    void . onEventListOverlay (csTeam(tId).tsUserListOverlay) (userListOverlayKeybindings tId)

-- | The keybindings we want to use while viewing a user list overlay
userListOverlayKeybindings :: TeamId -> KeyConfig -> KeyHandlerMap
userListOverlayKeybindings tId = mkKeybindings (userListOverlayKeyHandlers tId)

userListOverlayKeyHandlers :: TeamId -> [KeyEventHandler]
userListOverlayKeyHandlers tId =
    [ mkKb CancelEvent "Close the user search list" (exitListOverlay (csTeam(tId).tsUserListOverlay))
    , mkKb SearchSelectUpEvent "Select the previous user" $ userListSelectUp tId
    , mkKb SearchSelectDownEvent "Select the next user" $ userListSelectDown tId
    , mkKb PageDownEvent "Page down in the user list" $ userListPageDown tId
    , mkKb PageUpEvent "Page up in the user list" $ userListPageUp tId
    , mkKb ActivateListItemEvent "Interact with the selected user" (listOverlayActivateCurrent (csTeam(tId).tsUserListOverlay))
    ]
