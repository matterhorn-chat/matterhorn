module Events.UserListOverlay where

import qualified Graphics.Vty as Vty

import Types
import Events.Keybindings
import State.UserListOverlay

onEventUserListOverlay :: Vty.Event -> MH ()
onEventUserListOverlay =
  handleKeyboardEvent userListOverlayKeybindings $ \ _ -> return ()

-- | The keybindings we want to use while viewing a user list overlay
userListOverlayKeybindings :: KeyConfig -> [Keybinding]
userListOverlayKeybindings = mkKeybindings
  [ mkKb CancelEvent "Exit user browsing" exitUserListMode
  , mkKb SelectUpEvent "Select the previous user" userListSelectUp
  , mkKb SelectDownEvent "Select the next next" userListSelectDown
  ]
