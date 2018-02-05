module Events.UserListOverlay where

import qualified Graphics.Vty as Vty

import Brick.Widgets.Edit (handleEditorEvent)

import Types
import Events.Keybindings
import State.UserListOverlay

onEventUserListOverlay :: Vty.Event -> MH ()
onEventUserListOverlay =
  handleKeyboardEvent userListOverlayKeybindings $
      mhHandleEventLensed (csUserListOverlay.userListSearchInput) handleEditorEvent

-- | The keybindings we want to use while viewing a user list overlay
userListOverlayKeybindings :: KeyConfig -> [Keybinding]
userListOverlayKeybindings = mkKeybindings
  [ mkKb CancelEvent "Exit user browsing" exitUserListMode
  , mkKb SelectUpEvent "Select the previous user" userListSelectUp
  , mkKb SelectDownEvent "Select the next user" userListSelectDown
  , mkKb PageDownEvent "Page down in the user list" userListPageDown
  , mkKb PageUpEvent "Page up in the user list" userListPageUp
  ]
