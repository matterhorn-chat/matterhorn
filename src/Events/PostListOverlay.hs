module Events.PostListOverlay where

import qualified Graphics.Vty as Vty

import Types
import Events.Keybindings
import State.PostListOverlay

onEventPostListOverlay :: Vty.Event -> MH ()
onEventPostListOverlay =
  handleKeyboardEvent postListOverlayKeybindings $ \ _ -> return ()

-- | The keybindings we want to use while viewing a post list overlay
postListOverlayKeybindings :: KeyConfig -> [Keybinding]
postListOverlayKeybindings = mkKeybindings
  [ mkKb CancelEvent "Exit post browsing" exitPostListMode
  , mkKb SelectUpEvent "Select the previous message" postListSelectUp
  , mkKb SelectDownEvent "Select the next message" postListSelectDown
  , mkKb FlagMessageEvent "Toggle the selected message flag" postListUnflagSelected
  ]
