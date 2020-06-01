module Events.PostListOverlay where

import           Prelude ()
import           Prelude.MH

import qualified Graphics.Vty as Vty

import           Types
import           Events.Keybindings
import           State.PostListOverlay


onEventPostListOverlay :: Vty.Event -> MH ()
onEventPostListOverlay =
  void . handleKeyboardEvent postListOverlayKeybindings (const $ return ())

-- | The keybindings we want to use while viewing a post list overlay
postListOverlayKeybindings :: KeyConfig -> [KeyHandler]
postListOverlayKeybindings = mkKeybindings postListOverlayKeyHandlers

postListOverlayKeyHandlers :: [KeyEventHandler]
postListOverlayKeyHandlers =
  [ mkKb CancelEvent "Exit post browsing" exitPostListMode
  , mkKb SelectUpEvent "Select the previous message" postListSelectUp
  , mkKb SelectDownEvent "Select the next message" postListSelectDown
  , mkKb FlagMessageEvent "Toggle the selected message flag" postListUnflagSelected
  , mkKb ActivateListItemEvent "Jump to and select current message" postListJumpToCurrent
  ]
