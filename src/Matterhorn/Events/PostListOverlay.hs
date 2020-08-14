module Matterhorn.Events.PostListOverlay where

import           Prelude ()
import           Matterhorn.Prelude

import qualified Graphics.Vty as Vty

import           Matterhorn.Types
import           Matterhorn.Events.Keybindings
import           Matterhorn.State.PostListOverlay


onEventPostListOverlay :: Vty.Event -> MH ()
onEventPostListOverlay =
  void . handleKeyboardEvent postListOverlayKeybindings (const $ return ())

-- | The keybindings we want to use while viewing a post list overlay
postListOverlayKeybindings :: KeyConfig -> KeyHandlerMap
postListOverlayKeybindings = mkKeybindings postListOverlayKeyHandlers

postListOverlayKeyHandlers :: [KeyEventHandler]
postListOverlayKeyHandlers =
  [ mkKb CancelEvent "Exit post browsing" exitPostListMode
  , mkKb SelectUpEvent "Select the previous message" postListSelectUp
  , mkKb SelectDownEvent "Select the next message" postListSelectDown
  , mkKb FlagMessageEvent "Toggle the selected message flag" postListUnflagSelected
  , mkKb ActivateListItemEvent "Jump to and select current message" postListJumpToCurrent
  ]
