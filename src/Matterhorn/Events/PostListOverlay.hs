module Matterhorn.Events.PostListOverlay where

import           Prelude ()
import           Matterhorn.Prelude

import qualified Graphics.Vty as Vty

import           Network.Mattermost.Types ( TeamId )

import           Matterhorn.Types
import           Matterhorn.Events.Keybindings
import           Matterhorn.State.PostListOverlay


onEventPostListOverlay :: TeamId -> Vty.Event -> MH ()
onEventPostListOverlay tId =
    void . handleKeyboardEvent (postListOverlayKeybindings tId)

-- | The keybindings we want to use while viewing a post list overlay
postListOverlayKeybindings :: TeamId -> KeyConfig -> KeyHandlerMap
postListOverlayKeybindings tId = mkKeybindings (postListOverlayKeyHandlers tId)

postListOverlayKeyHandlers :: TeamId -> [KeyEventHandler]
postListOverlayKeyHandlers tId =
  [ mkKb CancelEvent "Exit post browsing" $ exitPostListMode tId
  , mkKb SelectUpEvent "Select the previous message" $ postListSelectUp tId
  , mkKb SelectDownEvent "Select the next message" $ postListSelectDown tId
  , mkKb FlagMessageEvent "Toggle the selected message flag" $ postListUnflagSelected tId
  , mkKb ActivateListItemEvent "Jump to and select current message" $ postListJumpToCurrent tId
  ]
