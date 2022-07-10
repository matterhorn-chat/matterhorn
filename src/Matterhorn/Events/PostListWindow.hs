module Matterhorn.Events.PostListWindow where

import           Prelude ()
import           Matterhorn.Prelude

import qualified Graphics.Vty as Vty

import           Network.Mattermost.Types ( TeamId )

import           Matterhorn.Types
import           Matterhorn.Types.KeyEvents
import           Matterhorn.State.PostListWindow


onEventPostListWindow :: TeamId -> Vty.Event -> MH ()
onEventPostListWindow tId =
    void . mhHandleKeyboardEvent (postListWindowKeybindings tId)

-- | The keybindings we want to use while viewing a post list window
postListWindowKeybindings :: TeamId -> KeyConfig KeyEvent -> KeyHandlerMap KeyEvent MH
postListWindowKeybindings tId = mkKeybindings (postListWindowKeyHandlers tId)

postListWindowKeyHandlers :: TeamId -> [MHKeyEventHandler]
postListWindowKeyHandlers tId =
  [ mkKb CancelEvent "Exit post browsing" $ exitPostListMode tId
  , mkKb SelectUpEvent "Select the previous message" $ postListSelectUp tId
  , mkKb SelectDownEvent "Select the next message" $ postListSelectDown tId
  , mkKb FlagMessageEvent "Toggle the selected message flag" $ postListUnflagSelected tId
  , mkKb ActivateListItemEvent "Jump to and select current message" $ postListJumpToCurrent tId
  ]
