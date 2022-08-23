module Matterhorn.Events.PostListWindow where

import           Prelude ()
import           Matterhorn.Prelude

import           Brick.Keybindings
import qualified Graphics.Vty as Vty

import           Network.Mattermost.Types ( TeamId )

import           Matterhorn.Types
import           Matterhorn.State.PostListWindow


onEventPostListWindow :: TeamId -> Vty.Event -> MH ()
onEventPostListWindow tId =
    void . mhHandleKeyboardEvent (postListWindowKeybindings tId)

-- | The keybindings we want to use while viewing a post list window
postListWindowKeybindings :: TeamId -> KeyConfig KeyEvent -> KeyDispatcher KeyEvent MH
postListWindowKeybindings tId kc = unsafeKeyDispatcher kc (postListWindowKeyHandlers tId)

postListWindowKeyHandlers :: TeamId -> [MHKeyEventHandler]
postListWindowKeyHandlers tId =
  [ onEvent CancelEvent "Exit post browsing" $ exitPostListMode tId
  , onEvent SelectUpEvent "Select the previous message" $ postListSelectUp tId
  , onEvent SelectDownEvent "Select the next message" $ postListSelectDown tId
  , onEvent FlagMessageEvent "Toggle the selected message flag" $ postListUnflagSelected tId
  , onEvent ActivateListItemEvent "Jump to and select current message" $ postListJumpToCurrent tId
  ]
