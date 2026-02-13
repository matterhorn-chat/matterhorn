module Matterhorn.Events.PostListWindow
  ( onEventPostListWindow
  , postListWindowKeyHandlers
  , postListWindowKeybindings
  )
where

import           Prelude ()
import           Matterhorn.Prelude

import           Brick.Keybindings
import qualified Graphics.Vty as Vty

import           Network.Mattermost.Types ( TeamId )

import           Matterhorn.Types
import           Matterhorn.State.PostListWindow
import           Matterhorn.Events.MessageListing


onEventPostListWindow :: TeamId -> Vty.Event -> MH ()
onEventPostListWindow tId =
    void .
    handleEventWith [ mhHandleKeyboardEvent (postListWindowKeybindings tId)
                    , mhHandleKeyboardEvent (messageListingKeybindings tId (csTeam(tId).tsPostListWindow))
                    ]

-- | The keybindings we want to use while viewing a post list window
postListWindowKeybindings :: TeamId -> KeyConfig KeyEvent -> KeyDispatcher KeyEvent MH
postListWindowKeybindings tId kc = unsafeKeyDispatcher kc (postListWindowKeyHandlers tId)

postListWindowKeyHandlers :: TeamId -> [MHKeyEventHandler]
postListWindowKeyHandlers tId =
  [ onEvent ActivateListItemEvent "Jump to and select current message" $ postListJumpToCurrent tId
  ]
