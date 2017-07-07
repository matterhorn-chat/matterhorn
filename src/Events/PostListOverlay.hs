module Events.PostListOverlay where

import qualified Graphics.Vty as Vty

import Types
import State.PostListOverlay

onEventPostListOverlay :: Vty.Event -> MH ()
onEventPostListOverlay e
  | Just kb <- lookupKeybinding e postListOverlayKeybindings =
      kbAction kb
onEventPostListOverlay _ = return ()

-- | The keybindings we want to use while viewing a post list overlay
postListOverlayKeybindings :: [Keybinding]
postListOverlayKeybindings =
  [ KB "Exit post browsing" (Vty.EvKey Vty.KEsc []) $
    exitPostListMode

  , KB "Exit post browsing" (Vty.EvKey (Vty.KChar 'c') [Vty.MCtrl]) $
    exitPostListMode

  , KB "Select the previous message" (Vty.EvKey (Vty.KChar 'k') []) $
    postListSelectUp

  , KB "Select the previous message" (Vty.EvKey Vty.KUp []) $
    postListSelectUp

  , KB "Select the next message" (Vty.EvKey (Vty.KChar 'j') []) $
    postListSelectDown

  , KB "Select the next message" (Vty.EvKey Vty.KDown []) $
    postListSelectDown

  , KB "Toggle the selected message flag" (Vty.EvKey (Vty.KChar 'f') []) $
    postListUnflagSelected
  ]
