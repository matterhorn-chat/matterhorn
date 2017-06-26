module Events.PostListOverlay where

import qualified Graphics.Vty as Vty
import           Lens.Micro.Platform

import Types
import State.PostListOverlay

onEventPostListOverlay :: Vty.Event -> MH ()
onEventPostListOverlay e
  | Just kb <- lookupKeybinding e postListOverlayKeybindings =
      kbAction kb
onEventPostListOverlay _ = return ()

postListOverlayKeybindings :: [Keybinding]
postListOverlayKeybindings =
  [ KB "Exit post browsing" (Vty.EvKey Vty.KEsc []) $
    csMode .= Main

  , KB "Exit post browsing" (Vty.EvKey (Vty.KChar 'c') [Vty.MCtrl]) $
    csMode .= Main

  , KB "Select the previous message" (Vty.EvKey (Vty.KChar 'k') []) $
    postListSelectUp

  , KB "Select the previous message" (Vty.EvKey Vty.KUp []) $
    postListSelectUp

  , KB "Select the next message" (Vty.EvKey (Vty.KChar 'j') []) $
    postListSelectDown

  , KB "Select the next message" (Vty.EvKey Vty.KDown []) $
    postListSelectDown
  ]
