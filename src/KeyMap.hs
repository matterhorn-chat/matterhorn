module KeyMap
  ( keybindingModeMap
  )
where

import           Prelude ()
import           Prelude.MH

import           Events.Keybindings
import           Events.ChannelSelect
import           Events.Main
import           Events.MessageSelect
import           Events.PostListOverlay
import           Events.ShowHelp
import           Events.UrlSelect
import           Events.ManageAttachments

keybindingModeMap :: [(String, KeyConfig -> [Keybinding])]
keybindingModeMap =
    [ ("main", mainKeybindings)
    , ("help screen", helpKeybindings)
    , ("channel select", channelSelectKeybindings)
    , ("url select", urlSelectKeybindings)
    , ("message select", messageSelectKeybindings)
    , ("post list overlay", postListOverlayKeybindings)
    , ("attachment list", attachmentListKeybindings)
    , ("attachment file browse", attachmentBrowseKeybindings)
    ]
