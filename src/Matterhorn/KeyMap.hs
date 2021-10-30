module Matterhorn.KeyMap
  ( keybindingModeMap
  )
where

import           Prelude ()
import           Matterhorn.Prelude

import           Matterhorn.Events.Keybindings
import           Matterhorn.Events.ChannelSelect
import           Matterhorn.Events.Main
import           Matterhorn.Events.MessageSelect
import           Matterhorn.Events.PostListOverlay
import           Matterhorn.Events.ShowHelp
import           Matterhorn.Events.UrlSelect
import           Matterhorn.Events.ManageAttachments

keybindingModeMap :: [(String, [KeyEventHandler])]
keybindingModeMap =
    [ ("main", mainKeyHandlers)
    , ("help screen", helpKeyHandlers)
    , ("channel select", channelSelectKeyHandlers)
    , ("url select", urlSelectKeyHandlers)
    , ("message select", messageSelectKeyHandlers)
    , ("post list overlay", postListOverlayKeyHandlers)
    , ("attachment list", attachmentListKeyHandlers)
    , ("attachment file browse", attachmentBrowseKeyHandlers)
    ]
