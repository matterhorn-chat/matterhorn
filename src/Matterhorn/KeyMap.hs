module Matterhorn.KeyMap
  ( keybindingModeMap
  )
where

import           Prelude ()
import           Matterhorn.Prelude

import           Network.Mattermost.Types ( TeamId )

import           Matterhorn.Events.Keybindings
import           Matterhorn.Events.ChannelSelect
import           Matterhorn.Events.Main
import           Matterhorn.Events.MessageSelect
import           Matterhorn.Events.PostListOverlay
import           Matterhorn.Events.ShowHelp
import           Matterhorn.Events.UrlSelect
import           Matterhorn.Events.ManageAttachments

keybindingModeMap :: [(String, TeamId -> KeyConfig -> KeyHandlerMap)]
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
