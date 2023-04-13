module Matterhorn.Events.ChannelSelect where

import           Prelude ()
import           Matterhorn.Prelude

import           Brick ( BrickEvent(VtyEvent) )
import           Brick.Keybindings
import           Brick.Widgets.Edit ( handleEditorEvent )
import qualified Graphics.Vty as Vty

import           Network.Mattermost.Types ( TeamId )

import           Matterhorn.State.Channels
import           Matterhorn.State.ChannelSelect
import           Matterhorn.State.Editing ( editingKeybindings )
import           Matterhorn.Types
import qualified Matterhorn.Zipper as Z


onEventChannelSelect :: TeamId -> Vty.Event -> MH ()
onEventChannelSelect tId =
    void .
    handleEventWith [ mhHandleKeyboardEvent (channelSelectKeybindings tId)
                    , \e -> do
                        void $ handleEventWith [ mhHandleKeyboardEvent (editingKeybindings (csTeam(tId).tsChannelSelectState.channelSelectInput))
                                               , \ev -> do
                                                   mhZoom (csTeam(tId).tsChannelSelectState.channelSelectInput) handleEditorEvent (VtyEvent ev)
                                                   return True
                                               ] e
                        updateChannelSelectMatches tId
                        return True
                    ]

channelSelectKeybindings :: TeamId -> KeyConfig KeyEvent -> KeyDispatcher KeyEvent MH
channelSelectKeybindings tId kc = unsafeKeyDispatcher kc (channelSelectKeyHandlers tId)

channelSelectKeyHandlers :: TeamId -> [MHKeyEventHandler]
channelSelectKeyHandlers tId =
    [ onKey (bind Vty.KEnter)
          "Switch to selected channel" $ do
             matches <- use (csTeam(tId).tsChannelSelectState.channelSelectMatches)
             case Z.focus matches of
                 Nothing -> return ()
                 Just match -> do
                     popMode tId
                     setFocus tId $ channelListEntryChannelId $ matchEntry match

    , onEvent CancelEvent "Cancel channel selection" $ popMode tId
    , onEvent NextChannelEvent "Select next match" $ channelSelectNext tId
    , onEvent PrevChannelEvent "Select previous match" $ channelSelectPrevious tId
    , onEvent NextChannelEventAlternate "Select next match (alternate binding)" $ channelSelectNext tId
    , onEvent PrevChannelEventAlternate "Select previous match (alternate binding)" $ channelSelectPrevious tId
    ]
