module Events.ChannelSelect where

import Prelude ()
import Prelude.Compat

import Control.Monad (when)
import qualified Data.Text as T
import qualified Graphics.Vty as Vty
import Lens.Micro.Platform

import Events.Keybindings
import Types
import State

onEventChannelSelect :: Vty.Event -> MH ()
onEventChannelSelect e | Just kb <- lookupKeybinding e channelSelectKeybindings =
    kbAction kb
onEventChannelSelect (Vty.EvKey Vty.KBS []) = do
    csChannelSelectState.channelSelectInput %= (\s -> if T.null s then s else T.init s)
    updateChannelSelectMatches
onEventChannelSelect (Vty.EvKey (Vty.KChar c) []) | c /= '\t' = do
    csChannelSelectState.channelSelectInput %= (flip T.snoc c)
    updateChannelSelectMatches
onEventChannelSelect _ = return ()

channelSelectKeybindings :: [Keybinding]
channelSelectKeybindings =
    [ KB "Switch to selected channel"
         (Vty.EvKey Vty.KEnter []) $ do
             selMatch <- use (csChannelSelectState.selectedMatch)

             csMode .= Main
             when (selMatch /= "") $ do
                 changeChannel selMatch

    , KB "Cancel channel selection"
         (Vty.EvKey Vty.KEsc []) $ do
           csMode .= Main

    , KB "Cancel channel selection"
         (Vty.EvKey (Vty.KChar 'c') [Vty.MCtrl]) $ do
           csMode .= Main

    , KB "Select next match"
         (Vty.EvKey (Vty.KChar 'n') [Vty.MCtrl]) $ do
           channelSelectNext

    , KB "Select previous match"
         (Vty.EvKey (Vty.KChar 'p') [Vty.MCtrl]) $ do
           channelSelectPrevious
    ]
