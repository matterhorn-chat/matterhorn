module Events.ChannelSelect where

import Prelude ()
import Prelude.Compat

import Data.Monoid ((<>))
import qualified Data.Text as T
import qualified Graphics.Vty as Vty
import Lens.Micro.Platform

import Events.Keybindings
import Types
import State

onEventChannelSelect :: Vty.Event -> MH ()
onEventChannelSelect =
  handleKeyboardEvent channelSelectKeybindings $ \ e -> case e of
    (Vty.EvKey Vty.KBS []) -> do
      csChannelSelectState.channelSelectInput %= (\s -> if T.null s then s else T.init s)
      updateChannelSelectMatches
    (Vty.EvKey (Vty.KChar c) []) | c /= '\t' -> do
      csChannelSelectState.channelSelectInput %= (flip T.snoc c)
      updateChannelSelectMatches
    _ -> return ()

channelSelectKeybindings :: KeyConfig -> [Keybinding]
channelSelectKeybindings = mkKeybindings
    [ staticKb "Switch to selected channel"
         (Vty.EvKey Vty.KEnter []) $ do
             selMatch <- use (csChannelSelectState.selectedMatch)

             setMode Main

             let switch (UserMatch m) = changeChannel (userSigil <> m)
                 switch (ChannelMatch m) = changeChannel (normalChannelSigil <> m)
             maybe (return ()) switch selMatch

    , mkKb CancelEvent "Cancel channel selection" $ setMode Main
    , mkKb NextChannelEvent "Select next match" channelSelectNext
    , mkKb PrevChannelEvent "Select previous match" channelSelectPrevious
    ]
