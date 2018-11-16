module Events.ChannelSelect where

import           Prelude ()
import           Prelude.MH

import qualified Data.Text as T
import qualified Graphics.Vty as Vty
import           Lens.Micro.Platform ( (%=) )

import           Events.Keybindings
import           State.Channels
import           State.ChannelSelect
import           Types
import qualified Zipper as Z


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
             matches <- use (csChannelSelectState.channelSelectMatches)
             case Z.focus matches of
                 Nothing -> return ()
                 Just match -> do
                     setMode Main
                     setFocus $ channelListEntryChannelId $ matchEntry match

    , mkKb CancelEvent "Cancel channel selection" $ setMode Main
    , mkKb NextChannelEvent "Select next match" channelSelectNext
    , mkKb PrevChannelEvent "Select previous match" channelSelectPrevious
    ]
