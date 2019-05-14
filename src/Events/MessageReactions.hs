module Events.MessageReactions
  ( onEventMessageReactions
  )
where

import           Prelude ()
import           Prelude.MH

import qualified Graphics.Vty as Vty

import           Events.Keybindings
import           Types


onEventMessageReactions :: Vty.Event -> MH ()
onEventMessageReactions =
  handleKeyboardEvent messageReactionsKeybindings $ \ _ -> return ()

messageReactionsKeybindings :: KeyConfig -> [Keybinding]
messageReactionsKeybindings = mkKeybindings
    [ mkKb CancelEvent "Close reactions window" $
        setMode Main
    ]
