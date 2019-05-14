module Events.MessageReactions
  ( messageReactionsKeybindings
  , onEventMessageReactions
  )
where

import           Prelude ()
import           Prelude.MH

import           Brick
import qualified Graphics.Vty as Vty

import           Events.Keybindings
import           Types
import           Constants


onEventMessageReactions :: Vty.Event -> MH ()
onEventMessageReactions =
  handleKeyboardEvent messageReactionsKeybindings $ \ _ -> return ()

vs :: ViewportScroll Name
vs = viewportScroll MessageReactionsArea

messageReactionsKeybindings :: KeyConfig -> [Keybinding]
messageReactionsKeybindings = mkKeybindings
    [ mkKb CancelEvent "Close reactions window" $
        setMode Main

    , mkKb ScrollUpEvent
        "Scroll the reaction list up"
        (mh $ vScrollBy vs (-1))

    , mkKb ScrollDownEvent
        "Scroll the reaction list down"
        (mh $ vScrollBy vs 1)

    , mkKb ScrollTopEvent
        "Scroll to the top of the reaction list"
        (mh $ vScrollToBeginning vs)

    , mkKb ScrollBottomEvent
        "Scroll to the bottom of the reaction list"
        (mh $ vScrollToEnd vs)

    , mkKb PageUpEvent
        "Scroll the reaction list up"
        (mh $ vScrollBy vs (-1 * pageAmount))

    , mkKb PageDownEvent
        "Scroll the reaction list down"
        (mh $ vScrollBy vs pageAmount)
    ]
