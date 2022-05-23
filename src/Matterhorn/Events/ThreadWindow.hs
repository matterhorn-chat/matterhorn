module Matterhorn.Events.ThreadWindow
  ( onEventThreadWindow
  , threadWindowKeyHandlers
  )
where

import Prelude ()
import Matterhorn.Prelude

import qualified Graphics.Vty as Vty
import Lens.Micro.Platform (Lens')

import Network.Mattermost.Types (TeamId)

import Matterhorn.Types
import Matterhorn.State.ThreadWindow
import Matterhorn.Events.Keybindings
import Matterhorn.Events.MessageInterface

onEventThreadWindow :: TeamId -> Vty.Event -> MH ()
onEventThreadWindow _ (Vty.EvResize {}) =
    return ()
onEventThreadWindow tId ev = do
    let ti :: Lens' ChatState ThreadInterface
        ti = unsafeThreadInterface tId

    void $ handleEventWith [ handleKeyboardEvent (threadWindowKeybindings tId)
                           , handleMessageInterfaceEvent tId ti
                           ] ev

threadWindowKeybindings :: TeamId
                        -> KeyConfig
                        -> KeyHandlerMap
threadWindowKeybindings tId =
    mkKeybindings (threadWindowKeyHandlers tId)

threadWindowKeyHandlers :: TeamId
                        -> [KeyEventHandler]
threadWindowKeyHandlers tId =
    [ mkKb CancelEvent
        "Close the thread window" $
        closeThreadWindow tId
    ]
