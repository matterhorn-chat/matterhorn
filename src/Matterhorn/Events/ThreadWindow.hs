module Matterhorn.Events.ThreadWindow
  ( onEventThreadWindow
  , threadWindowKeyHandlers
  )
where

import Prelude ()
import Matterhorn.Prelude

import qualified Graphics.Vty as Vty
import Lens.Micro.Platform (Lens', (.=))

import Network.Mattermost.Types (TeamId)

import Matterhorn.Types
import Matterhorn.State.Editing
import Matterhorn.State.ThreadWindow
import Matterhorn.Events.Keybindings
import Matterhorn.Events.Main
import Matterhorn.Events.MessageSelect

onEventThreadWindow :: TeamId -> Vty.Event -> MH ()
onEventThreadWindow _ (Vty.EvResize {}) =
    return ()
onEventThreadWindow tId ev = do
    st <- use id

    let ti :: Lens' ChatState (ThreadInterface Name)
        ti = threadInterface tId

    case st^.ti.threadMode of
        MessageSelect ->
            onEventMessageSelect tId (ti.threadMessageSelect) (ti.threadMessages) (ti.threadEditor) ev
        Compose -> do
            let messageListingBindings = messageListingKeybindings tId (ti.threadMessageSelect)
                                                                       (ti.threadMessages)
                                                                       (Just $ FromThreadIn $ st^.ti.threadParentChannelId)
                                                                       (ti.threadMode .= MessageSelect)
            void $ handleEventWith [ handleKeyboardEvent (threadWindowKeybindings tId)
                                   , handleKeyboardEvent (messageEditorKeybindings (ti.threadEditor))
                                   , handleKeyboardEvent messageListingBindings
                                   , \_ -> do
                                       case ev of
                                           (Vty.EvPaste bytes) -> handlePaste (ti.threadEditor) bytes
                                           _ -> handleEditingInput (ti.threadEditor) ev
                                       return True
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
