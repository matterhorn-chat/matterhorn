module Matterhorn.Events.ThreadWindow
  ( onEventThreadWindow
  , threadWindowKeyHandlers
  )
where

import Prelude ()
import Matterhorn.Prelude

import qualified Graphics.Vty as Vty
import Lens.Micro.Platform (Lens', _Just, singular, to)

import Network.Mattermost.Types (TeamId)

import Matterhorn.Types
import Matterhorn.State.Editing
import Matterhorn.State.ThreadWindow
import Matterhorn.Events.Keybindings
import Matterhorn.Events.Main

onEventThreadWindow :: TeamId -> Vty.Event -> MH ()
onEventThreadWindow _ (Vty.EvResize {}) =
    return ()
onEventThreadWindow tId ev = do
    st <- use id

    let ti :: Lens' ChatState ThreadInterface
        ti = csTeam(tId).tsThreadInterface.singular _Just

    void $ handleKeyboardEvent (threadWindowKeybindings tId) (\ e -> do
        let fallback e2 = do
                let fallback2 e3 = do
                        case e3 of
                            (Vty.EvPaste bytes) -> handlePaste (ti.threadEditor) bytes
                            _ -> handleEditingInput tId (ti.threadEditor) e3
                    bindings = messageListingKeybindings tId
                                                         (ti.threadMessageSelect)
                                                         (ti.threadMessages)
                                                         (Just $ FromThread $ st^.ti.threadRootPostId)
                                                         ThreadWindowMessageSelect
                void $ handleKeyboardEvent bindings fallback2 e2
        void $ handleKeyboardEvent (messageEditorKeybindings tId (ti.threadEditor)
                                       (ti.threadParentChannelId.to Just))
                                   fallback e
        ) ev

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
