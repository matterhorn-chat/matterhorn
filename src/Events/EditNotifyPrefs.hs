module Events.EditNotifyPrefs
    ( onEventEditNotifyPrefs
    , editNotifyPrefsKeybindings
    , editNotifyPrefsKeyHandlers
    )
where

import           Prelude ()
import           Prelude.MH

import           Brick
import           Brick.Forms (handleFormEvent)
import qualified Graphics.Vty as V

import           Lens.Micro.Platform (_Just, (.=), singular)

import           Types
import           Types.KeyEvents
import           Events.Keybindings
import           State.NotifyPrefs

onEventEditNotifyPrefs :: V.Event -> MH Bool
onEventEditNotifyPrefs =
    let fallback e = do
            form <- use (csNotifyPrefs.singular _Just)
            updatedForm <- mh $ handleFormEvent (VtyEvent e) form
            csNotifyPrefs .= Just updatedForm
    in handleKeyboardEvent editNotifyPrefsKeybindings fallback

editNotifyPrefsKeybindings :: KeyConfig -> KeyHandlerMap
editNotifyPrefsKeybindings = mkKeybindings editNotifyPrefsKeyHandlers

editNotifyPrefsKeyHandlers :: [KeyEventHandler]
editNotifyPrefsKeyHandlers =
    [ mkKb CancelEvent "Close channel notification preferences" exitEditNotifyPrefsMode ]
