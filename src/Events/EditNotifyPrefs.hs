module Events.EditNotifyPrefs
    ( onEventEditNotifyPrefs
    , editNotifyPrefsKeybindings
    , editNotifyPrefsKeyHandlers
    )
where

import           Prelude ()
import           Prelude.MH

import           Brick
import           Brick.Forms (handleFormEvent, formState)
import           Data.Maybe (fromJust)
import qualified Graphics.Vty as V
import qualified Network.Mattermost.Endpoints as MM

import           Lens.Micro.Platform (_Just, (.=), singular)

import           Types
import           Types.KeyEvents
import           Events.Keybindings
import           State.NotifyPrefs
import           State.Async

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
    [ mkKb CancelEvent "Close channel notification preferences" exitEditNotifyPrefsMode
    , staticKb "Save channel notification preferences"
        (V.EvKey V.KEnter []) $ do
            st <- use id
            let form = fromJust $ st^.csNotifyPrefs
                cId = st^.csCurrentChannelId

            doAsyncChannelMM Preempt cId
                (\s _ -> MM.mmUpdateChannelNotifications cId (myUserId st) (formState form) s)
                (\_ _ -> Nothing)
            exitEditNotifyPrefsMode
    ]
