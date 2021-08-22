module Matterhorn.Events.EditNotifyPrefs
    ( onEventEditNotifyPrefs
    , editNotifyPrefsKeybindings
    , editNotifyPrefsKeyHandlers
    , handleEditNotifyPrefsEvent
    )
where

import           Prelude ()
import           Matterhorn.Prelude

import           Brick
import           Brick.Forms (handleFormEvent, formState)
import           Data.Maybe (fromJust)
import qualified Graphics.Vty as V
import qualified Network.Mattermost.Endpoints as MM

import           Lens.Micro.Platform (_Just, (.=), singular)

import           Matterhorn.Types
import           Matterhorn.Types.KeyEvents
import           Matterhorn.Events.Keybindings
import           Matterhorn.State.NotifyPrefs
import           Matterhorn.State.Async

onEventEditNotifyPrefs :: V.Event -> MH Bool
onEventEditNotifyPrefs =
    handleKeyboardEvent editNotifyPrefsKeybindings (handleEditNotifyPrefsEvent . VtyEvent)

handleEditNotifyPrefsEvent :: BrickEvent Name MHEvent -> MH ()
handleEditNotifyPrefsEvent e = do
    form <- use (csCurrentTeam.tsNotifyPrefs.singular _Just)
    updatedForm <- mh $ handleFormEvent e form
    csCurrentTeam.tsNotifyPrefs .= Just updatedForm

editNotifyPrefsKeybindings :: KeyConfig -> KeyHandlerMap
editNotifyPrefsKeybindings = mkKeybindings editNotifyPrefsKeyHandlers

editNotifyPrefsKeyHandlers :: [KeyEventHandler]
editNotifyPrefsKeyHandlers =
    [ mkKb CancelEvent "Close channel notification preferences" exitEditNotifyPrefsMode
    , mkKb FormSubmitEvent "Save channel notification preferences" $ do
        st <- use id
        let form = fromJust $ st^.csCurrentTeam.tsNotifyPrefs
            cId = st^.csCurrentChannelId(st^.csCurrentTeamId)

        doAsyncChannelMM Preempt cId
          (\s _ -> MM.mmUpdateChannelNotifications cId (myUserId st) (formState form) s)
          (\_ _ -> Nothing)
        exitEditNotifyPrefsMode
    ]
