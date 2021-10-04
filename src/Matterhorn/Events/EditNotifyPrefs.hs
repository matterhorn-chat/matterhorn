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
import           Network.Mattermost.Types ( TeamId )

import           Lens.Micro.Platform (_Just, (.=), singular)

import           Matterhorn.Types
import           Matterhorn.Types.KeyEvents
import           Matterhorn.Events.Keybindings
import           Matterhorn.State.NotifyPrefs
import           Matterhorn.State.Async

onEventEditNotifyPrefs :: TeamId -> V.Event -> MH Bool
onEventEditNotifyPrefs tId =
    handleKeyboardEvent (editNotifyPrefsKeybindings tId) (handleEditNotifyPrefsEvent tId . VtyEvent)

handleEditNotifyPrefsEvent :: TeamId -> BrickEvent Name MHEvent -> MH ()
handleEditNotifyPrefsEvent tId e = do
    form <- use (csTeam(tId).tsNotifyPrefs.singular _Just)
    updatedForm <- mh $ handleFormEvent e form
    csTeam(tId).tsNotifyPrefs .= Just updatedForm

editNotifyPrefsKeybindings :: TeamId -> KeyConfig -> KeyHandlerMap
editNotifyPrefsKeybindings tId = mkKeybindings (editNotifyPrefsKeyHandlers tId)

editNotifyPrefsKeyHandlers :: TeamId -> [KeyEventHandler]
editNotifyPrefsKeyHandlers tId =
    [ mkKb CancelEvent "Close channel notification preferences" exitEditNotifyPrefsMode
    , mkKb FormSubmitEvent "Save channel notification preferences" $ do
        st <- use id
        let form = fromJust $ st^.csTeam(tId).tsNotifyPrefs
            cId = st^.csCurrentChannelId(tId)

        doAsyncChannelMM Preempt cId
          (\s _ -> MM.mmUpdateChannelNotifications cId (myUserId st) (formState form) s)
          (\_ _ -> Nothing)
        exitEditNotifyPrefsMode
    ]
