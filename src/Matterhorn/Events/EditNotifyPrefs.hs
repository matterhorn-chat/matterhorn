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
import           Matterhorn.State.NotifyPrefs
import           Matterhorn.State.Async

onEventEditNotifyPrefs :: TeamId -> V.Event -> MH Bool
onEventEditNotifyPrefs tId =
    handleEventWith [ mhHandleKeyboardEvent (editNotifyPrefsKeybindings tId)
                    , handleEditNotifyPrefsEvent tId . VtyEvent
                    ]

handleEditNotifyPrefsEvent :: TeamId -> BrickEvent Name MHEvent -> MH Bool
handleEditNotifyPrefsEvent tId e = do
    form <- use (csTeam(tId).tsNotifyPrefs.singular _Just)
    updatedForm <- mh $ handleFormEvent e form
    csTeam(tId).tsNotifyPrefs .= Just updatedForm
    return True

editNotifyPrefsKeybindings :: TeamId -> KeyConfig KeyEvent -> KeyHandlerMap KeyEvent MH
editNotifyPrefsKeybindings tId = mkKeybindings (editNotifyPrefsKeyHandlers tId)

editNotifyPrefsKeyHandlers :: TeamId -> [KeyEventHandler KeyEvent MH]
editNotifyPrefsKeyHandlers tId =
    [ mkKb CancelEvent "Close channel notification preferences" $
        exitEditNotifyPrefsMode tId
    , mkKb FormSubmitEvent "Save channel notification preferences" $ do
        st <- use id
        withCurrentChannel tId $ \cId _ -> do
            let form = fromJust $ st^.csTeam(tId).tsNotifyPrefs
            doAsyncChannelMM Preempt cId
              (\s _ -> MM.mmUpdateChannelNotifications cId (myUserId st) (formState form) s)
              (\_ _ -> Nothing)
            exitEditNotifyPrefsMode tId
    ]
