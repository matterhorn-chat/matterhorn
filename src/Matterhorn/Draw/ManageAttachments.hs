module Matterhorn.Draw.ManageAttachments
  ( drawManageAttachments
  )
where

import           Prelude ()
import           Matterhorn.Prelude

import           Brick
import           Brick.Widgets.Border
import           Brick.Widgets.Center
import qualified Brick.Widgets.FileBrowser as FB
import           Brick.Widgets.List
import           Data.Maybe ( fromJust )

import           Network.Mattermost.Types ( TeamId )

import           Matterhorn.Types
import           Matterhorn.Types.KeyEvents
import           Matterhorn.Events.Keybindings ( firstActiveBinding )
import           Matterhorn.Themes


drawManageAttachments :: ChatState -> TeamId -> Widget Name
drawManageAttachments st tId =
    topLayer
    where
        topLayer = case st^.csTeam(tId).tsMode of
            ManageAttachments -> drawAttachmentList st tId
            ManageAttachmentsBrowseFiles -> drawFileBrowser st tId
            _ -> error "BUG: drawManageAttachments called in invalid mode"

drawAttachmentList :: ChatState -> TeamId -> Widget Name
drawAttachmentList st tId =
    let addBinding = ppBinding $ firstActiveBinding kc AttachmentListAddEvent
        delBinding = ppBinding $ firstActiveBinding kc AttachmentListDeleteEvent
        escBinding = ppBinding $ firstActiveBinding kc CancelEvent
        openBinding = ppBinding $ firstActiveBinding kc AttachmentOpenEvent
        kc = st^.csResources.crConfiguration.configUserKeysL
    in centerLayer $
       hLimit 60 $
       vLimit 15 $
       joinBorders $
       borderWithLabel (withDefAttr clientEmphAttr $ txt "Attachments") $
       vBox [ renderList renderAttachmentItem True (st^.csTeam(tId).tsEditState.cedAttachmentList)
            , hBorder
            , hCenter $ withDefAttr clientMessageAttr $
                        txt $ addBinding <> ":add " <>
                              delBinding <> ":delete " <>
                              openBinding <> ":open " <>
                              escBinding <> ":close"
            ]

renderAttachmentItem :: Bool -> AttachmentData -> Widget Name
renderAttachmentItem _ d =
    padRight Max $ str $ FB.fileInfoSanitizedFilename $ attachmentDataFileInfo d

drawFileBrowser :: ChatState -> TeamId -> Widget Name
drawFileBrowser st tId =
    centerLayer $
    hLimit 60 $
    vLimit 20 $
    borderWithLabel (withDefAttr clientEmphAttr $ txt "Attach File") $
    -- invariant: cedFileBrowser is not Nothing if appMode is
    -- ManageAttachmentsBrowseFiles, and that is the only way to reach
    -- this code, ergo the fromJust.
    FB.renderFileBrowser True $ fromJust (st^.csTeam(tId).tsEditState.cedFileBrowser)
