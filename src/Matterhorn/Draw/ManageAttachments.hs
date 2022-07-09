{-# LANGUAGE RankNTypes #-}
module Matterhorn.Draw.ManageAttachments
  ( drawAttachmentList
  , drawFileBrowser
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
import           Lens.Micro.Platform ( Lens' )

import           Matterhorn.Types
import           Matterhorn.Types.KeyEvents
import           Matterhorn.Themes


drawAttachmentList :: ChatState -> Lens' ChatState (MessageInterface Name i) -> Widget Name
drawAttachmentList st which =
    let addBinding = ppMaybeBinding $ firstActiveBinding kc AttachmentListAddEvent
        delBinding = ppMaybeBinding $ firstActiveBinding kc AttachmentListDeleteEvent
        escBinding = ppMaybeBinding $ firstActiveBinding kc CancelEvent
        openBinding = ppMaybeBinding $ firstActiveBinding kc AttachmentOpenEvent
        kc = st^.csResources.crConfiguration.configUserKeysL
    in borderWithLabel (withDefAttr clientEmphAttr $ txt "Attachments") $
       vBox [ renderList renderAttachmentItem True (st^.which.miEditor.esAttachmentList)
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

drawFileBrowser :: ChatState -> Lens' ChatState (MessageInterface Name i) -> Widget Name
drawFileBrowser st which =
    borderWithLabel (withDefAttr clientEmphAttr $ txt "Attach File") $
    -- invariant: cedFileBrowser is not Nothing if appMode is
    -- ManageAttachmentsBrowseFiles, and that is the only way to reach
    -- this code, ergo the fromJust.
    FB.renderFileBrowser True $ fromJust (st^.which.miEditor.esFileBrowser)
