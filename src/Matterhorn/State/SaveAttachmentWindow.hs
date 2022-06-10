{-# LANGUAGE RankNTypes #-}
module Matterhorn.State.SaveAttachmentWindow
  ( openSaveAttachmentWindow
  , closeSaveAttachmentWindow
  )
where

import           Prelude ()
import           Matterhorn.Prelude
import           Brick ( getName )
import           Brick.Widgets.List ( listSelectedElement )

import           Lens.Micro.Platform ( Lens', (.=), to )

import           Network.Mattermost.Types ( fileInfoName )
import           Network.Mattermost.Endpoints ( mmGetMetadataForFile )

import           Matterhorn.Types
import           Matterhorn.State.Common
import           Matterhorn.State.Teams ( newSaveAttachmentDialog )


-- | If the currently selected link in the URL list is for an
-- attachment, open a window to get the user to provide a path to which
-- to save the attachment. If the URL list is empty or if the selected
-- entry is not for an attachment, this returns to the Main mode but
-- otherwise does nothing.
openSaveAttachmentWindow :: Lens' ChatState (MessageInterface Name i) -> MH ()
openSaveAttachmentWindow which = do
    selected <- use (which.miUrlList.ulList.to listSelectedElement)
    case selected of
        Nothing -> return ()
        Just (_, (_, link)) ->
            case link^.linkTarget of
                LinkFileId fId -> do
                    session <- getSession
                    doAsyncWith Normal $ do
                        info <- mmGetMetadataForFile fId session
                        return $ Just $ do
                            -- Use the message interface's URL list name
                            -- as a unique basis for the names of the UI
                            -- elements in the attachment dialog
                            listName <- getName <$> use (which.miUrlList.ulList)
                            which.miSaveAttachmentDialog .= newSaveAttachmentDialog listName (fileInfoName info)
                            which.miMode .= SaveAttachment link
                _ ->
                    -- The selected link is not for an attachment.
                    return ()

closeSaveAttachmentWindow :: Lens' ChatState (MessageInterface n i)
                          -> MH ()
closeSaveAttachmentWindow which = do
    -- TODO: this might need to be some other mode
    which.miMode .= Compose
