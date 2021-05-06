module Matterhorn.State.SaveAttachmentWindow
  ( openSaveAttachmentWindow
  )
where

import           Prelude ()
import           Matterhorn.Prelude
import           Brick.Widgets.List ( listSelectedElement )

import           Lens.Micro.Platform ( (.=), to )

import           Network.Mattermost.Types ( fileInfoName )
import           Network.Mattermost.Endpoints ( mmGetMetadataForFile )

import           Matterhorn.Types
import           Matterhorn.State.Common


-- | If the currently selected link in the URL list is for an
-- attachment, open a window to get the user to provide a path to which
-- to save the attachment. If the URL list is empty or if the selected
-- entry is not for an attachment, this returns to the Main mode but
-- otherwise does nothing.
openSaveAttachmentWindow :: MH ()
openSaveAttachmentWindow = do
    selected <- use (csCurrentTeam.tsUrlList.to listSelectedElement)
    case selected of
        Nothing -> return ()
        Just (_, (_, link)) ->
            case link^.linkTarget of
                LinkFileId fId -> do
                    tId <- use csCurrentTeamId
                    session <- getSession
                    doAsyncWith Normal $ do
                        info <- mmGetMetadataForFile fId session
                        return $ Just $ do
                            csCurrentTeam.tsSaveAttachmentDialog .= newSaveAttachmentDialog tId (fileInfoName info)
                            setMode $ SaveAttachmentWindow link
                _ ->
                    -- The selected link is not for an attachment.
                    return ()
