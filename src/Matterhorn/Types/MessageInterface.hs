{-# LANGUAGE TemplateHaskell #-}
module Matterhorn.Types.MessageInterface
  ( MessageInterface(..)
  , miMessages
  , miEditor
  , miMode
  , miMessageSelect
  , miRootPostId
  , miChannelId
  , miTarget
  , miUrlListSource
  , miUrlList
  , miSaveAttachmentDialog

  , messageInterfaceCursor

  , MessageInterfaceMode(..)
  , MessageInterfaceTarget(..)
  , URLListSource(..)

  , URLList(..)
  , ulList
  , ulSource

  , SaveAttachmentDialogState(..)
  , attachmentPathEditor
  , attachmentPathDialogFocus
  )
where

import           Prelude ()
import           Matterhorn.Prelude

import           Brick ( getName )
import           Brick.Focus ( FocusRing )
import           Brick.Widgets.List ( List )
import           Brick.Widgets.Edit ( Editor )
import           Brick.Widgets.FileBrowser ( fileBrowserNameG )
import qualified Data.Text as T
import           Lens.Micro.Platform ( makeLenses, _Just )
import           Network.Mattermost.Types ( ChannelId, TeamId )

import           Matterhorn.Types.Core ( MessageSelectState )
import           Matterhorn.Types.EditState
import           Matterhorn.Types.Messages


-- | A UI region in which a specific message listing is viewed, where
-- the user can send messages in that channel or thread.
data MessageInterface n i =
    MessageInterface { _miMessages :: !Messages
                     -- ^ The messages.
                     , _miEditor :: !(EditState n)
                     -- ^ The editor and associated state for composing
                     -- messages in this channel or thread.
                     , _miMessageSelect :: !MessageSelectState
                     -- ^ Message selection state for the interface.
                     , _miRootPostId :: !i
                     -- ^ The root post ID if these messages belong to a
                     -- thread.
                     , _miChannelId :: !ChannelId
                     -- ^ The channel that these messages belong to.
                     , _miMode :: !MessageInterfaceMode
                     -- ^ The mode of the interface.
                     , _miTarget :: !MessageInterfaceTarget
                     -- ^ The target value for this message interface
                     , _miUrlListSource :: !URLListSource
                     -- ^ How to characterize the URLs found in messages
                     -- in this interface
                     , _miUrlList :: !(URLList n)
                     -- ^ The URL listing for this interface
                     , _miSaveAttachmentDialog :: !(SaveAttachmentDialogState n)
                     -- ^ The state for the interactive attachment-saving
                     -- editor window.
                     }

messageInterfaceCursor :: MessageInterface n i -> Maybe n
messageInterfaceCursor mi =
    case _miMode mi of
        Compose           -> Just $ getName $ _esEditor $ _miEditor mi
        SaveAttachment {} -> Just $ getName $ _attachmentPathEditor $ _miSaveAttachmentDialog mi
        BrowseFiles       -> (_esFileBrowser $ _miEditor mi)^?_Just.fileBrowserNameG
        ManageAttachments -> Nothing
        MessageSelect     -> Nothing
        ShowUrlList       -> Nothing

data MessageInterfaceMode =
    Compose
    -- ^ Composing messages and interacting with the editor
    | MessageSelect
    -- ^ Selecting from messages in the listing
    | ShowUrlList
    -- ^ Show the URL listing
    | SaveAttachment !LinkChoice
    -- ^ Show the attachment save UI
    | ManageAttachments
    -- ^ Managing the attachment list
    | BrowseFiles
    -- ^ Browsing the filesystem for attachment files
    deriving (Eq, Show)

data URLListSource =
    FromChannel !ChannelId
    | FromThreadIn !ChannelId
    deriving (Show, Eq)

data MessageInterfaceTarget =
    MITeamThread !TeamId
    | MIChannel !ChannelId
    deriving (Eq, Show)

data URLList n =
    URLList { _ulList :: !(List n (Int, LinkChoice))
            , _ulSource :: !(Maybe URLListSource)
            }

-- | The state of the attachment path window.
data SaveAttachmentDialogState n =
    SaveAttachmentDialogState { _attachmentPathEditor :: !(Editor T.Text n)
                              -- ^ The attachment path editor state.
                              , _attachmentPathDialogFocus :: !(FocusRing n)
                              -- ^ The window focus state (editor/buttons)
                              }

makeLenses ''MessageInterface
makeLenses ''URLList
makeLenses ''SaveAttachmentDialogState
