{-# LANGUAGE TemplateHaskell #-}
module Matterhorn.Types.MessageInterface
  ( MessageInterface(..)
  , miEditor
  , miMode
  , miRootPostId
  , miChannelId
  , miTarget
  , miListing
  , miUrlList
  , miSaveAttachmentDialog
  , miBookmarkManager

  , MessageListing(..)
  , mlUrlListSource
  , mlMessageSelect
  , mlMessages
  , mlMode

  , messageInterfaceCursor

  , MessageInterfaceMode(..)
  , MessageListingMode(..)
  , MessageInterfaceTarget(..)
  , URLListSource(..)

  , URLList(..)
  , ulList
  , ulSource

  , SaveAttachmentDialogState(..)
  , attachmentPathEditor
  , attachmentPathDialogFocus

  , BookmarkManager(..)
  , bmBookmarkList
  )
where

import           Prelude ()
import           Matterhorn.Prelude

import           Brick ( getName )
import           Brick.Focus ( FocusRing )
import           Brick.Widgets.List ( List, GenericList )
import           Brick.Widgets.Edit ( Editor )
import           Brick.Widgets.FileBrowser ( fileBrowserNameG )
import qualified Data.Text as T
import qualified Data.Sequence as Seq
import           Lens.Micro.Platform ( makeLenses, _Just )
import           Network.Mattermost.Types ( ChannelId, TeamId, Bookmark )

import           Matterhorn.Types.Core ( MessageSelectState )
import           Matterhorn.Types.EditState
import           Matterhorn.Types.Messages


-- | A UI region in which a specific message listing is viewed, in which
-- the user can select from the listed messages and take actions on
-- them.
data MessageListing n =
    MessageListing { _mlMessages :: !Messages
                   -- ^ The messages.
                   , _mlMessageSelect :: !MessageSelectState
                   -- ^ Message selection state for the listing.
                   , _mlMode :: !MessageListingMode
                   -- ^ The mode of the listing.
                   , _mlUrlListSource :: !URLListSource
                   -- ^ How to characterize the URLs found in messages
                   -- in this listing
                   }

data BookmarkManager n =
    BookmarkManager { _bmBookmarkList :: !(GenericList n Seq.Seq Bookmark)
                    -- ^ The list of bookmarks shown in the list for editing
                    }

-- | A UI region in which a specific message listing is viewed, where
-- the user can send messages in that channel or thread and edit,
-- delete, or otherwise modify messages.
data MessageInterface n i =
    MessageInterface { _miEditor :: !(EditState n)
                     -- ^ The editor and associated state for composing
                     -- messages in this channel or thread.
                     , _miRootPostId :: !i
                     -- ^ The root post ID if these messages belong to a
                     -- thread.
                     , _miChannelId :: !ChannelId
                     -- ^ The channel that these messages belong to.
                     , _miMode :: !MessageInterfaceMode
                     -- ^ The mode of the interface.
                     , _miTarget :: !MessageInterfaceTarget
                     -- ^ The target value for this message interface
                     , _miListing :: MessageListing n
                     -- ^ The message listing in this interface
                     , _miUrlList :: !(URLList n)
                     -- ^ The URL listing for this interface
                     , _miSaveAttachmentDialog :: !(SaveAttachmentDialogState n)
                     -- ^ The state for the interactive attachment-saving
                     -- editor window.
                     , _miBookmarkManager :: !(BookmarkManager n)
                     -- ^ The bookmark manager for this interface, used
                     -- only for channel interfaces
                     }

messageInterfaceCursor :: MessageInterface n i -> Maybe n
messageInterfaceCursor mi =
    case _mlMode (_miListing mi) of
        MessageSelect -> Nothing
        ShowingTail ->
            case _miMode mi of
                Compose           -> Just $ getName $ _esEditor $ _miEditor mi
                BrowseFiles       -> (_esFileBrowser $ _miEditor mi)^?_Just.fileBrowserNameG
                SaveAttachment {} -> Just $ getName $ _attachmentPathEditor $ _miSaveAttachmentDialog mi
                ManageAttachments -> Nothing
                ManageBookmarks   -> Nothing
                ShowUrlList       -> Nothing

data MessageListingMode =
    MessageSelect
    -- ^ Selecting from messages in the listing
    | ShowingTail
    -- ^ Showing the most recent messages of the listing
    deriving (Eq, Show)

data MessageInterfaceMode =
    Compose
    -- ^ Composing messages and interacting with the editor
    | ManageAttachments
    -- ^ Managing the attachment list
    | ManageBookmarks
    -- ^ Managing the bookmark list
    | BrowseFiles
    -- ^ Browsing the filesystem for attachment files
    | ShowUrlList
    -- ^ Show the URL listing
    | SaveAttachment !LinkChoice
    -- ^ Show the attachment save UI
    deriving (Eq, Show)

data URLListSource =
    FromChannel !ChannelId
    | FromThreadIn !ChannelId
    | FromPostList
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
makeLenses ''MessageListing
makeLenses ''URLList
makeLenses ''SaveAttachmentDialogState
makeLenses ''BookmarkManager
