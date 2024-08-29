{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
module Matterhorn.Types.Core
  ( Name(..)
  , ChannelListEntry(..)
  , ChannelListEntryType(..)
  , ChannelSelectMatch(..)
  , LinkTarget(..)

  , KeyEvent(..)
  , allEvents

  , MessageId(..)
  , messageIdPostId

  , ChannelListGroupLabel(..)
  , channelListGroupNames

  , MessageSelectState(..)
  , HelpScreen(..)
  )
where

import           Prelude ()
import           Matterhorn.Prelude

import qualified Brick
import           Brick.Keybindings
import           Data.Hashable ( Hashable )
import qualified Data.Text as T
import           Data.UUID ( UUID )
import           GHC.Generics ( Generic )
import           Network.Mattermost.Types

import           Matterhorn.Types.RichText ( URL, TeamURLName )

-- | This 'Name' type is the type used in 'brick' to identify various
-- parts of the interface.
data Name =
    MessageInterfaceMessages Name
    -- ^ The rendering of messages for the specified message interface
    -- (by editor name)
    | MessageInput ChannelId
    -- ^ The message editor for the specified channel's main message
    -- interface
    | MessageInputPrompt Name
    -- ^ A wrapper name for reporting the extent of a message editor's
    -- prompt. The specified name is the name of the editor whose prompt
    -- extent is being reported.
    | ChannelListViewport TeamId
    -- ^ The name of the channel list viewport for the specified team.
    | HelpViewport
    -- ^ The name of the viewport for the help interface.
    | PostList
    -- ^ The tag for messages rendered in the post list window.
    | HelpContent HelpScreen
    -- ^ The cache key constructor for caching help screen content.
    | CompletionList Name
    -- ^ The name of the list of completion alternatives in the
    -- specified editor's autocomplete pop-up.
    | JoinChannelList TeamId
    -- ^ The name of the channel list in the "/join" window.
    | UrlList Name
    -- ^ The name of a URL listing for the specified message interface's
    -- editor name.
    | MessagePreviewViewport Name
    -- ^ The name of the message interface editor's preview area.
    | ThemeListSearchInput TeamId
    -- ^ The list of themes in the "/theme" window for the specified
    -- team.
    | UserListSearchInput TeamId
    -- ^ The editor name for the user search input in the specified
    -- team's user list window.
    | JoinChannelListSearchInput TeamId
    -- ^ The editor name for the search input in the specified team's
    -- "/join" window.
    | UserListSearchResults TeamId
    -- ^ The list name for the specified team's user list window search
    -- results.
    | ThemeListSearchResults TeamId
    -- ^ The list name for the specified team's theme list window search
    -- results.
    | ViewMessageArea TeamId
    -- ^ The viewport for the specified team's single-message view
    -- window.
    | ViewMessageReactionsArea TeamId
    -- ^ The viewport for the specified team's single-message view
    -- window's reaction tab.
    | ViewMessageAuthorArea TeamId
    -- ^ The viewport for the specified team's single-message view
    -- window's author info tab.
    | ChannelSidebar TeamId
    -- ^ The cache key for the specified team's channel list viewport
    -- contents.
    | ChannelSelectInput TeamId
    -- ^ The editor name for the specified team's channel selection mode
    -- editor.
    | AttachmentList ChannelId
    -- ^ The name of the attachment list for the specified channel's
    -- message interface.
    | AttachmentFileBrowser ChannelId
    | ReactionEmojiList TeamId
    -- ^ The name of the list of emoji to choose from for reactions for
    -- the specified team.
    | ReactionEmojiListInput TeamId
    -- ^ The name of the search editor for the specified team's emoji
    -- search window.
    | TabbedWindowTabBar TeamId
    -- ^ The name of the specified team's tabbed window tab bar
    -- viewport.
    | MuteToggleField TeamId
    -- ^ The name of the channel preferences mute form field.
    | ChannelMentionsField TeamId
    -- ^ The name of the channel preferences mentions form field.
    | DesktopNotificationsField TeamId (WithDefault NotifyOption)
    -- ^ The name of the channel preferences desktop notifications form
    -- field.
    | PushNotificationsField TeamId (WithDefault NotifyOption)
    -- ^ The name of the channel preferences push notifications form
    -- field.
    | ChannelTopicEditor TeamId
    -- ^ The specified team's channel topic window editor.
    | ChannelTopicSaveButton TeamId
    -- ^ The specified team's channel topic window save button.
    | ChannelTopicCancelButton TeamId
    -- ^ The specified team's channel topic window canel button.
    | ChannelTopicEditorPreview TeamId
    -- ^ The specified team's channel topic window preview area
    -- viewport.
    | ThreadMessageInput ChannelId
    -- ^ The message editor for the specified channel's thread view.
    | ThreadEditorAttachmentList ChannelId
    -- ^ The list name for the specified channel's thread message
    -- interface's attachment list.
    | ChannelTopic ChannelId
    -- ^ The mouse click area tag for a rendered channel topic.
    | TeamList
    -- ^ The viewport name for the team list.
    | ClickableChannelSelectEntry ChannelSelectMatch
    -- ^ The name of a clickable channel select entry in the channel
    -- select match list.
    | ClickableChannelListEntry ChannelId
    -- ^ The name of a clickable entry in the channel list.
    | ClickableTeamListEntry TeamId
    -- ^ The name of a clickable entry in the team list.
    | ClickableURL (Maybe MessageId) Name Int LinkTarget
    -- ^ The name of a clickable URL rendered in RichText. If provided,
    -- the message ID is the ID of the message in which the URL appears.
    -- The integer is the URL index in the rich text block for unique
    -- identification.
    | ClickableReaction PostId Name Text (Set UserId)
    -- ^ The name of a clickable reaction rendered in RichText when it
    -- is part of a message.
    | ClickableAttachmentInMessage Name FileId
    -- ^ The name of a clickable attachment.
    | ClickableUsername (Maybe MessageId) Name Int Text
    -- ^ The name of a clickable username rendered in RichText. The
    -- message ID and integer sequence number uniquely identify the
    -- clickable region.
    | ClickableURLListEntry Int LinkTarget
    -- ^ The name of a clickable URL list entry. The integer is the list
    -- index.
    | ClickableChannelListGroupHeading ChannelListGroupLabel
    -- ^ The name of a clickable channel list group heading.
    | ClickableReactionEmojiListWindowEntry (Bool, T.Text)
    -- ^ The name of a clickable reaction emoji list entry.
    | AttachmentPathEditor Name
    -- ^ The name of the specified message interface's attachment
    -- browser path editor.
    | AttachmentPathSaveButton Name
    -- ^ The name of the specified message interface's attachment
    -- browser save button.
    | AttachmentPathCancelButton Name
    -- ^ The name of the specified message interface's attachment
    -- browser cancel button.
    | RenderedMessage MessageId
    -- ^ The cache key for the rendering of the specified message.
    | SelectedChannelListEntry TeamId
    -- ^ The name of the specified team's currently selected channel
    -- list entry, used to bring the entry into view in its viewport.
    | VScrollBar Brick.ClickableScrollbarElement Name
    -- ^ The name of the scroll bar elements for the specified viewport
    -- name.
    deriving (Eq, Show, Ord)

-- | A match in channel selection mode.
data ChannelSelectMatch =
    ChannelSelectMatch { nameBefore :: Text
                       -- ^ The content of the match before the user's
                       -- matching input.
                       , nameMatched :: Text
                       -- ^ The potion of the name that matched the
                       -- user's input.
                       , nameAfter :: Text
                       -- ^ The portion of the name that came after the
                       -- user's matching input.
                       , matchFull :: Text
                       -- ^ The full string for this entry so it doesn't
                       -- have to be reassembled from the parts above.
                       , matchEntry :: ChannelListEntry
                       -- ^ The original entry data corresponding to the
                       -- text match.
                       }
                       deriving (Eq, Show, Ord)

-- | The type of channel list entries.
data ChannelListEntry =
    ChannelListEntry { channelListEntryChannelId :: ChannelId
                     , channelListEntryType :: ChannelListEntryType
                     , channelListEntryUnread :: Bool
                     , channelListEntrySortValue :: T.Text
                     , channelListEntryFavorite :: Bool
                     , channelListEntryMuted :: Bool
                     }
                     deriving (Eq, Show, Ord)

data ChannelListEntryType =
    CLChannel
    -- ^ A non-DM entry
    | CLUserDM UserId
    -- ^ A single-user DM entry
    | CLGroupDM
    -- ^ A multi-user DM entry
    deriving (Eq, Show, Ord)

-- | The 'HelpScreen' type represents the set of possible 'Help' screens
-- we have to choose from.
data HelpScreen =
    MainHelp
    | ScriptHelp
    | ThemeHelp
    | SyntaxHighlightHelp
    | KeybindingHelp
    deriving (Eq, Show, Ord)

data LinkTarget =
    LinkURL URL
    | LinkFileId FileId
    | LinkPermalink TeamURLName PostId
    deriving (Eq, Show, Ord)

data MessageId = MessagePostId PostId
               | MessageUUID UUID
               deriving (Eq, Read, Ord, Show, Generic, Hashable)

messageIdPostId :: MessageId -> Maybe PostId
messageIdPostId (MessagePostId p) = Just p
messageIdPostId _ = Nothing

data ChannelListGroupLabel =
    ChannelGroupPublicChannels
    | ChannelGroupPrivateChannels
    | ChannelGroupFavoriteChannels
    | ChannelGroupDirectMessages
    deriving (Eq, Ord, Show)

channelListGroupNames :: [(T.Text, ChannelListGroupLabel)]
channelListGroupNames =
    [ ("public", ChannelGroupPublicChannels)
    , ("private", ChannelGroupPrivateChannels)
    , ("favorite", ChannelGroupFavoriteChannels)
    , ("direct", ChannelGroupDirectMessages)
    ]

-- | The state of message selection mode.
data MessageSelectState =
    MessageSelectState { selectMessageId :: Maybe MessageId
                       }

-- | This enum represents all the possible key events a user might
--   want to use.
data KeyEvent
  = VtyRefreshEvent
  | ShowHelpEvent
  | EnterSelectModeEvent
  | ReplyRecentEvent
  | ToggleMessagePreviewEvent
  | InvokeEditorEvent
  | EnterFastSelectModeEvent
  | QuitEvent
  | NextChannelEvent
  | PrevChannelEvent
  | NextChannelEventAlternate
  | PrevChannelEventAlternate
  | NextUnreadChannelEvent
  | NextUnreadUserOrChannelEvent
  | LastChannelEvent
  | EnterOpenURLModeEvent
  | ClearUnreadEvent
  | ToggleMultiLineEvent
  | EnterFlaggedPostsEvent
  | ToggleChannelListVisibleEvent
  | ToggleExpandedChannelTopicsEvent
  | ShowAttachmentListEvent
  | ChangeMessageEditorFocus

  | EditorKillToBolEvent
  | EditorKillToEolEvent
  | EditorBolEvent
  | EditorEolEvent
  | EditorTransposeCharsEvent
  | EditorDeleteCharacter
  | EditorPrevCharEvent
  | EditorNextCharEvent
  | EditorPrevWordEvent
  | EditorNextWordEvent
  | EditorDeleteNextWordEvent
  | EditorDeletePrevWordEvent
  | EditorHomeEvent
  | EditorEndEvent
  | EditorYankEvent

  | CycleChannelListSorting

  | SelectNextTabEvent
  | SelectPreviousTabEvent

  | SaveAttachmentEvent

  -- generic cancel
  | CancelEvent

  -- channel-scroll-specific
  | LoadMoreEvent
  | OpenMessageURLEvent

  -- scrolling events---maybe rebindable?
  | ScrollUpEvent
  | ScrollDownEvent
  | ScrollLeftEvent
  | ScrollRightEvent
  | PageUpEvent
  | PageDownEvent
  | PageRightEvent
  | PageLeftEvent
  | ScrollTopEvent
  | ScrollBottomEvent
  | SelectOldestMessageEvent
  | ChannelListScrollUpEvent
  | ChannelListScrollDownEvent

  -- select events---not the same as scrolling sometimes!
  | SelectUpEvent
  | SelectDownEvent

  -- search select events---these need to not be valid editor inputs
  -- (such as 'j' and 'k')
  | SearchSelectUpEvent
  | SearchSelectDownEvent

  -- E.g. Pressing enter on an item in a list to do something with it
  | ActivateListItemEvent

  | ViewMessageEvent
  | FillGapEvent
  | CopyPostLinkEvent
  | FlagMessageEvent
  | OpenThreadEvent
  | PinMessageEvent
  | YankMessageEvent
  | YankWholeMessageEvent
  | DeleteMessageEvent
  | EditMessageEvent
  | ReplyMessageEvent
  | ReactToMessageEvent
  | OpenMessageInExternalEditorEvent

  -- Attachments
  | AttachmentListAddEvent
  | AttachmentListDeleteEvent
  | AttachmentOpenEvent

  -- Attachment file browser
  | FileBrowserBeginSearchEvent
  | FileBrowserSelectEnterEvent
  | FileBrowserSelectCurrentEvent
  | FileBrowserListPageUpEvent
  | FileBrowserListPageDownEvent
  | FileBrowserListHalfPageUpEvent
  | FileBrowserListHalfPageDownEvent
  | FileBrowserListTopEvent
  | FileBrowserListBottomEvent
  | FileBrowserListNextEvent
  | FileBrowserListPrevEvent


  -- Form submission
  | FormSubmitEvent

  -- Team switching
  | NextTeamEvent
  | PrevTeamEvent
  | MoveCurrentTeamLeftEvent
  | MoveCurrentTeamRightEvent
    deriving (Eq, Show, Ord, Enum)

allEvents :: KeyEvents KeyEvent
allEvents =
    keyEvents
    [ ("quit", QuitEvent)
    , ("vty-refresh", VtyRefreshEvent)
    , ("clear-unread", ClearUnreadEvent)
    , ("cancel", CancelEvent)
    , ("toggle-message-preview", ToggleMessagePreviewEvent)
    , ("invoke-editor", InvokeEditorEvent)
    , ("toggle-multiline", ToggleMultiLineEvent)
    , ("reply-recent", ReplyRecentEvent)
    , ("enter-fast-select", EnterFastSelectModeEvent)
    , ("focus-next-channel", NextChannelEvent)
    , ("focus-prev-channel", PrevChannelEvent)
    , ("focus-next-channel-alternate", NextChannelEventAlternate)
    , ("focus-prev-channel-alternate", PrevChannelEventAlternate)
    , ("focus-next-unread", NextUnreadChannelEvent)
    , ("focus-next-unread-user-or-channel", NextUnreadUserOrChannelEvent)
    , ("focus-last-channel", LastChannelEvent)
    , ("select-next-tab", SelectNextTabEvent)
    , ("select-previous-tab", SelectPreviousTabEvent)
    , ("save-attachment", SaveAttachmentEvent)
    , ("show-attachment-list", ShowAttachmentListEvent)
    , ("change-message-editor-focus", ChangeMessageEditorFocus)
    , ("editor-kill-to-beginning-of-line", EditorKillToBolEvent)
    , ("editor-kill-to-end-of-line", EditorKillToEolEvent)
    , ("editor-beginning-of-line", EditorBolEvent)
    , ("editor-end-of-line", EditorEolEvent)
    , ("editor-transpose-chars", EditorTransposeCharsEvent)
    , ("editor-delete-char", EditorDeleteCharacter)
    , ("editor-prev-char", EditorPrevCharEvent)
    , ("editor-next-char", EditorNextCharEvent)
    , ("editor-prev-word", EditorPrevWordEvent)
    , ("editor-next-word", EditorNextWordEvent)
    , ("editor-delete-next-word", EditorDeleteNextWordEvent)
    , ("editor-delete-prev-word", EditorDeletePrevWordEvent)
    , ("editor-home", EditorHomeEvent)
    , ("editor-end", EditorEndEvent)
    , ("editor-yank", EditorYankEvent)
    , ("cycle-channel-list-sorting", CycleChannelListSorting)
    , ("next-team", NextTeamEvent)
    , ("prev-team", PrevTeamEvent)
    , ("move-current-team-left", MoveCurrentTeamLeftEvent)
    , ("move-current-team-right", MoveCurrentTeamRightEvent)
    , ("show-flagged-posts", EnterFlaggedPostsEvent)
    , ("toggle-channel-list-visibility", ToggleChannelListVisibleEvent)
    , ("toggle-expanded-channel-topics", ToggleExpandedChannelTopicsEvent)
    , ("show-help", ShowHelpEvent)
    , ("select-mode", EnterSelectModeEvent)
    , ("enter-url-open", EnterOpenURLModeEvent)
    , ("load-more", LoadMoreEvent)
    , ("open-message-url", OpenMessageURLEvent)
    , ("scroll-up", ScrollUpEvent)
    , ("scroll-down", ScrollDownEvent)
    , ("scroll-left", ScrollLeftEvent)
    , ("scroll-right", ScrollRightEvent)
    , ("channel-list-scroll-up", ChannelListScrollUpEvent)
    , ("channel-list-scroll-down", ChannelListScrollDownEvent)
    , ("page-up", PageUpEvent)
    , ("page-down", PageDownEvent)
    , ("page-left", PageLeftEvent)
    , ("page-right", PageRightEvent)
    , ("scroll-top", ScrollTopEvent)
    , ("scroll-bottom", ScrollBottomEvent)
    , ("select-oldest-message", SelectOldestMessageEvent)
    , ("select-up", SelectUpEvent)
    , ("select-down", SelectDownEvent)
    , ("search-select-up", SearchSelectUpEvent)
    , ("search-select-down", SearchSelectDownEvent)
    , ("activate-list-item", ActivateListItemEvent)
    , ("open-thread", OpenThreadEvent)
    , ("flag-message", FlagMessageEvent)
    , ("open-message-in-editor", OpenMessageInExternalEditorEvent)
    , ("pin-message", PinMessageEvent)
    , ("view-message", ViewMessageEvent)
    , ("fetch-for-gap", FillGapEvent)
    , ("copy-post-link", CopyPostLinkEvent)
    , ("yank-message", YankMessageEvent)
    , ("yank-whole-message", YankWholeMessageEvent)
    , ("delete-message", DeleteMessageEvent)
    , ("edit-message", EditMessageEvent)
    , ("reply-message", ReplyMessageEvent)
    , ("react-to-message", ReactToMessageEvent)
    , ("add-to-attachment-list", AttachmentListAddEvent)
    , ("delete-from-attachment-list", AttachmentListDeleteEvent)
    , ("open-attachment", AttachmentOpenEvent)
    , ("filebrowser-begin-search", FileBrowserBeginSearchEvent)
    , ("filebrowser-select-file-or-enter-directory", FileBrowserSelectEnterEvent)
    , ("filebrowser-select-current", FileBrowserSelectCurrentEvent)
    , ("filebrowser-list-page-up", FileBrowserListPageUpEvent)
    , ("filebrowser-list-page-down", FileBrowserListPageDownEvent)
    , ("filebrowser-list-half-page-up", FileBrowserListHalfPageUpEvent)
    , ("filebrowser-list-half-page-down", FileBrowserListHalfPageDownEvent)
    , ("filebrowser-list-top", FileBrowserListTopEvent)
    , ("filebrowser-list-bottom", FileBrowserListBottomEvent)
    , ("filebrowser-list-next", FileBrowserListNextEvent)
    , ("filebrowser-list-previous", FileBrowserListPrevEvent)
    , ("submit-form", FormSubmitEvent)
    ]

