{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
module Matterhorn.Types.Core
  ( Name(..)
  , ChannelListEntry(..)
  , ChannelListEntryType(..)
  , ChannelSelectMatch(..)
  , LinkTarget(..)

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
    | MessagePreviewViewport TeamId
    -- ^ The name of the message editor's preview area for the specified
    -- team's current channel.
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
    | ThreadWindowEditorPreview ChannelId
    -- ^ The message preview viewport name for the specified channel's
    -- thread message interface.
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
    | ClickableURL Name Int LinkTarget
    -- ^ The name of a clickable URL rendered in RichText when it is not
    -- part of a message.
    | ClickableURLInMessage Name MessageId Int LinkTarget
    -- ^ The name of a clickable URL rendered in RichText when it is
    -- part of a message.
    | ClickableUsernameInMessage Name MessageId Int Text
    -- ^ The name of a clickable username rendered in RichText when it
    -- is part of a message.
    | ClickableReactionInMessage Name PostId Text (Set UserId)
    -- ^ The name of a clickable reaction rendered in RichText when it
    -- is part of a message.
    | ClickableAttachmentInMessage Name FileId
    -- ^ The name of a clickable attachment.
    | ClickableUsername Name Int Text
    -- ^ The name of a clickable username rendered in RichText when it
    -- is not part of a message.
    | ClickableURLListEntry Int LinkTarget
    -- ^ The name of a clickable URL list entry. The integer is the list
    -- index.
    | ClickableReaction PostId Text (Set UserId)
    -- ^ The name of a clickable reaction rendered in RichText when it
    -- is part of a message.
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
