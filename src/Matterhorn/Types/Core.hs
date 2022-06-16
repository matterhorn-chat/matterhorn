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
    ChannelMessages ChannelId
    | MessageInput ChannelId
    | MessageInputPrompt Name
    | ChannelListViewport TeamId
    | HelpViewport
    | PostList
    | HelpContent HelpScreen
    | ChannelSelectString TeamId
    | CompletionAlternatives TeamId
    | CompletionList ChannelId
    | JoinChannelList TeamId
    | UrlList ChannelId (Maybe PostId)
    | MessagePreviewViewport TeamId
    | ThemeListSearchInput TeamId
    | UserListSearchInput TeamId
    | JoinChannelListSearchInput TeamId
    | UserListSearchResults TeamId
    | ThemeListSearchResults TeamId
    | ViewMessageArea TeamId
    | ViewMessageReactionsArea TeamId
    | ChannelSidebar TeamId
    | ChannelSelectInput TeamId
    | AttachmentList ChannelId
    | AttachmentFileBrowser ChannelId
    | MessageReactionsArea TeamId
    | ReactionEmojiList TeamId
    | ReactionEmojiListInput TeamId
    | TabbedWindowTabBar TeamId
    | MuteToggleField TeamId
    | ChannelMentionsField TeamId
    | DesktopNotificationsField TeamId (WithDefault NotifyOption)
    | PushNotificationsField TeamId (WithDefault NotifyOption)
    | ChannelTopicEditor TeamId
    | ChannelTopicSaveButton TeamId
    | ChannelTopicCancelButton TeamId
    | ChannelTopicEditorPreview TeamId
    | ThreadMessageInput ChannelId
    | ThreadWindowEditorPreview ChannelId
    | ThreadEditorAttachmentList ChannelId
    | ThreadWindowMessages ChannelId
    | ChannelTopic ChannelId
    | TeamList
    | ClickableChannelSelectEntry ChannelSelectMatch
    | ClickableChannelListEntry ChannelId
    | ClickableTeamListEntry TeamId
    | ClickableURL Name Int LinkTarget
    | ClickableURLInMessage Name MessageId Int LinkTarget
    | ClickableUsernameInMessage Name MessageId Int Text
    | ClickableReactionInMessage Name PostId Text (Set UserId)
    | ClickableAttachmentInMessage Name FileId
    | ClickableUsername Name Int Text
    | ClickableURLListEntry Int LinkTarget
    | ClickableReaction PostId Text (Set UserId)
    | ClickableChannelListGroupHeading ChannelListGroupLabel
    | AttachmentPathEditor Name
    | AttachmentPathSaveButton Name
    | AttachmentPathCancelButton Name
    | RenderedMessage MessageId
    | ReactionEmojiListWindowEntry (Bool, T.Text)
    | SelectedChannelListEntry TeamId
    | VScrollBar Brick.ClickableScrollbarElement Name
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
