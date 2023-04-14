{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE TemplateHaskell #-}
module Matterhorn.Types.Posts
  ( ClientMessage
  , newClientMessage
  , cmDate
  , cmType
  , cmText

  , ClientMessageType(..)

  , Attachment
  , mkAttachment
  , attachmentName
  , attachmentFileId
  , attachmentURL

  , ClientPostType(..)

  , ClientPost
  , toClientPost
  , cpUserOverride
  , cpMarkdownSource
  , cpUser
  , cpText
  , cpType
  , cpReactions
  , cpPending
  , cpOriginalPost
  , cpFromWebhook
  , cpInReplyToPost
  , cpDate
  , cpChannelId
  , cpAttachments
  , cpDeleted
  , cpPostId
  , cpPinned

  , unEmote

  , postIsLeave
  , postIsJoin
  , postIsTopicChange
  , postIsEmote
  )
where

import           Prelude ()
import           Matterhorn.Prelude

import qualified Data.Map.Strict as Map
import qualified Data.Sequence as Seq
import qualified Data.Text as T
import qualified Data.Set as S
import           Data.Time.Clock ( getCurrentTime )
import           Lens.Micro.Platform ( makeLenses )

import           Network.Mattermost.Lenses
import           Network.Mattermost.Types

import           Matterhorn.Types.Common
import           Matterhorn.Types.RichText ( Blocks(..), Block(..)
                                           , TeamBaseURL, Inlines(..), Inline(..)
                                           , ListType(..), ListSpacing(..)
                                           , parseMarkdown, singleB, singleI
                                           )


-- * Client Messages

-- | A 'ClientMessage' is a message given to us by our client,
--   like help text or an error message.
data ClientMessage = ClientMessage
  { _cmText :: Text
  , _cmDate :: ServerTime
  , _cmType :: ClientMessageType
  } deriving (Show)

-- | Create a new 'ClientMessage' value.  This is a message generated
-- by this Matterhorn client and not by (or visible to) the Server.
-- These should be visible, but not necessarily integrated into any
-- special position in the output stream (i.e., they should generally
-- appear at the bottom of the messages display, but subsequent
-- messages should follow them), so this is a special place where
-- there is an assumed approximation of equality between local time
-- and server time.
newClientMessage :: (MonadIO m) => ClientMessageType -> Text -> m ClientMessage
newClientMessage ty msg = do
  now <- liftIO getCurrentTime
  return (ClientMessage msg (ServerTime now) ty)

-- | We format 'ClientMessage' values differently depending on
--   their 'ClientMessageType'
data ClientMessageType =
    Informative
    | Error
    | DateTransition
    | NewMessagesTransition
    | UnknownGapBefore -- ^ a region where the server may have
                       -- messages before the given timestamp that are
                       -- not known locally by this client
    | UnknownGapAfter  -- ^ a region where server may have messages
                       -- after the given timestamp that are not known
                       -- locally by this client
    deriving (Show, Eq)

-- ** 'ClientMessage' Lenses

makeLenses ''ClientMessage

-- * Mattermost Posts

-- | A 'ClientPost' is a temporary internal representation of
--   the Mattermost 'Post' type, with unnecessary information
--   removed and some preprocessing done.
data ClientPost = ClientPost
  { _cpText          :: Blocks
  , _cpMarkdownSource :: Text
  , _cpUser          :: Maybe UserId
  , _cpUserOverride  :: Maybe Text
  , _cpDate          :: ServerTime
  , _cpType          :: ClientPostType
  , _cpPending       :: Bool
  , _cpDeleted       :: Bool
  , _cpAttachments   :: Seq Attachment
  , _cpInReplyToPost :: Maybe PostId
  , _cpPostId        :: PostId
  , _cpChannelId     :: ChannelId
  , _cpReactions     :: Map.Map Text (S.Set UserId)
  , _cpOriginalPost  :: Post
  , _cpFromWebhook   :: Bool
  , _cpPinned        :: Bool
  } deriving (Show)

-- | An attachment has a very long URL associated, as well as
--   an actual file URL
data Attachment = Attachment
  { _attachmentName   :: Text
  , _attachmentURL    :: Text
  , _attachmentFileId :: FileId
  } deriving (Eq, Show)

mkAttachment :: Text -> Text -> FileId -> Attachment
mkAttachment = Attachment

-- | A Mattermost 'Post' value can represent either a normal
--   chat message or one of several special events.
data ClientPostType =
    NormalPost
    | Emote
    | Join
    | Leave
    | TopicChange
    deriving (Eq, Show)

-- ** Creating 'ClientPost' Values

-- | Determine the internal 'PostType' based on a 'Post'
postClientPostType :: Post -> ClientPostType
postClientPostType cp =
    if | postIsEmote cp       -> Emote
       | postIsJoin  cp       -> Join
       | postIsLeave cp       -> Leave
       | postIsTopicChange cp -> TopicChange
       | otherwise            -> NormalPost

-- | Find out whether a 'Post' represents a topic change
postIsTopicChange :: Post -> Bool
postIsTopicChange p = postType p == PostTypeHeaderChange

-- | Find out whether a 'Post' is from a @/me@ command
postIsEmote :: Post -> Bool
postIsEmote p =
    and [ p^.postPropsL.postPropsOverrideIconUrlL == Just (""::Text)
        , ("*" `T.isPrefixOf` (sanitizeUserText $ postMessage p))
        , ("*" `T.isSuffixOf` (sanitizeUserText $ postMessage p))
        ]

-- | Find out whether a 'Post' is a user joining a channel
postIsJoin :: Post -> Bool
postIsJoin p =
  p^.postTypeL == PostTypeJoinChannel

-- | Find out whether a 'Post' is a user leaving a channel
postIsLeave :: Post -> Bool
postIsLeave p =
  p^.postTypeL == PostTypeLeaveChannel

-- | Undo the automatic formatting of posts generated by @/me@-commands
unEmote :: ClientPostType -> Text -> Text
unEmote Emote t = if "*" `T.isPrefixOf` t && "*" `T.isSuffixOf` t
                  then T.init $ T.tail t
                  else t
unEmote _ t = t

-- | Convert a Mattermost 'Post' to a 'ClientPost', passing in a
--   'ParentId' if it has a known one.
toClientPost :: Maybe TeamBaseURL -> Post -> Maybe PostId -> ClientPost
toClientPost baseUrl p parentId =
  let src = unEmote (postClientPostType p) $ sanitizeUserText $ postMessage p
  in ClientPost { _cpText          = parseMarkdown baseUrl src <> getAttachmentText p
                , _cpMarkdownSource = src
                , _cpUser          = postUserId p
                , _cpUserOverride  = p^.postPropsL.postPropsOverrideUsernameL
                , _cpDate          = postCreateAt p
                , _cpType          = postClientPostType p
                , _cpPending       = False
                , _cpDeleted       = False
                , _cpPinned        = fromMaybe False $ postPinned p
                , _cpAttachments   = Seq.empty
                , _cpInReplyToPost = parentId
                , _cpPostId        = p^.postIdL
                , _cpChannelId     = p^.postChannelIdL
                , _cpReactions     = Map.empty
                , _cpOriginalPost  = p
                , _cpFromWebhook   = fromMaybe False $ p^.postPropsL.postPropsFromWebhookL
                }

-- | Right now, instead of treating 'attachment' properties specially, we're
--   just going to roll them directly into the message text
getAttachmentText :: Post -> Blocks
getAttachmentText p =
  case p^.postPropsL.postPropsAttachmentsL of
    Nothing -> mempty
    Just attachments ->
      Blocks $ fmap (Blockquote . render) attachments
  where render att = parseMarkdown Nothing (att^.ppaTextL) <>
                     parseMarkdown Nothing (att^.ppaFallbackL) <>
                     renderAttFields (att^.ppaFieldsL)

-- | Render a bulleted list with any text fields that the post may have
--   attached to it
renderAttFields :: Seq PostPropAttachmentField -> Blocks
renderAttFields fs = singleB $
                     List (BulletList '*') LooseList $
                     fmap renderAttFieldItem fs

-- | Each item will be rendered as the field name in boldface and the value
--   right below it
renderAttFieldItem :: PostPropAttachmentField -> Blocks
renderAttFieldItem f = singleB $ Para $ renderAttFieldItemContent f

renderAttFieldItemContent :: PostPropAttachmentField -> Inlines
renderAttFieldItemContent f = Inlines $ Seq.fromList $
                              renderAttFieldItemName f <>
                              [EText $ ppafValue f]

-- | The field name can sometimes be empty
renderAttFieldItemName :: PostPropAttachmentField -> [Inline]
renderAttFieldItemName f =
  if ppafTitle f == T.empty
  then []
  else [EStrong $ singleI $ EText $ ppafTitle f, ELineBreak]

-- ** 'ClientPost' Lenses

makeLenses ''Attachment
makeLenses ''ClientPost
