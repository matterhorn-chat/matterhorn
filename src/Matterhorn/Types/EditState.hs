{-# LANGUAGE TemplateHaskell #-}
module Matterhorn.Types.EditState
  ( EditMode(..)
  , AttachmentData(..)
  , AutocompletionType(..)

  , CompletionSource(..)
  , SpecialMention(..)
  , specialMentionName
  , isSpecialMention

  , EditState(..)
  , newEditState
  , unsafeEsFileBrowser
  , esAttachmentList
  , esFileBrowser
  , esMisspellings
  , esEditMode
  , esEphemeral
  , esEditor
  , esAutocomplete
  , esAutocompletePending
  , esResetEditMode
  , esJustCompleted
  , esShowReplyPrompt

  , EphemeralEditState(..)
  , defaultEphemeralEditState
  , eesMultiline
  , eesInputHistoryPosition
  , eesLastInput
  , eesTypingUsers
  , addEphemeralStateTypingUser

  , AutocompleteState(..)
  , acPreviousSearchString
  , acCompletionList
  , acCachedResponses
  , acType

  , AutocompleteAlternative(..)
  , autocompleteAlternativeReplacement
  )
where

import           Prelude ()
import           Matterhorn.Prelude

import           Brick.Widgets.Edit ( Editor, editor )
import           Brick.Widgets.List ( List, list )
import qualified Brick.Widgets.FileBrowser as FB
import qualified Data.ByteString as BS
import qualified Data.HashMap.Strict as HM
import qualified Data.Text as T
import           Lens.Micro.Platform ( Lens', makeLenses, (.~), (^?!), lens, _Just
                                     , (%~) )
import           Network.Mattermost.Types

import           Matterhorn.Types.Common
import           Matterhorn.Types.Messages ( Message, MessageType )
import           Matterhorn.Types.Users ( TypingUsers, noTypingUsers, addTypingUser
                                        , addUserSigil, trimUserSigil )
import           Matterhorn.Constants


-- | A "special" mention that does not map to a specific user, but is an
-- alias that the server uses to notify users.
data SpecialMention =
    MentionAll
    -- ^ @all: notify everyone in the channel.
    | MentionChannel
    -- ^ @channel: notify everyone in the channel.

data AutocompleteAlternative =
    UserCompletion User Bool
    -- ^ User, plus whether the user is in the channel that triggered
    -- the autocomplete
    | SpecialMention SpecialMention
    -- ^ A special mention.
    | ChannelCompletion Bool Channel
    -- ^ Channel, plus whether the user is a member of the channel
    | SyntaxCompletion Text
    -- ^ Name of a skylighting syntax definition
    | CommandCompletion CompletionSource Text Text Text
    -- ^ Source, name of a slash command, argspec, and description
    | EmojiCompletion Text
    -- ^ The text of an emoji completion

-- | The source of an autocompletion alternative.
data CompletionSource = Server | Client
                      deriving (Eq, Show)

specialMentionName :: SpecialMention -> Text
specialMentionName MentionChannel = "channel"
specialMentionName MentionAll = "all"

isSpecialMention :: T.Text -> Bool
isSpecialMention n = isJust $ lookup (T.toLower $ trimUserSigil n) pairs
    where
        pairs = mkPair <$> mentions
        mentions = [ MentionChannel
                   , MentionAll
                   ]
        mkPair v = (specialMentionName v, v)

autocompleteAlternativeReplacement :: AutocompleteAlternative -> Text
autocompleteAlternativeReplacement (EmojiCompletion e) =
    ":" <> e <> ":"
autocompleteAlternativeReplacement (SpecialMention m) =
    addUserSigil $ specialMentionName m
autocompleteAlternativeReplacement (UserCompletion u _) =
    addUserSigil $ userUsername u
autocompleteAlternativeReplacement (ChannelCompletion _ c) =
    normalChannelSigil <> (sanitizeUserText $ channelName c)
autocompleteAlternativeReplacement (SyntaxCompletion t) =
    "```" <> t
autocompleteAlternativeReplacement (CommandCompletion _ t _ _) =
    "/" <> t

-- | The type of data that the autocompletion logic supports. We use
-- this to track the kind of completion underway in case the type of
-- completion needs to change.
data AutocompletionType =
    ACUsers
    | ACChannels
    | ACCodeBlockLanguage
    | ACEmoji
    | ACCommands
    deriving (Eq, Show)

-- | An attachment.
data AttachmentData =
    AttachmentData { attachmentDataFileInfo :: FB.FileInfo
                   , attachmentDataBytes :: BS.ByteString
                   }
                   deriving (Eq, Show)

-- | The input state associated with the message editor.
data EditMode =
    NewPost
    -- ^ The input is for a new post.
    | Editing Post MessageType
    -- ^ The input is ultimately to replace the body of an existing post
    -- of the specified type.
    | Replying Message Post
    -- ^ The input is to be used as a new post in reply to the specified
    -- post.
    deriving (Show)

data AutocompleteState n =
    AutocompleteState { _acPreviousSearchString :: Text
                      -- ^ The search string used for the
                      -- currently-displayed autocomplete results, for
                      -- use in deciding whether to issue another server
                      -- query
                      , _acCompletionList :: List n AutocompleteAlternative
                      -- ^ The list of alternatives that the user
                      -- selects from
                      , _acType :: AutocompletionType
                      -- ^ The type of data that we're completing
                      , _acCachedResponses :: HM.HashMap Text [AutocompleteAlternative]
                      -- ^ A cache of alternative lists, keyed on search
                      -- string, for use in avoiding server requests.
                      -- The idea here is that users type quickly enough
                      -- (and edit their input) that would normally lead
                      -- to rapid consecutive requests, some for the
                      -- same strings during editing, that we can avoid
                      -- that by caching them here. Note that this cache
                      -- gets destroyed whenever autocompletion is not
                      -- on, so this cache does not live very long.
                      }

-- | The 'EditState' value contains the editor widget itself as well as
-- history and metadata we need for editing-related operations.
data EditState n =
    EditState { _esEditor :: Editor Text n
              , _esEditMode :: EditMode
              , _esEphemeral :: EphemeralEditState
              , _esMisspellings :: Set Text
              , _esAutocomplete :: Maybe (AutocompleteState n)
              -- ^ The autocomplete state. The autocompletion UI is
              -- showing only when this state is present.
              , _esResetEditMode :: EditMode
              -- ^ The editing mode to reset to after input is handled.
              , _esAutocompletePending :: Maybe Text
              -- ^ The search string associated with the latest
              -- in-flight autocompletion request. This is used to
              -- determine whether any (potentially late-arriving) API
              -- responses are for stale queries since the user can type
              -- more quickly than the server can get us the results,
              -- and we wouldn't want to show results associated with
              -- old editor states.
              , _esAttachmentList :: List n AttachmentData
              -- ^ The list of attachments to be uploaded with the post
              -- being edited.
              , _esFileBrowser :: Maybe (FB.FileBrowser n)
              -- ^ The browser for selecting attachment files. This is
              -- a Maybe because the instantiation of the FileBrowser
              -- causes it to read and ingest the target directory, so
              -- this action is deferred until the browser is needed.
              , _esJustCompleted :: Bool
              -- A flag that indicates whether the most recent editing
              -- event was a tab-completion. This is used by the smart
              -- trailing space handling.
              , _esShowReplyPrompt :: Bool
              -- ^ Whether to show the reply prompt when replying
              }

newEditState :: n -> n -> EditMode -> Bool -> EditState n
newEditState editorName attachmentListName initialEditMode showReplyPrompt =
    EditState { _esEditor               = editor editorName Nothing ""
              , _esEphemeral            = defaultEphemeralEditState
              , _esEditMode             = initialEditMode
              , _esResetEditMode        = initialEditMode
              , _esMisspellings         = mempty
              , _esAutocomplete         = Nothing
              , _esAutocompletePending  = Nothing
              , _esAttachmentList       = list attachmentListName mempty 1
              , _esFileBrowser          = Nothing
              , _esJustCompleted        = False
              , _esShowReplyPrompt      = showReplyPrompt
              }

data EphemeralEditState =
    EphemeralEditState { _eesMultiline :: Bool
                       -- ^ Whether the editor is in multiline mode
                       , _eesInputHistoryPosition :: Maybe Int
                       -- ^ The input history position, if any
                       , _eesLastInput :: (T.Text, EditMode)
                       -- ^ The input entered into the text editor last
                       -- time the user was focused on the channel
                       -- associated with this state.
                       , _eesTypingUsers :: TypingUsers
                       }

defaultEphemeralEditState :: EphemeralEditState
defaultEphemeralEditState =
    EphemeralEditState { _eesMultiline = False
                       , _eesInputHistoryPosition = Nothing
                       , _eesLastInput = ("", NewPost)
                       , _eesTypingUsers = noTypingUsers
                       }

makeLenses ''EphemeralEditState

-- | Add user to the list of users in this state who are currently typing.
addEphemeralStateTypingUser :: UserId -> UTCTime -> EphemeralEditState -> EphemeralEditState
addEphemeralStateTypingUser uId ts = eesTypingUsers %~ (addTypingUser uId ts)

makeLenses ''EditState
makeLenses ''AutocompleteState

unsafeEsFileBrowser :: Lens' (EditState n) (FB.FileBrowser n)
unsafeEsFileBrowser =
     lens (\st   -> st^.esFileBrowser ^?! _Just)
          (\st t -> st & esFileBrowser .~ Just t)
