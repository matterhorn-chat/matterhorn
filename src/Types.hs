{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE RankNTypes #-}

module Types where

import           Prelude ()
import           Prelude.Compat

import           Brick (EventM, txt, Next)
import           Brick.BChan
import           Brick.AttrMap (AttrMap)
import           Brick.Widgets.Edit (Editor, editor)
import           Brick.Widgets.List (List)
import           Cheapskate (Blocks)
import qualified Control.Concurrent.STM as STM
import           Control.Concurrent.MVar (MVar)
import           Control.Exception (SomeException)
import           Data.HashMap.Strict (HashMap)
import           Data.Time.Clock (UTCTime)
import           Data.Time.LocalTime (TimeZone)
import qualified Data.HashMap.Strict as HM
import           Data.List (partition, sort)
import qualified Data.Map.Strict as Map
import           Data.Maybe
import qualified Data.Sequence as Seq
import           Data.Monoid
import qualified Graphics.Vty as Vty
import           Lens.Micro.Platform (at, makeLenses, lens, (&), (^.), (^?), (%~), ix, to, SimpleGetter)
import           Network.Mattermost
import           Network.Mattermost.Exceptions
import           Network.Mattermost.Lenses
import           Network.Mattermost.WebSocket.Types
import           Network.Connection (HostNotResolved, HostCannotConnect)
import qualified Cheapskate as C
import qualified Data.Text as T
import           System.Exit (ExitCode)

import           Zipper (Zipper, focusL)

import           InputHistory

data PasswordSource =
    PasswordString T.Text
    | PasswordCommand T.Text
    deriving (Eq, Read, Show)

data Config = Config
  { configUser           :: Maybe T.Text
  , configHost           :: Maybe T.Text
  , configTeam           :: Maybe T.Text
  , configPort           :: Int
  , configPass           :: Maybe PasswordSource
  , configTimeFormat     :: Maybe T.Text
  , configDateFormat     :: Maybe T.Text
  , configTheme          :: Maybe T.Text
  , configSmartBacktick  :: Bool
  , configURLOpenCommand :: Maybe T.Text
  , configActivityBell   :: Bool
  , configShowMessagePreview :: Bool
  } deriving (Eq, Show)

-- * 'MMNames' structures

-- | The 'MMNames' record is for listing human-readable
--   names and mapping them back to internal IDs.
data MMNames = MMNames
  { _cnChans    :: [T.Text] -- ^ All channel names
  , _cnDMs      :: [T.Text] -- ^ All DM channel names
  , _cnToChanId :: HashMap T.Text ChannelId
      -- ^ Mapping from channel names to 'ChannelId' values
  , _cnUsers    :: [T.Text] -- ^ All users
  , _cnToUserId :: HashMap T.Text UserId
      -- ^ Mapping from user names to 'UserId' values
  }

-- | An empty 'MMNames' record
emptyMMNames :: MMNames
emptyMMNames = MMNames mempty mempty mempty mempty mempty

-- ** 'MMNames' Lenses

makeLenses ''MMNames

-- * Internal Names and References

data Name = ChannelMessages ChannelId
          | MessageInput
          | ChannelList
          | HelpViewport
          | HelpText
          | ChannelSelectString
          | CompletionAlternatives
          | JoinChannelList
          | UrlList
          | MessagePreviewViewport
          deriving (Eq, Show, Ord)

-- | The sum type of exceptions we expect to encounter on authentication
-- failure. We encode them explicitly here so that we can print them in
-- a more user-friendly manner than just 'show'.
data AuthenticationException =
    ConnectError HostCannotConnect
    | ResolveError HostNotResolved
    | LoginError LoginFailureException
    | OtherAuthError SomeException
    deriving (Show)

data ConnectionInfo =
    ConnectionInfo { ciHostname :: T.Text
                   , ciPort     :: Int
                   , ciUsername :: T.Text
                   , ciPassword :: T.Text
                   }

-- | We want to continue referring to posts by their IDs, but we don't want to
-- have to synthesize new valid IDs for messages from the client itself. To
-- that end, a PostRef can be either a PostId or a newly-generated client ID
data PostRef
  = MMId PostId
  | CLId Int
    deriving (Eq, Show)

-- * Client Messages

-- | A 'ClientMessage' is a message given to us by our client,
--   like help text or an error message.
data ClientMessage = ClientMessage
  { _cmText :: T.Text
  , _cmDate :: UTCTime
  , _cmType :: ClientMessageType
  } deriving (Eq, Show)

-- | We format 'ClientMessage' values differently depending on
--   their 'ClientMessageType'
data ClientMessageType =
    Informative
    | Error
    | DateTransition
    | NewMessagesTransition
    deriving (Eq, Show)

-- ** 'ClientMessage' Lenses

makeLenses ''ClientMessage

-- * Mattermost Posts

-- | A 'ClientPost' is a temporary interal representation of
--   the Mattermost 'Post' type, with unnecessary information
--   removed and some preprocessing done.
data ClientPost = ClientPost
  { _cpText          :: Blocks
  , _cpUser          :: Maybe UserId
  , _cpUserOverride  :: Maybe T.Text
  , _cpDate          :: UTCTime
  , _cpType          :: PostType
  , _cpPending       :: Bool
  , _cpDeleted       :: Bool
  , _cpAttachments   :: Seq.Seq Attachment
  , _cpInReplyToPost :: Maybe PostId
  , _cpPostId        :: PostId
  , _cpChannelId     :: ChannelId
  , _cpReactions     :: Map.Map T.Text Int
  , _cpOriginalPost  :: Post
  } deriving (Show)

-- | An attachment has a very long URL associated, as well as
--   an actual file URL
data Attachment = Attachment
  { _attachmentName :: T.Text
  , _attachmentURL  :: T.Text
  } deriving (Eq, Show)

attachmentFromURL :: FileId -> Attachment
attachmentFromURL fId = Attachment
  { _attachmentName = urlForFile fId
  , _attachmentURL  = urlForFile fId
  }

-- | For representing links to things in the 'open links' view
data LinkChoice = LinkChoice
  { _linkTime :: UTCTime
  , _linkUser :: T.Text
  , _linkName :: T.Text
  , _linkURL  :: T.Text
  } deriving (Eq, Show)

-- | A Mattermost 'Post' value can represent either a normal
--   chat message or one of several special events.
data PostType =
    NormalPost
    | Emote
    | Join
    | Leave
    | TopicChange
    deriving (Eq, Show)

-- ** Creating 'ClientPost' Values

-- | Parse text as Markdown and extract the AST
getBlocks :: T.Text -> Blocks
getBlocks s = bs where C.Doc _ bs = C.markdown C.def s

-- | Determine the internal 'PostType' based on a 'Post'
postClientPostType :: Post -> PostType
postClientPostType cp =
    if | postIsEmote cp       -> Emote
       | postIsJoin  cp       -> Join
       | postIsLeave cp       -> Leave
       | postIsTopicChange cp -> TopicChange
       | otherwise            -> NormalPost

-- | Find out whether a 'Post' represents a topic change
postIsTopicChange :: Post -> Bool
postIsTopicChange p = postType p == SystemHeaderChange

-- | Find out whether a 'Post' is from a @/me@ command
postIsEmote :: Post -> Bool
postIsEmote p =
    and [ p^.postPropsL.postPropsOverrideIconUrlL == Just (""::T.Text)
        , p^.postPropsL.postPropsOverrideUsernameL == Just ("webhook"::T.Text)
        , ("*" `T.isPrefixOf` postMessage p)
        , ("*" `T.isSuffixOf` postMessage p)
        ]

-- | Find out whether a 'Post' is a user joining a channel
postIsJoin :: Post -> Bool
postIsJoin p = "has joined the channel" `T.isInfixOf` postMessage p

-- | Find out whether a 'Post' is a user leaving a channel
postIsLeave :: Post -> Bool
postIsLeave p = "has left the channel" `T.isInfixOf` postMessage p

-- | Undo the automatic formatting of posts generated by @/me@-commands
unEmote :: PostType -> T.Text -> T.Text
unEmote Emote t = if "*" `T.isPrefixOf` t && "*" `T.isSuffixOf` t
                  then T.init $ T.tail t
                  else t
unEmote _ t = t

-- | Convert a Mattermost 'Post' to a 'ClientPost', passing in a
--   'ParentId' if it has a known one.
toClientPost :: Post -> Maybe PostId -> ClientPost
toClientPost p parentId = ClientPost
  { _cpText          = (getBlocks $ unEmote (postClientPostType p) $ postMessage p)
                       <> getAttachmentText p
  , _cpUser          = postUserId p
  , _cpUserOverride  = case p^.postPropsL.postPropsOverrideIconUrlL of
      Just _ -> Nothing
      _      -> p^.postPropsL.postPropsOverrideUsernameL
  , _cpDate          = postCreateAt p
  , _cpType          = postClientPostType p
  , _cpPending       = False
  , _cpDeleted       = False
  , _cpAttachments   = Seq.empty
  , _cpInReplyToPost = parentId
  , _cpPostId        = p^.postIdL
  , _cpChannelId     = p^.postChannelIdL
  , _cpReactions     = Map.empty
  , _cpOriginalPost  = p
  }

-- | Right now, instead of treating 'attachment' properties specially, we're
--   just going to roll them directly into the message text
getAttachmentText :: Post -> Blocks
getAttachmentText p =
  case p^.postPropsL.postPropsAttachmentsL of
    Nothing -> Seq.empty
    Just attachments ->
      fmap (C.Blockquote . render) attachments
  where render att = getBlocks (att^.ppaTextL)

-- ** 'ClientPost' Lenses

makeLenses ''Attachment
makeLenses ''ClientPost
makeLenses ''LinkChoice

-- * Messages

-- | A 'Message' is any message we might want to render, either from
--   Mattermost itself or from a client-internal source.
data Message = Message
  { _mText          :: Blocks
  , _mUserName      :: Maybe T.Text
  , _mDate          :: UTCTime
  , _mType          :: MessageType
  , _mPending       :: Bool
  , _mDeleted       :: Bool
  , _mAttachments   :: Seq.Seq Attachment
  , _mInReplyToMsg  :: ReplyState
  , _mPostId        :: Maybe PostId
  , _mReactions     :: Map.Map T.Text Int
  , _mOriginalPost  :: Maybe Post
  } deriving (Show)

-- | A 'Message' is the representation we use for storage and
--   rendering, so it must be able to represent either a
--   post from Mattermost or an internal message. This represents
--   the union of both kinds of post types.
data MessageType = C ClientMessageType
                 | CP PostType
                 deriving (Eq, Show)

-- | The 'ReplyState' of a message represents whether a message
--   is a reply, and if so, to what message
data ReplyState =
    NotAReply
    | ParentLoaded PostId Message
    | ParentNotLoaded PostId
    deriving (Show)

-- | Convert a 'ClientMessage' to a 'Message'
clientMessageToMessage :: ClientMessage -> Message
clientMessageToMessage cm = Message
  { _mText          = getBlocks (_cmText cm)
  , _mUserName      = Nothing
  , _mDate          = _cmDate cm
  , _mType          = C $ _cmType cm
  , _mPending       = False
  , _mDeleted       = False
  , _mAttachments   = Seq.empty
  , _mInReplyToMsg  = NotAReply
  , _mPostId        = Nothing
  , _mReactions     = Map.empty
  , _mOriginalPost  = Nothing
  }

-- ** 'Message' Lenses

makeLenses ''Message

-- * Channel representations

-- | A 'ClientChannel' contains both the message
--   listing and the metadata about a channel
data ClientChannel = ClientChannel
  { _ccContents :: ChannelContents
    -- ^ A list of 'Message's in the channel
  , _ccInfo     :: ChannelInfo
    -- ^ The 'ChannelInfo' for the channel
  }

initialChannelInfo :: Channel -> ChannelInfo
initialChannelInfo chan =
    let updated  = chan ^. channelLastPostAtL
    in ChannelInfo { _cdViewed           = updated
                   , _cdUpdated          = updated
                   , _cdName             = chan^.channelNameL
                   , _cdHeader           = chan^.channelHeaderL
                   , _cdType             = chan^.channelTypeL
                   , _cdCurrentState     = ChanUnloaded
                   , _cdNewMessageCutoff = Nothing
                   }

channelInfoFromChannelWithData :: ChannelWithData -> ChannelInfo -> ChannelInfo
channelInfoFromChannelWithData (ChannelWithData chan chanData) ci =
    let viewed   = chanData ^. channelDataLastViewedAtL
        updated  = chan ^. channelLastPostAtL
    in ci { _cdViewed           = viewed
          , _cdUpdated          = updated
          , _cdName             = (chan^.channelNameL)
          , _cdHeader           = (chan^.channelHeaderL)
          , _cdType             = (chan^.channelTypeL)
          }

-- | The 'ChannelContents' is a wrapper for a list of
--   'Message' values
data ChannelContents = ChannelContents
  { _cdMessages :: Seq.Seq Message
  }

-- | An initial empty 'ChannelContents' value
emptyChannelContents :: ChannelContents
emptyChannelContents = ChannelContents
  { _cdMessages = mempty
  }

-- | The 'ChannelState' represents our internal state
--   of the channel with respect to our knowledge (or
--   lack thereof) about the server's information
--   about the channel.
data ChannelState
  = ChanUnloaded
  | ChanLoaded
  | ChanLoadPending
  | ChanRefreshing
    deriving (Eq, Show)

-- | The 'ChannelInfo' record represents metadata
--   about a channel
data ChannelInfo = ChannelInfo
  { _cdViewed           :: UTCTime
    -- ^ The last time we looked at a channel
  , _cdUpdated          :: UTCTime
    -- ^ The last time a message showed up in the channel
  , _cdName             :: T.Text
    -- ^ The name of the channel
  , _cdHeader           :: T.Text
    -- ^ The header text of a channel
  , _cdType             :: Type
    -- ^ The type of a channel: public, private, or DM
  , _cdCurrentState     :: ChannelState
    -- ^ The current state of the channel
  , _cdNewMessageCutoff :: Maybe UTCTime
    -- ^ The last time we looked at the new messages in
    --   this channel, if ever
  }

-- ** Channel-matching types

data ChannelSelectMatch =
    ChannelSelectMatch { nameBefore     :: T.Text
                       , nameMatched    :: T.Text
                       , nameAfter      :: T.Text
                       }
                       deriving (Eq, Show)

channelNameFromMatch :: ChannelSelectMatch -> T.Text
channelNameFromMatch (ChannelSelectMatch b m a) = b <> m <> a

data ChannelSelectPattern = CSP MatchType T.Text
                          deriving (Eq, Show)

data MatchType = Prefix | Suffix | Infix | Equal deriving (Eq, Show)


-- ** Channel-related Lenses

makeLenses ''ChannelContents
makeLenses ''ChannelInfo
makeLenses ''ClientChannel

-- * 'UserInfo' Values

-- | A 'UserInfo' value represents everything we need to know at
--   runtime about a user
data UserInfo = UserInfo
  { _uiName   :: T.Text
  , _uiId     :: UserId
  , _uiStatus :: UserStatus
  , _uiInTeam :: Bool
  } deriving (Eq, Show)

-- | Create a 'UserInfo' value from a Mattermost 'User' value
userInfoFromUser :: User -> Bool -> UserInfo
userInfoFromUser up inTeam = UserInfo
  { _uiName   = userUsername up
  , _uiId     = userId up
  , _uiStatus = Offline
  , _uiInTeam = inTeam
  }

-- | The 'UserStatus' value represents possible current status for
--   a user
data UserStatus
  = Online
  | Away
  | Offline
  | Other T.Text
    deriving (Eq, Show)

statusFromText :: T.Text -> UserStatus
statusFromText t = case t of
  "online"  -> Online
  "offline" -> Offline
  "away"    -> Away
  _         -> Other t

-- ** 'UserInfo' lenses

makeLenses ''UserInfo

instance Ord UserInfo where
  u1 `compare` u2
    | u1^.uiStatus == Offline && u2^.uiStatus /= Offline =
      GT
    | u1^.uiStatus /= Offline && u2^.uiStatus == Offline =
      LT
    | otherwise =
      (u1^.uiName) `compare` (u2^.uiName)

-- * Application State Values

data ProgramOutput =
    ProgramOutput { program :: FilePath
                  , programArgs :: [String]
                  , programStdout :: String
                  , programStderr :: String
                  , programExitCode :: ExitCode
                  }

-- | 'ChatResources' represents configuration and
-- connection-related information, as opposed to
-- current model or view information. Information
-- that goes in the 'ChatResources' value should be
-- limited to information that we read or set up
-- prior to setting up the bulk of the application state.
data ChatResources = ChatResources
  { _crTok           :: Token
  , _crConn          :: ConnectionData
  , _crRequestQueue  :: RequestChan
  , _crEventQueue    :: BChan MHEvent
  , _crSubprocessLog :: STM.TChan ProgramOutput
  , _crTheme         :: AttrMap
  , _crQuitCondition :: MVar ()
  , _crConfiguration :: Config
  }

-- | The 'ChatEditState' value contains the editor widget itself
--   as well as history and metadata we need for editing-related
--   operations.
data ChatEditState = ChatEditState
  { _cedEditor               :: Editor T.Text Name
  , _cedEditMode             :: EditMode
  , _cedMultiline            :: Bool
  , _cedInputHistory         :: InputHistory
  , _cedInputHistoryPosition :: HM.HashMap ChannelId (Maybe Int)
  , _cedLastChannelInput     :: HM.HashMap ChannelId (T.Text, EditMode)
  , _cedCurrentCompletion    :: Maybe T.Text
  , _cedCurrentAlternative   :: T.Text
  , _cedCompletionAlternatives :: [T.Text]
  , _cedYankBuffer           :: T.Text
  }

data EditMode =
    NewPost
    | Editing Post
    | Replying Message Post

-- | We can initialize a new 'ChatEditState' value with just an
--   edit history, which we save locally.
emptyEditState :: InputHistory -> ChatEditState
emptyEditState hist = ChatEditState
  { _cedEditor               = editor MessageInput (txt . T.unlines) Nothing ""
  , _cedMultiline            = False
  , _cedInputHistory         = hist
  , _cedInputHistoryPosition = mempty
  , _cedLastChannelInput     = mempty
  , _cedCurrentCompletion    = Nothing
  , _cedCompletionAlternatives = []
  , _cedCurrentAlternative   = ""
  , _cedEditMode             = NewPost
  , _cedYankBuffer           = ""
  }

-- | A 'RequestChan' is a queue of operations we have to perform
--   in the background to avoid blocking on the main loop
type RequestChan = STM.TChan (IO (ChatState -> EventM Name ChatState))

-- | The 'HelpScreen' type represents the set of possible 'Help'
--   dialogues we have to choose from.
data HelpScreen
  = MainHelp
  | ScriptHelp
    deriving (Eq)

-- | The 'Mode' represents the current dominant UI activity
data Mode =
    Main
    | ShowHelp HelpScreen
    | ChannelSelect
    | UrlSelect
    | LeaveChannelConfirm
    | DeleteChannelConfirm
    | JoinChannel
    | ChannelScroll
    | MessageSelect
    | MessageSelectDeleteConfirm
    deriving (Eq)

-- | We're either connected or we're not. yeop
data ConnectionStatus = Connected | Disconnected

-- | This is the giant bundle of fields that represents the current
--  state of our application at any given time. Some of this should
--  be broken out further, but hasn't yet been.
data ChatState = ChatState
  { _csResources                   :: ChatResources
  , _csFocus                       :: Zipper ChannelId
  , _csNames                       :: MMNames
  , _csMe                          :: User
  , _csMyTeam                      :: Team
  , _msgMap                        :: HashMap ChannelId ClientChannel
  , _csPostMap                     :: HashMap PostId Message
  , _usrMap                        :: HashMap UserId UserInfo
  , _timeZone                      :: TimeZone
  , _csEditState                   :: ChatEditState
  , _csMode                        :: Mode
  , _csShowMessagePreview          :: Bool
  , _csChannelSelectString         :: T.Text
  , _csChannelSelectChannelMatches :: HashMap T.Text ChannelSelectMatch
  , _csChannelSelectUserMatches    :: HashMap T.Text ChannelSelectMatch
  , _csRecentChannel               :: Maybe ChannelId
  , _csUrlList                     :: List Name LinkChoice
  , _csConnectionStatus            :: ConnectionStatus
  , _csJoinChannelList             :: Maybe (List Name Channel)
  , _csMessageSelect               :: MessageSelectState
  }

data MessageSelectState =
    MessageSelectState { selectMessagePostId :: Maybe PostId
                       }

-- | This represents any event that we might care about in the
--   main application loop
data MHEvent
  = WSEvent WebsocketEvent
    -- ^ For events that arise from the websocket
  | RespEvent (ChatState -> EventM Name ChatState)
    -- ^ For the result values of async IO operations
  | AsyncErrEvent SomeException
    -- ^ For errors that arise in the course of async IO operations
  | RefreshWebsocketEvent
    -- ^ Tell our main loop to refresh the websocket connection
  | WebsocketDisconnect
  | WebsocketConnect

-- ** Application State Lenses

makeLenses ''ChatResources
makeLenses ''ChatState
makeLenses ''ChatEditState

-- ** Utility Lenses
csCurrentChannelId :: Lens' ChatState ChannelId
csCurrentChannelId = csFocus.focusL

csCurrentChannel :: Lens' ChatState ClientChannel
csCurrentChannel =
  lens (\ st -> (st^.msgMap) HM.! (st^.csCurrentChannelId))
       (\ st n -> st & msgMap %~ HM.insert (st^.csCurrentChannelId) n)

csChannel :: ChannelId -> Lens' ChatState ClientChannel
csChannel cId =
  lens (\ st -> (st^.msgMap) HM.! cId)
       (\ st n -> st & msgMap %~ HM.insert cId n)

csUser :: UserId -> Lens' ChatState UserInfo
csUser uId =
  lens (\ st -> (st^.usrMap) HM.! uId)
       (\ st n -> st & usrMap %~ HM.insert uId n)

-- ** Interim lenses for backwards compat

csTheme :: Lens' ChatState AttrMap
csTheme = csResources . crTheme

csTok :: Lens' ChatState Token
csTok = csResources . crTok

csConn :: Lens' ChatState ConnectionData
csConn = csResources . crConn

csRequestQueue :: Lens' ChatState RequestChan
csRequestQueue = csResources . crRequestQueue

cmdLine :: Lens' ChatState (Editor T.Text Name)
cmdLine = csEditState . cedEditor

csInputHistory :: Lens' ChatState InputHistory
csInputHistory = csEditState . cedInputHistory

csInputHistoryPosition :: Lens' ChatState (HM.HashMap ChannelId (Maybe Int))
csInputHistoryPosition = csEditState . cedInputHistoryPosition

csLastChannelInput :: Lens' ChatState (HM.HashMap ChannelId (T.Text, EditMode))
csLastChannelInput = csEditState . cedLastChannelInput

csCurrentCompletion :: Lens' ChatState (Maybe T.Text)
csCurrentCompletion = csEditState . cedCurrentCompletion

timeFormat :: SimpleGetter ChatState (Maybe T.Text)
timeFormat = csResources . crConfiguration . to configTimeFormat

dateFormat :: SimpleGetter ChatState (Maybe T.Text)
dateFormat = csResources . crConfiguration . to configDateFormat

-- ** 'ChatState' Helper Functions

getMessageForPostId :: ChatState -> PostId -> ReplyState
getMessageForPostId st pId =
    case st^.csPostMap.at(pId) of
        Nothing -> ParentNotLoaded pId
        Just m -> ParentLoaded pId m

getUsernameForUserId :: ChatState -> UserId -> Maybe T.Text
getUsernameForUserId st uId = st^.usrMap ^? ix uId.uiName

clientPostToMessage :: ChatState -> ClientPost -> Message
clientPostToMessage st cp = Message
  { _mText          = _cpText cp
  , _mUserName      = case _cpUserOverride cp of
    Just n
      | _cpType cp == NormalPost -> Just (n <> "[BOT]")
    _ -> getUsernameForUserId st =<< _cpUser cp
  , _mDate          = _cpDate cp
  , _mType          = CP $ _cpType cp
  , _mPending       = _cpPending cp
  , _mDeleted       = _cpDeleted cp
  , _mAttachments   = _cpAttachments cp
  , _mInReplyToMsg  =
    case cp^.cpInReplyToPost of
      Nothing  -> NotAReply
      Just pId -> getMessageForPostId st pId
  , _mPostId        = Just $ cp^.cpPostId
  , _mReactions     = _cpReactions cp
  , _mOriginalPost  = Just $ cp^.cpOriginalPost
  }

-- * Slash Commands

-- | The 'CmdArgs' type represents the arguments to a slash-command;
--   the type parameter represents the argument structure.
data CmdArgs :: * -> * where
  NoArg    :: CmdArgs ()
  LineArg  :: T.Text -> CmdArgs T.Text
  TokenArg :: T.Text -> CmdArgs rest -> CmdArgs (T.Text, rest)

-- | A 'CmdExec' value represents the implementation of a command
--   when provided with its arguments
type CmdExec a = a -> ChatState -> EventM Name (Next ChatState)

-- | A 'Cmd' packages up a 'CmdArgs' specifier and the 'CmdExec'
--   implementation with a name and a description.
data Cmd = forall a. Cmd
  { cmdName    :: T.Text
  , cmdDescr   :: T.Text
  , cmdArgSpec :: CmdArgs a
  , cmdAction  :: CmdExec a
  }

-- | Helper function to extract the name out of a 'Cmd' value
commandName :: Cmd -> T.Text
commandName (Cmd name _ _ _ ) = name

-- * Keybindings

-- | A 'Keybinding' represents a keybinding along with its
--   implementation
data Keybinding =
    KB { kbDescription :: T.Text
       , kbEvent :: Vty.Event
       , kbAction :: ChatState -> EventM Name (Next ChatState)
       }

-- | Find a keybinding that matches a Vty Event
lookupKeybinding :: Vty.Event -> [Keybinding] -> Maybe Keybinding
lookupKeybinding e kbs = listToMaybe $ filter ((== e) . kbEvent) kbs

sortedUserList :: ChatState -> [UserInfo]
sortedUserList st = sort yes ++ sort no
  where userList = filter showUser (HM.elems(st^.usrMap))
        showUser u = not (isSelf u) && (u^.uiInTeam)
        isSelf u = (st^.csMe.userIdL) == (u^.uiId)
        hasUnread u =
          case st^.csNames.cnToChanId.at(u^.uiName) of
            Nothing  -> False
            Just cId
              | (st^.csCurrentChannelId) == cId -> False
              | otherwise ->
                  let info = st^.csChannel(cId).ccInfo
                  in info^.cdUpdated > info^.cdViewed
        (yes, no) = partition hasUnread userList
