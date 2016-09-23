{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiWayIf #-}

module Types where

import           Brick (EventM, txt, vBox, Next)
import           Brick.AttrMap (AttrMap)
import           Brick.Widgets.Edit (Editor, editor)
import           Brick.Widgets.List (List)
import           Cheapskate (Blocks)
import           Control.Concurrent.Chan (Chan)
import           Control.Concurrent.MVar (MVar)
import           Data.HashMap.Strict (HashMap)
import           Data.Time.Clock (UTCTime)
import           Data.Time.LocalTime (TimeZone)
import qualified Data.HashMap.Strict as HM
import qualified Data.Sequence as Seq
import qualified Graphics.Vty as Vty
import           Lens.Micro.Platform (makeLenses, (^.), (^?), ix)
import           Network.Mattermost
import           Network.Mattermost.Lenses
import           Network.Mattermost.WebSocket.Types
import qualified Cheapskate as C
import qualified Data.Text as T

import           Zipper (Zipper)

import           InputHistory

data MMNames = MMNames
  { _cnChans    :: [T.Text]
  , _cnDMs      :: [T.Text]
  , _cnToChanId :: HashMap T.Text ChannelId
  , _cnUsers    :: [T.Text]
  , _cnToUserId :: HashMap T.Text UserId
  }

emptyMMNames :: MMNames
emptyMMNames = MMNames mempty mempty mempty mempty mempty

makeLenses ''MMNames

data Name = ChannelMessages ChannelId
          | MessageInput
          | NormalChannelList
          | DMChannelList
          | HelpViewport
          | ChannelSelectString
          | CompletionAlternatives
          | JoinChannelList
          deriving (Eq, Show, Ord)

-- We want to continue referring to posts by their IDs, but we don't want to
-- have to synthesize new valid IDs for messages from the client itself. To
-- that end, a PostRef can be either a PostId or a newly-generated client ID
data PostRef
  = MMId PostId
  | CLId Int
    deriving (Eq, Show)

data ClientMessageType =
    Informative
    | Error
    | DateTransition
    deriving (Eq, Show)

-- A ClientMessage is a message given to us by our client, like help text
-- or an error message.
data ClientMessage = ClientMessage
  { _cmText :: T.Text
  , _cmDate :: UTCTime
  , _cmType :: ClientMessageType
  } deriving (Eq, Show)

makeLenses ''ClientMessage

data PostType =
    NormalPost
    | Emote
    | Join
    | Leave
    | TopicChange
    deriving (Eq, Show)

data ClientPost = ClientPost
  { _cpText        :: Blocks
  , _cpUser        :: UserId
  , _cpDate        :: UTCTime
  , _cpType        :: PostType
  , _cpPending     :: Bool
  , _cpDeleted     :: Bool
  , _cpAttachments :: Seq.Seq T.Text
  , _cpPostId      :: PostId
  } deriving (Show)

makeLenses ''ClientPost

data MessageType = C ClientMessageType
                 | CP PostType
                 deriving (Eq, Show)

-- This represents any message we might want to render.
data Message = Message
  { _mText        :: Blocks
  , _mUserName    :: Maybe T.Text
  , _mDate        :: UTCTime
  , _mType        :: MessageType
  , _mPending     :: Bool
  , _mDeleted     :: Bool
  , _mAttachments :: Seq.Seq T.Text
  , _mPostId      :: Maybe PostId
  } deriving (Show)

makeLenses ''Message

getBlocks :: T.Text -> Blocks
getBlocks s = bs where C.Doc _ bs = C.markdown C.def s

postClientPostType :: Post -> PostType
postClientPostType cp =
    if | postIsEmote cp       -> Emote
       | postIsJoin  cp       -> Join
       | postIsLeave cp       -> Leave
       | postIsTopicChange cp -> TopicChange
       | otherwise            -> NormalPost

postIsTopicChange :: Post -> Bool
postIsTopicChange p =
    "updated the channel header from:" `T.isInfixOf` postMessage p

postIsEmote :: Post -> Bool
postIsEmote p =
    and [ HM.lookup "override_icon_url" (postProps p) == Just (""::T.Text)
        , HM.lookup "override_username" (postProps p) == Just ("webhook"::T.Text)
        , ("*" `T.isPrefixOf` postMessage p)
        , ("*" `T.isSuffixOf` postMessage p)
        ]

postIsJoin :: Post -> Bool
postIsJoin p = "has joined the channel" `T.isInfixOf` postMessage p

postIsLeave :: Post -> Bool
postIsLeave p = "has left the channel" `T.isInfixOf` postMessage p

unEmote :: PostType -> T.Text -> T.Text
unEmote Emote t = if "*" `T.isPrefixOf` t && "*" `T.isSuffixOf` t
                  then T.init $ T.tail t
                  else t
unEmote _ t = t

toClientPost :: Post -> ClientPost
toClientPost p = ClientPost
  { _cpText        = getBlocks $ unEmote (postClientPostType p) $ postMessage p
  , _cpUser        = postUserId p
  , _cpDate        = postCreateAt p
  , _cpType        = postClientPostType p
  , _cpPending     = False
  , _cpDeleted     = False
  , _cpAttachments = postFilenames p
  , _cpPostId      = p^.postIdL
  }

data ChannelContents = ChannelContents
  { _cdMessages :: Seq.Seq Message
  }

emptyChannelContents :: ChannelContents
emptyChannelContents = ChannelContents
  { _cdMessages = mempty
  }

makeLenses ''ChannelContents

data ChannelInfo = ChannelInfo
  { _cdViewed  :: UTCTime
  , _cdUpdated :: UTCTime
  , _cdName    :: T.Text
  , _cdHeader  :: T.Text
  , _cdType    :: Type
  , _cdLoaded  :: Bool
  }

makeLenses ''ChannelInfo

data ClientChannel = ClientChannel
  { _ccContents :: ChannelContents
  , _ccInfo     :: ChannelInfo
  }

makeLenses ''ClientChannel

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

data UserInfo = UserInfo
  { _uiName   :: T.Text
  , _uiId     :: UserId
  , _uiStatus :: UserStatus
  } deriving (Eq, Show)

makeLenses ''UserInfo

instance Ord UserInfo where
  u1 `compare` u2
    | u1^.uiStatus == Offline && u2^.uiStatus /= Offline =
      GT
    | u1^.uiStatus /= Offline && u2^.uiStatus == Offline =
      LT
    | otherwise =
      (u1^.uiName) `compare` (u2^.uiName)

userInfoFromProfile :: UserProfile -> UserInfo
userInfoFromProfile up = UserInfo
  { _uiName   = userProfileUsername up
  , _uiId     = userProfileId up
  , _uiStatus = Offline
  }

-- 'ChatResources' represents configuration and connection-related
-- information, as opposed to current model or view information.
data ChatResources = ChatResources
  { _crTok           :: Token
  , _crConn          :: ConnectionData
  , _crRequestQueue  :: RequestChan
  , _crEventQueue    :: Chan Event
  , _crTheme         :: AttrMap
  , _crTimeFormat    :: Maybe T.Text
  , _crQuitCondition :: MVar ()
  , _crSmartBacktick :: Bool
  }

data ChatEditState = ChatEditState
  { _cedEditor               :: Editor T.Text Name
  , _cedInputHistory         :: InputHistory
  , _cedInputHistoryPosition :: HM.HashMap ChannelId (Maybe Int)
  , _cedLastChannelInput     :: HM.HashMap ChannelId T.Text
  , _cedCurrentCompletion    :: Maybe T.Text
  , _cedCurrentAlternative   :: T.Text
  , _cedCompletionAlternatives :: [T.Text]
  }

emptyEditState :: InputHistory -> ChatEditState
emptyEditState hist = ChatEditState
  { _cedEditor               = editor MessageInput (vBox . map txt) (Just 1) ""
  , _cedInputHistory         = hist
  , _cedInputHistoryPosition = mempty
  , _cedLastChannelInput     = mempty
  , _cedCurrentCompletion    = Nothing
  , _cedCompletionAlternatives = []
  , _cedCurrentAlternative   = ""
  }

type RequestChan = Chan (IO (ChatState -> EventM Name ChatState))

data Mode =
    Main
    | ShowHelp
    | ChannelSelect
    | LeaveChannelConfirm
    | JoinChannel
    | ChannelScroll
    deriving (Eq)

data ConnectionStatus = Connected | Disconnected

-- This is the giant bundle of fields that represents the current
-- state of our application at any given time.
data ChatState = ChatState
  { _csResources            :: ChatResources
  , _csFocus                :: Zipper ChannelId
  , _csNames                :: MMNames
  , _csMe                   :: User
  , _csMyTeam               :: Team
  , _msgMap                 :: HashMap ChannelId ClientChannel
  , _usrMap                 :: HashMap UserId UserInfo
  , _timeZone               :: TimeZone
  , _csEditState            :: ChatEditState
  , _csMode                 :: Mode
  , _csChannelSelect        :: T.Text
  , _csRecentChannel        :: Maybe ChannelId
  , _csConnectionStatus     :: ConnectionStatus
  , _csJoinChannelList      :: Maybe (List Name Channel)
  }

data Event
  = VtyEvent Vty.Event
    -- ^ For events that arise from VTY
  | WSEvent WebsocketEvent
    -- ^ For events that arise from the websocket
  | RespEvent (ChatState -> EventM Name ChatState)
    -- ^ For the result values of async IO operations
  | RefreshWebsocketEvent
    -- ^ Tell our main loop to refresh the websocket connection
  | WebsocketDisconnect
  | WebsocketConnect

makeLenses ''ChatResources
makeLenses ''ChatState
makeLenses ''ChatEditState

-- interim lenses
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

csLastChannelInput :: Lens' ChatState (HM.HashMap ChannelId T.Text)
csLastChannelInput = csEditState . cedLastChannelInput

csCurrentCompletion :: Lens' ChatState (Maybe T.Text)
csCurrentCompletion = csEditState . cedCurrentCompletion

timeFormat :: Lens' ChatState (Maybe T.Text)
timeFormat = csResources . crTimeFormat

getUsernameForUserId :: ChatState -> UserId -> Maybe T.Text
getUsernameForUserId st uId = st^.usrMap ^? ix uId.uiName

clientPostToMessage :: ChatState -> ClientPost -> Message
clientPostToMessage st cp = Message
  { _mText     = _cpText cp
  , _mUserName = getUsernameForUserId st (_cpUser cp)
  , _mDate     = _cpDate cp
  , _mType     = CP $ _cpType cp
  , _mPending  = _cpPending cp
  , _mDeleted  = _cpDeleted cp
  , _mAttachments = _cpAttachments cp
  , _mPostId   = Just $ cp^.cpPostId
  }

clientMessageToMessage :: ClientMessage -> Message
clientMessageToMessage cm = Message
  { _mText        = getBlocks (_cmText cm)
  , _mUserName    = Nothing
  , _mDate        = _cmDate cm
  , _mType        = C $ _cmType cm
  , _mPending     = False
  , _mDeleted     = False
  , _mAttachments = Seq.empty
  , _mPostId      = Nothing
  }

data CmdArgs :: * -> * where
  NoArg    :: CmdArgs ()
  LineArg  :: T.Text -> CmdArgs T.Text
  TokenArg :: T.Text -> CmdArgs rest -> CmdArgs (T.Text, rest)

type CmdExec a = a -> ChatState -> EventM Name (Next ChatState)

data Cmd = forall a. Cmd
  { cmdName    :: T.Text
  , cmdDescr   :: T.Text
  , cmdArgSpec :: CmdArgs a
  , cmdAction  :: CmdExec a
  }

commandName :: Cmd -> T.Text
commandName (Cmd name _ _ _ ) = name

data Keybinding =
    KB { kbDescription :: T.Text
       , kbEvent :: Vty.Event
       , kbAction :: ChatState -> EventM Name (Next ChatState)
       }
