{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module Types
  ( ConnectionStatus(..)
  , HelpTopic(..)
  , MessageSelectState(..)
  , ProgramOutput(..)
  , MHEvent(..)
  , Name(..)
  , ChannelSelectMatch(..)
  , ConnectionInfo(..)
  , ciHostname
  , ciPort
  , ciUsername
  , ciPassword
  , Config(..)
  , HelpScreen(..)
  , PasswordSource(..)
  , MatchType(..)
  , EditMode(..)
  , Mode(..)
  , ChannelSelectPattern(..)
  , PostListContents(..)
  , ChannelSelectMap
  , AuthenticationException(..)
  , BackgroundInfo(..)
  , RequestChan

  , MMNames
  , mkNames
  , refreshChannelZipper
  , getChannelIdsInOrder

  , LinkChoice(LinkChoice)
  , linkUser
  , linkURL
  , linkTime
  , linkName
  , linkFileId

  , ChannelSelectState(..)
  , userMatches
  , channelMatches
  , channelSelectInput
  , selectedMatch
  , emptyChannelSelectState

  , ChatState
  , newState
  , csResources
  , csFocus
  , csCurrentChannel
  , csCurrentChannelId
  , csUrlList
  , csShowMessagePreview
  , csPostMap
  , csRecentChannel
  , csPostListOverlay
  , csMode
  , csMessageSelect
  , csJoinChannelList
  , csConnectionStatus
  , csWorkerIsBusy
  , csChannel
  , csChannels
  , csChannelSelectState
  , csEditState
  , timeZone

  , ChatEditState
  , emptyEditState
  , cedYankBuffer
  , cedSpellChecker
  , cedMisspellings
  , cedEditMode
  , cedCompletionAlternatives
  , cedCurrentCompletion
  , cedEditor
  , cedCurrentAlternative
  , cedMultiline
  , cedInputHistory
  , cedInputHistoryPosition
  , cedLastChannelInput

  , PostListOverlayState
  , postListSelected
  , postListPosts

  , ChatResources(ChatResources)
  , crPreferences
  , crEventQueue
  , crTheme
  , crSession
  , crSubprocessLog
  , crWebsocketActionChan
  , crRequestQueue
  , crUserStatusLock
  , crUserIdSet
  , crFlaggedPosts
  , crConn
  , crConfiguration

  , WebsocketAction(..)

  , Cmd(..)
  , commandName
  , CmdArgs(..)

  , MH
  , runMHEvent
  , mh
  , mhSuspendAndResume
  , mhHandleEventLensed

  , requestQuit
  , clientPostToMessage
  , getMessageForPostId
  , getParentMessage
  , resetSpellCheckTimer
  , withChannel
  , withChannelOrDefault
  , userList
  , hasUnread
  , channelNameFromMatch
  , isMine
  , getMyUser
  , getMyUser'
  , getMyUserId
  , getMyUserId'
  , getMyTeamId
  , getMyTeamId'
  , getUsernameForUserId
  , getUserIdForUsername
  , getUserIdForUsername'
  , getUserByDMChannelName'
  , getUserByUsername
  , getUserByUsername'
  , setUserStatus
  , getChannelIdByName
  , getChannelIdByName'
  , getChannelByName
  , getChannelByName'
  , getUserById
  , getUserById'
  , getAllUserIds
  , getAllChannelNames
  , getAllChannelNames'
  , getAllUsernames
  , getAllUsernames'
  , sortedUserList
  , removeChannelName
  , addChannelName
  , addNewUser
  , setUserIdSet
  , getChannelMentionCount'

  , userSigil
  , normalChannelSigil

  , HighlightSet(..)
  , UserSet
  , ChannelSet
  , getHighlightSet
  )
where

import           Prelude ()
import           Prelude.Compat

import           Brick (EventM, Next)
import qualified Brick
import           Brick.BChan
import           Brick.AttrMap (AttrMap)
import           Brick.Widgets.Edit (Editor, editor)
import           Brick.Widgets.List (List, list)
import           Control.Monad (when)
import qualified Control.Concurrent.STM as STM
import           Control.Concurrent.MVar (MVar)
import           Control.Exception (SomeException)
import qualified Control.Monad.State as St
import qualified Data.Foldable as F
import qualified Data.Sequence as Seq
import           Data.HashMap.Strict (HashMap)
import           Data.Time (UTCTime)
import           Data.Time.LocalTime.TimeZone.Series (TimeZoneSeries)
import qualified Data.HashMap.Strict as HM
import           Data.List (sort, partition, sortBy)
import           Data.Maybe
import           Data.Monoid
import qualified Data.Set as Set
import           Lens.Micro.Platform ( at, makeLenses, lens, (&), (^.), (%~), (.~), (^?!), (^?)
                                     , (.=), (%=), _Just, Traversal', preuse, (^..), folded, to
                                     , use
                                     )
import           Network.Mattermost (ConnectionData)
import           Network.Mattermost.Exceptions
import           Network.Mattermost.Lenses
import           Network.Mattermost.Types
import           Network.Mattermost.WebSocket (WebsocketEvent)
import           Network.Connection (HostNotResolved, HostCannotConnect)
import qualified Data.Text as T
import           System.Exit (ExitCode)
import           Text.Aspell (Aspell)

import           Zipper (Zipper, focusL, updateList)

import           InputHistory

import           Types.Channels
import           Types.KeyEvents
import           Types.Posts
import           Types.Messages
import           Types.Users

-- * Configuration

-- | A user password is either given to us directly, or a command
-- which we execute to find the password.
data PasswordSource =
    PasswordString T.Text
    | PasswordCommand T.Text
    deriving (Eq, Read, Show)

-- | These are all the values that can be read in our configuration
-- file.
data Config = Config
  { configUser                      :: Maybe T.Text
  , configHost                      :: Maybe T.Text
  , configTeam                      :: Maybe T.Text
  , configPort                      :: Int
  , configPass                      :: Maybe PasswordSource
  , configTimeFormat                :: Maybe T.Text
  , configDateFormat                :: Maybe T.Text
  , configTheme                     :: Maybe T.Text
  , configThemeCustomizationFile    :: Maybe T.Text
  , configSmartBacktick             :: Bool
  , configURLOpenCommand            :: Maybe T.Text
  , configURLOpenCommandInteractive :: Bool
  , configActivityBell              :: Bool
  , configShowBackground            :: BackgroundInfo
  , configShowMessagePreview        :: Bool
  , configEnableAspell              :: Bool
  , configAspellDictionary          :: Maybe T.Text
  , configUnsafeUseHTTP             :: Bool
  , configChannelListWidth          :: Int
  , configShowOlderEdits            :: Bool
  , configShowTypingIndicator       :: Bool
  , configAbsPath                   :: Maybe FilePath
  , configUserKeys                  :: KeyConfig
  } deriving (Eq, Show)

data BackgroundInfo = Disabled | Active | ActiveCount deriving (Eq, Show)

-- * 'MMNames' structures

-- | The 'MMNames' record is for listing human-readable
--   names and mapping them back to internal IDs.
data MMNames = MMNames
  { _cnChans    :: [T.Text] -- ^ All channel names
  , _cnToChanId :: HashMap T.Text ChannelId
      -- ^ Mapping from channel names to 'ChannelId' values
  , _cnUsers    :: [T.Text] -- ^ All users
  , _cnToUserId :: HashMap T.Text UserId
      -- ^ Mapping from user names to 'UserId' values
  }

mkNames :: User -> HM.HashMap UserId User -> Seq.Seq Channel -> MMNames
mkNames myUser users chans = MMNames
  { _cnChans = sort
               [ preferredChannelName c
               | c <- F.toList chans, channelType c /= Direct ]
  , _cnToChanId = HM.fromList $
                  [ (preferredChannelName c, channelId c) | c <- F.toList chans ] ++
                  [ (userUsername u, c)
                  | u <- HM.elems users
                  , c <- lookupChan (getDMChannelName (getId myUser) (getId u))
                  ]
  , _cnUsers = sort (map userUsername (HM.elems users))
  , _cnToUserId = HM.fromList
                  [ (userUsername u, getId u) | u <- HM.elems users ]
  }
  where lookupChan n = [ c^.channelIdL
                       | c <- F.toList chans, c^.channelNameL == n
                       ]

-- ** 'MMNames' Lenses

makeLenses ''MMNames

-- ** 'MMNames' functions

mkChannelZipperList :: MMNames -> [ChannelId]
mkChannelZipperList chanNames =
  getChannelIdsInOrder chanNames ++
  getDMChannelIdsInOrder chanNames

getChannelIdsInOrder :: MMNames -> [ChannelId]
getChannelIdsInOrder n = [ (n ^. cnToChanId) HM.! i | i <- n ^. cnChans ]

getDMChannelIdsInOrder :: MMNames -> [ChannelId]
getDMChannelIdsInOrder n =
  [ c | i <- n ^. cnUsers
      , c <- maybeToList (HM.lookup i (n ^. cnToChanId))
  ]

-- * Internal Names and References

-- | This 'Name' type is the value used in `brick` to identify the
-- currently focused widget or state.
data Name = ChannelMessages ChannelId
          | MessageInput
          | ChannelList
          | HelpViewport
          | HelpText
          | ScriptHelpText
          | ThemeHelpText
          | KeybindingHelpText
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

-- | Our 'ConnectionInfo' contains exactly as much information as is
-- necessary to start a connection with a Mattermost server
data ConnectionInfo =
    ConnectionInfo { _ciHostname :: T.Text
                   , _ciPort     :: Int
                   , _ciUsername :: T.Text
                   , _ciPassword :: T.Text
                   }

makeLenses ''ConnectionInfo

-- | We want to continue referring to posts by their IDs, but we don't want to
-- have to synthesize new valid IDs for messages from the client
-- itself (like error messages or informative client responses). To
-- that end, a PostRef can be either a PostId or a newly-generated
-- client ID
data PostRef
  = MMId PostId
  | CLId Int
    deriving (Eq, Show)

-- | For representing links to things in the 'open links' view
data LinkChoice = LinkChoice
  { _linkTime   :: ServerTime
  , _linkUser   :: UserRef
  , _linkName   :: T.Text
  , _linkURL    :: T.Text
  , _linkFileId :: Maybe FileId
  } deriving (Eq, Show)

makeLenses ''LinkChoice

-- Sigils
normalChannelSigil :: T.Text
normalChannelSigil = "~"

userSigil :: T.Text
userSigil = "@"

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


-- * Application State Values

data ProgramOutput =
    ProgramOutput { program :: FilePath
                  , programArgs :: [String]
                  , programStdout :: String
                  , programStdoutExpected :: Bool
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
  { _crSession             :: Session
  , _crConn                :: ConnectionData
  , _crRequestQueue        :: RequestChan
  , _crEventQueue          :: BChan MHEvent
  , _crSubprocessLog       :: STM.TChan ProgramOutput
  , _crWebsocketActionChan :: STM.TChan WebsocketAction
  , _crTheme               :: AttrMap
  , _crUserStatusLock      :: MVar ()
  , _crUserIdSet           :: STM.TVar (Seq.Seq UserId)
  , _crConfiguration       :: Config
  , _crFlaggedPosts        :: Set.Set PostId
  , _crPreferences         :: Seq.Seq Preference
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
  , _cedSpellChecker         :: Maybe (Aspell, IO ())
  , _cedMisspellings         :: Set.Set T.Text
  }

data EditMode =
    NewPost
    | Editing Post
    | Replying Message Post
      deriving (Show)

-- | We can initialize a new 'ChatEditState' value with just an
--   edit history, which we save locally.
emptyEditState :: InputHistory -> Maybe (Aspell, IO ()) -> ChatEditState
emptyEditState hist sp = ChatEditState
  { _cedEditor               = editor MessageInput Nothing ""
  , _cedMultiline            = False
  , _cedInputHistory         = hist
  , _cedInputHistoryPosition = mempty
  , _cedLastChannelInput     = mempty
  , _cedCurrentCompletion    = Nothing
  , _cedCompletionAlternatives = []
  , _cedCurrentAlternative   = ""
  , _cedEditMode             = NewPost
  , _cedYankBuffer           = ""
  , _cedSpellChecker         = sp
  , _cedMisspellings         = mempty
  }

-- | A 'RequestChan' is a queue of operations we have to perform
--   in the background to avoid blocking on the main loop
type RequestChan = STM.TChan (IO (MH ()))

-- | The 'HelpScreen' type represents the set of possible 'Help'
--   dialogues we have to choose from.
data HelpScreen
  = MainHelp
  | ScriptHelp
  | ThemeHelp
  | KeybindingHelp
    deriving (Eq)

-- |  Help topics
data HelpTopic =
    HelpTopic { helpTopicName         :: T.Text
              , helpTopicDescription  :: T.Text
              , helpTopicScreen       :: HelpScreen
              , helpTopicViewportName :: Name
              }
              deriving (Eq)

-- | Mode type for the current contents of the post list overlay
data PostListContents
  = PostListFlagged
  | PostListSearch T.Text Bool -- for the query and search status
  --   | PostListPinned ChannelId
  deriving (Eq)

-- | The 'Mode' represents the current dominant UI activity
data Mode =
    Main
    | ShowHelp HelpTopic
    | ChannelSelect
    | UrlSelect
    | LeaveChannelConfirm
    | DeleteChannelConfirm
    | JoinChannel
    | ChannelScroll
    | MessageSelect
    | MessageSelectDeleteConfirm
    | PostListOverlay PostListContents
    deriving (Eq)

-- | We're either connected or we're not.
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
  , _csChannels                    :: ClientChannels
  , _csPostMap                     :: HashMap PostId Message
  , _csUsers                       :: Users
  , _timeZone                      :: TimeZoneSeries
  , _csEditState                   :: ChatEditState
  , _csMode                        :: Mode
  , _csShowMessagePreview          :: Bool
  , _csChannelSelectState          :: ChannelSelectState
  , _csRecentChannel               :: Maybe ChannelId
  , _csUrlList                     :: List Name LinkChoice
  , _csConnectionStatus            :: ConnectionStatus
  , _csWorkerIsBusy                :: Maybe (Maybe Int)
  , _csJoinChannelList             :: Maybe (List Name Channel)
  , _csMessageSelect               :: MessageSelectState
  , _csPostListOverlay             :: PostListOverlayState
  }

newState :: ChatResources
         -> Zipper ChannelId
         -> User
         -> Team
         -> TimeZoneSeries
         -> InputHistory
         -> Maybe (Aspell, IO ())
         -> MMNames
         -> ChatState
newState rs i u m tz hist sp ns = ChatState
  { _csResources                   = rs
  , _csFocus                       = i
  , _csMe                          = u
  , _csMyTeam                      = m
  , _csNames                       = ns
  , _csChannels                    = noChannels
  , _csPostMap                     = HM.empty
  , _csUsers                       = noUsers
  , _timeZone                      = tz
  , _csEditState                   = emptyEditState hist sp
  , _csMode                        = Main
  , _csShowMessagePreview          = configShowMessagePreview $ _crConfiguration rs
  , _csChannelSelectState          = emptyChannelSelectState
  , _csRecentChannel               = Nothing
  , _csUrlList                     = list UrlList mempty 2
  , _csConnectionStatus            = Connected
  , _csWorkerIsBusy                = Nothing
  , _csJoinChannelList             = Nothing
  , _csMessageSelect               = MessageSelectState Nothing
  , _csPostListOverlay             = PostListOverlayState mempty Nothing
  }

type ChannelSelectMap = HM.HashMap T.Text ChannelSelectMatch

data ChannelSelectState =
    ChannelSelectState { _channelSelectInput :: T.Text
                       , _channelMatches     :: ChannelSelectMap
                       , _userMatches        :: ChannelSelectMap
                       , _selectedMatch      :: T.Text
                       }

emptyChannelSelectState :: ChannelSelectState
emptyChannelSelectState =
    ChannelSelectState { _channelSelectInput = ""
                       , _channelMatches     = mempty
                       , _userMatches        = mempty
                       , _selectedMatch      = ""
                       }

data MessageSelectState =
    MessageSelectState { selectMessagePostId :: Maybe PostId }

data PostListOverlayState = PostListOverlayState
  { _postListPosts    :: Messages
  , _postListSelected :: Maybe PostId
  }

-- | Actions that can be sent on the websocket to the server.
data WebsocketAction =
    UserTyping UTCTime ChannelId (Maybe PostId) -- ^ user typing in the input box
  -- | GetStatuses
  -- | GetStatusesByIds [UserId]
  deriving (Read, Show, Eq, Ord)

-- * MH Monad

-- | A value of type 'MH' @a@ represents a computation that can
-- manipulate the application state and also request that the
-- application quit
newtype MH a =
  MH { fromMH :: St.StateT (ChatState, ChatState -> EventM Name (Next ChatState))
                           (EventM Name) a }

-- | Run an 'MM' computation, choosing whether to continue or halt
--   based on the resulting
runMHEvent :: ChatState -> MH () -> EventM Name (Next ChatState)
runMHEvent st (MH mote) = do
  ((), (st', rs)) <- St.runStateT mote (st, Brick.continue)
  rs st'

-- | lift a computation in 'EventM' into 'MH'
mh :: EventM Name a -> MH a
mh = MH . St.lift

mhHandleEventLensed :: Lens' ChatState b -> (e -> b -> EventM Name b) -> e -> MH ()
mhHandleEventLensed ln f event = MH $ do
  (st, b) <- St.get
  n <- St.lift $ f event (st ^. ln)
  St.put (st & ln .~ n , b)

mhSuspendAndResume :: (ChatState -> IO ChatState) -> MH ()
mhSuspendAndResume mote = MH $ do
  (st, _) <- St.get
  St.put (st, \ _ -> Brick.suspendAndResume (mote st))

-- | This will request that after this computation finishes the
-- application should exit
requestQuit :: MH ()
requestQuit = MH $ do
  (st, _) <- St.get
  St.put (st, Brick.halt)

instance Functor MH where
  fmap f (MH x) = MH (fmap f x)

instance Applicative MH where
  pure x = MH (pure x)
  MH f <*> MH x = MH (f <*> x)

instance Monad MH where
  return x = MH (return x)
  MH x >>= f = MH (x >>= \ x' -> fromMH (f x'))

-- We want to pretend that the state is only the ChatState, rather
-- than the ChatState and the Brick continuation
instance St.MonadState ChatState MH where
  get = fst `fmap` MH St.get
  put st = MH $ do
    (_, c) <- St.get
    St.put (st, c)

instance St.MonadIO MH where
  liftIO = MH . St.liftIO

-- | This represents events that we handle in the main application loop.
data MHEvent
    = WSEvent WebsocketEvent
    -- ^ For events that arise from the websocket
    | RespEvent (MH ())
    -- ^ For the result values of async IO operations
    | AsyncErrEvent SomeException
    -- ^ For errors that arise in the course of async IO operations
    | RefreshWebsocketEvent
    -- ^ Tell our main loop to refresh the websocket connection
    | WebsocketParseError String
    -- ^ We failed to parse an incoming websocket event
    | WebsocketDisconnect
    -- ^ The websocket connection went down.
    | WebsocketConnect
    -- ^ The websocket connection came up.
    | BGIdle
    -- ^ background worker is idle
    | BGBusy (Maybe Int)
    -- ^ background worker is busy (with n requests)

-- ** Application State Lenses

makeLenses ''ChatResources
makeLenses ''ChatState
makeLenses ''ChatEditState
makeLenses ''PostListOverlayState
makeLenses ''ChannelSelectState

resetSpellCheckTimer :: ChatEditState -> IO ()
resetSpellCheckTimer s =
    case s^.cedSpellChecker of
        Nothing -> return ()
        Just (_, reset) -> reset

-- ** Utility Lenses
csCurrentChannelId :: Lens' ChatState ChannelId
csCurrentChannelId = csFocus.focusL

csCurrentChannel :: Lens' ChatState ClientChannel
csCurrentChannel =
  lens (\ st -> findChannelById (st^.csCurrentChannelId) (st^.csChannels) ^?! _Just)
       (\ st n -> st & csChannels %~ addChannel (st^.csCurrentChannelId) n)

csChannel :: ChannelId -> Traversal' ChatState ClientChannel
csChannel cId =
  csChannels . channelByIdL cId

withChannel :: ChannelId -> (ClientChannel -> MH ()) -> MH ()
withChannel cId = withChannelOrDefault cId ()

withChannelOrDefault :: ChannelId -> a -> (ClientChannel -> MH a) -> MH a
withChannelOrDefault cId deflt mote = do
  chan <- preuse (csChannel(cId))
  case chan of
    Nothing -> return deflt
    Just c  -> mote c

-- ** 'ChatState' Helper Functions

isMine :: ChatState -> Message -> Bool
isMine st msg = (UserI $ getMyUserId' st) == msg^.mUser

getMessageForPostId :: ChatState -> PostId -> Maybe Message
getMessageForPostId st pId = st^.csPostMap.at(pId)

getParentMessage :: ChatState -> Message -> Maybe Message
getParentMessage st msg
  | InReplyTo pId <- msg^.mInReplyToMsg
    = st^.csPostMap.at(pId)
  | otherwise = Nothing

setUserStatus :: UserId -> T.Text -> MH ()
setUserStatus uId t = csUsers %= modifyUserById uId (uiStatus .~ statusFromText t)

getUsernameForUserId :: ChatState -> UserId -> Maybe T.Text
getUsernameForUserId st uId = _uiName <$> findUserById uId (st^.csUsers)

getUserIdForUsername :: T.Text -> MH (Maybe UserId)
getUserIdForUsername name = getUserIdForUsername' name <$> use id

getUserIdForUsername' :: T.Text -> ChatState -> Maybe UserId
getUserIdForUsername' name st = st^.csNames.cnToUserId.at name

getChannelIdByName :: T.Text -> MH (Maybe ChannelId)
getChannelIdByName name = do
    st <- use id
    return $ getChannelIdByName' st name

getChannelByName :: T.Text -> MH (Maybe ClientChannel)
getChannelByName name = do
    st <- use id
    return $ getChannelByName' st name

getChannelIdByName' :: ChatState -> T.Text -> Maybe ChannelId
getChannelIdByName' st name =
    let nameToChanId = st^.csNames.cnToChanId
    in HM.lookup (trimAnySigil name) nameToChanId

getChannelByName' :: ChatState -> T.Text -> Maybe ClientChannel
getChannelByName' st name = do
    cId <- getChannelIdByName' st name
    findChannelById cId (st^.csChannels)

trimAnySigil :: T.Text -> T.Text
trimAnySigil n
    | normalChannelSigil `T.isPrefixOf` n = T.tail n
    | userSigil `T.isPrefixOf` n          = T.tail n
    | otherwise                           = n

addNewUser :: UserInfo -> MH ()
addNewUser u = do
    csUsers %= addUser u

    let uname = u^.uiName
        uid = u^.uiId
    csNames.cnUsers %= (sort . (uname:))
    csNames.cnToUserId.at uname .= Just uid

    userSet <- use (csResources.crUserIdSet)
    St.liftIO $ STM.atomically $ STM.modifyTVar userSet $ (uid Seq.<|)

setUserIdSet :: Seq.Seq UserId -> MH ()
setUserIdSet ids = do
    userSet <- use (csResources.crUserIdSet)
    St.liftIO $ STM.atomically $ STM.writeTVar userSet ids

addChannelName :: Type -> ChannelId -> T.Text -> MH ()
addChannelName chType cid name = do
    csNames.cnToChanId.at(name) .= Just cid

    -- For direct channels the username is already in the user list so
    -- do nothing
    existingNames <- getAllChannelNames
    when (chType /= Direct && (not $ name `elem` existingNames)) $
        csNames.cnChans %= (sort . (name:))

getChannelMentionCount' :: ChatState -> ChannelId -> Int
getChannelMentionCount' st cId =
    maybe 0 id (st^?csChannel(cId).ccInfo.cdMentionCount)

getAllChannelNames :: MH [T.Text]
getAllChannelNames = getAllChannelNames' <$> use id

getAllUsernames :: MH [T.Text]
getAllUsernames = getAllUsernames' <$> use id

getAllChannelNames' :: ChatState -> [T.Text]
getAllChannelNames' st = st^.csNames.cnChans

getAllUsernames' :: ChatState -> [T.Text]
getAllUsernames' st = st^.csNames.cnChans

removeChannelName :: T.Text -> MH ()
removeChannelName name = do
    -- Flush cnToChanId
    csNames.cnToChanId.at name .= Nothing
    -- Flush cnChans
    csNames.cnChans %= filter (/= name)

-- Rebuild the channel zipper contents from the current names collection.
refreshChannelZipper :: MH ()
refreshChannelZipper = do
    -- We should figure out how to do this better: this adds it to the
    -- channel zipper in such a way that we don't ever change our focus
    -- to something else, which is kind of silly
    names <- use csNames
    let newZip = updateList (mkChannelZipperList names)
    csFocus %= newZip

clientPostToMessage :: ClientPost -> Message
clientPostToMessage cp = Message
  { _mText          = cp^.cpText
  , _mUser          = case cp^.cpUserOverride of
                        Just n | cp^.cpType == NormalPost -> UserOverride (n <> "[BOT]")
                        _ -> maybe NoUser UserI $ cp^.cpUser
  , _mDate          = cp^.cpDate
  , _mType          = CP $ cp^.cpType
  , _mPending       = cp^.cpPending
  , _mDeleted       = cp^.cpDeleted
  , _mAttachments   = cp^.cpAttachments
  , _mInReplyToMsg  =
    case cp^.cpInReplyToPost of
      Nothing  -> NotAReply
      Just pId -> InReplyTo pId
  , _mPostId        = Just $ cp^.cpPostId
  , _mReactions     = cp^.cpReactions
  , _mOriginalPost  = Just $ cp^.cpOriginalPost
  , _mFlagged       = False
  , _mChannelId     = Just $ cp^.cpChannelId
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
type CmdExec a = a -> MH ()

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

-- *  Channel Updates and Notifications

hasUnread :: ChatState -> ChannelId -> Bool
hasUnread st cId = maybe False id $ do
  chan <- findChannelById cId (st^.csChannels)
  lastViewTime <- chan^.ccInfo.cdViewed
  return $ (chan^.ccInfo.cdUpdated > lastViewTime) ||
           (isJust $ chan^.ccInfo.cdEditedMessageThreshold)

userList :: ChatState -> [UserInfo]
userList st = filter showUser $ allUsers (st^.csUsers)
  where showUser u = not (isSelf u) && (u^.uiInTeam)
        isSelf u = (getMyUserId' st) == (u^.uiId)

getAllUserIds :: MH [UserId]
getAllUserIds = allUserIds <$> use csUsers

getUserById :: UserId -> MH (Maybe UserInfo)
getUserById uId = getUserById' uId <$> use id

getUserById' :: UserId -> ChatState -> Maybe UserInfo
getUserById' uId st = findUserById uId (st^.csUsers)

getMyUserId :: MH UserId
getMyUserId = getMyUserId' <$> use id

getMyUserId' :: ChatState -> UserId
getMyUserId' st = getMyUser' st ^. userIdL

getMyTeamId :: MH TeamId
getMyTeamId = getMyTeamId' <$> use id

getMyTeamId' :: ChatState -> TeamId
getMyTeamId' st = st ^. csMyTeam . teamIdL

getMyUser :: MH User
getMyUser = getMyUser' <$> use id

getMyUser' :: ChatState -> User
getMyUser' st = st^.csMe

getUserByDMChannelName' :: T.Text
                        -- ^ the dm channel name
                        -> UserId
                        -- ^ me
                        -> ChatState
                        -> Maybe UserInfo
                        -- ^ you
getUserByDMChannelName' name self st =
    findUserByDMChannelName (st^.csUsers) name self

getUserByUsername :: T.Text -> MH (Maybe UserInfo)
getUserByUsername name = getUserByUsername' name <$> use id

getUserByUsername' :: T.Text -> ChatState -> Maybe UserInfo
getUserByUsername' name st = do
    uId <- getUserIdForUsername' name st
    getUserById' uId st

sortedUserList :: ChatState -> [UserInfo]
sortedUserList st = sortBy cmp yes <> sortBy cmp no
  where
      cmp = compareUserInfo uiName
      dmHasUnread u =
          case st^.csNames.cnToChanId.at(u^.uiName) of
            Nothing  -> False
            Just cId
              | (st^.csCurrentChannelId) == cId -> False
              | otherwise -> hasUnread st cId
      (yes, no) = partition dmHasUnread (filter (not . _uiDeleted) $ userList st)

compareUserInfo :: (Ord a) => Lens' UserInfo a -> UserInfo -> UserInfo -> Ordering
compareUserInfo field u1 u2
    | u1^.uiStatus == Offline && u2^.uiStatus /= Offline =
      GT
    | u1^.uiStatus /= Offline && u2^.uiStatus == Offline =
      LT
    | otherwise =
      (u1^.field) `compare` (u2^.field)

-- * HighlightSet

type UserSet = Set.Set T.Text
type ChannelSet = Set.Set T.Text

data HighlightSet = HighlightSet
  { hUserSet    :: Set.Set T.Text
  , hChannelSet :: Set.Set T.Text
  }

getHighlightSet :: ChatState -> HighlightSet
getHighlightSet st = HighlightSet
  { hUserSet = Set.fromList (st^..csUsers.to allUsers.folded.uiName)
  , hChannelSet = Set.fromList (st^..csChannels.folded.ccInfo.cdName)
  }
