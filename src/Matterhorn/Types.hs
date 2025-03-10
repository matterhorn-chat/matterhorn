{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RecordWildCards #-}
module Matterhorn.Types
  ( ConnectionStatus(..)
  , HelpTopic(..)
  , ProgramOutput(..)
  , MHEvent(..)
  , InternalEvent(..)
  , StartupStateInfo(..)
  , MHError(..)
  , CPUUsagePolicy(..)
  , SemEq(..)
  , Work(..)
  , handleEventWith
  , getServerBaseUrl
  , serverBaseUrl
  , ConnectionInfo(..)
  , SidebarUpdate(..)
  , PendingChannelChange(..)
  , ViewMessageWindowTab(..)
  , clearChannelUnreadStatus
  , ChannelListSorting(..)
  , TeamListSorting(..)
  , ThreadOrientation(..)
  , ChannelListOrientation(..)
  , channelListEntryUserId
  , userIdsFromZipper
  , entryIsDMEntry
  , ciHostname
  , ciPort
  , ciUrlPath
  , ciUsername
  , ciOTPToken
  , ciPassword
  , ciType
  , ciAccessToken
  , ChannelTopicDialogState(..)
  , channelTopicDialogEditor
  , channelTopicDialogFocus

  , CharWidths
  , newCharWidths
  , buildWidthMap

  , resultToWidget

  , MHKeyEventHandler
  , mhHandleKeyboardEvent

  , Config(..)
  , configUserL
  , configHostL
  , configTeamL
  , configPortL
  , configUrlPathL
  , configPassL
  , configTokenL
  , configOTPTokenL
  , configTimeFormatL
  , configDateFormatL
  , configThemeL
  , configThemeCustomizationFileL
  , configSmartBacktickL
  , configSmartEditingL
  , configURLOpenCommandL
  , configURLOpenCommandInteractiveL
  , configActivityNotifyCommandL
  , configActivityNotifyVersionL
  , configActivityBellL
  , configTruncateVerbatimBlocksL
  , configChannelListSortingL
  , configTeamListSortingL
  , configShowMessageTimestampsL
  , configShowBackgroundL
  , configShowMessagePreviewL
  , configShowChannelListL
  , configShowExpandedChannelTopicsL
  , configEnableAspellL
  , configAspellDictionaryL
  , configUnsafeUseHTTPL
  , configValidateServerCertificateL
  , configChannelListWidthL
  , configLogMaxBufferSizeL
  , configShowOlderEditsL
  , configShowTypingIndicatorL
  , configSendTypingNotificationsL
  , configAbsPathL
  , configUserKeysL
  , configHyperlinkingModeL
  , configSyntaxDirsL
  , configDirectChannelExpirationDaysL
  , configCpuUsagePolicyL
  , configDefaultAttachmentPathL
  , configChannelListOrientationL
  , configThreadOrientationL
  , configMouseModeL
  , configShowLastOpenThreadL
  , configChannelSelectCaseInsensitiveL
  , configCharacterWidthsL

  , unsafeKeyDispatcher
  , bindingConflictMessage

  , ChannelListWidth(..)
  , NotificationVersion(..)
  , PasswordSource(..)
  , TokenSource(..)
  , OTPTokenSource(..)
  , MatchType(..)
  , Mode(..)
  , ChannelSelectPattern(..)
  , PostListContents(..)
  , AuthenticationException(..)
  , BackgroundInfo(..)
  , RequestChan
  , UserFetch(..)
  , writeBChan
  , InternalTheme(..)

  , attrNameToConfig

  , matchesTeam
  , teamUnreadCount
  , teamZipperIds
  , mkChannelZipperList
  , ChannelListGroup(..)
  , nonDMChannelListGroupUnread

  , ThreadInterface
  , ChannelMessageInterface

  , threadInterface
  , unsafeThreadInterface
  , maybeThreadInterface
  , threadInterfaceEmpty
  , threadInterfaceDeleteWhere
  , modifyThreadMessages
  , modifyEachThreadMessage

  , trimChannelSigil

  , ChannelSelectState(..)
  , channelSelectMatches
  , channelSelectInput
  , emptyChannelSelectState

  , TeamState(..)
  , tsFocus
  , tsPendingChannelChange
  , tsRecentChannel
  , tsReturnChannel
  , tsTeam
  , tsChannelSelectState
  , tsViewedMessage
  , tsPostListWindow
  , tsUserListWindow
  , tsChannelListWindow
  , tsNotifyPrefs
  , tsChannelTopicDialog
  , tsReactionEmojiListWindow
  , tsThemeListWindow
  , tsChannelListSorting
  , tsThreadInterface
  , tsMessageInterfaceFocus

  , teamMode
  , teamModes
  , getTeamMode

  , MessageInterfaceFocus(..)
  , messageInterfaceFocusNext
  , messageInterfaceFocusPrev

  , channelEditor
  , channelMessageSelect

  , ChatState
  , newState

  , withCurrentChannel
  , withCurrentChannel'
  , withCurrentTeam
  , forEachTeam

  , csTeamZipper
  , csTeams
  , csTeam
  , csChannelListOrientation
  , csResources
  , csLastMouseDownEvent
  , csGlobalEditState
  , csVerbatimTruncateSetting
  , csCurrentChannelId
  , csCurrentTeamId
  , csPostMap
  , csUsers
  , csHiddenChannelGroups
  , csConnectionStatus
  , csWorkerIsBusy
  , csChannel
  , csChannelMessages
  , csChannelMessageInterface
  , maybeChannelMessageInterface
  , csChannels
  , csClientConfig
  , csInputHistory
  , csMe
  , timeZone
  , whenMode
  , pushMode
  , pushMode'
  , popMode
  , replaceMode

  , GlobalEditState(..)
  , emptyGlobalEditState
  , gedYankBuffer

  , PostListWindowState(..)
  , postListSelected
  , postListPosts

  , UserSearchScope(..)
  , ChannelSearchScope(..)

  , ListWindowState(..)
  , listWindowSearchResults
  , listWindowSearchInput
  , listWindowSearchScope
  , listWindowSearching
  , listWindowEnterHandler
  , listWindowNewList
  , listWindowFetchResults
  , listWindowRecordCount

  , getUsers

  , ChatResources(..)
  , crUserPreferences
  , crEventQueue
  , crTheme
  , crThemeOriginal
  , crStatusUpdateChan
  , crSubprocessLog
  , crWebsocketActionChan
  , crWebsocketThreadId
  , crRequestQueue
  , crFlaggedPosts
  , crConn
  , crConfiguration
  , crSyntaxMap
  , crLogManager
  , crSpellChecker
  , crWindowSize
  , crEmoji
  , getSession
  , getResourceSession

  , specialUserMentions

  , applyTeamOrder
  , refreshTeamZipper

  , UserPreferences(UserPreferences)
  , userPrefShowJoinLeave
  , userPrefFlaggedPostList
  , userPrefGroupChannelPrefs
  , userPrefDirectChannelPrefs
  , userPrefTeammateNameDisplayMode
  , userPrefTeamOrder
  , userPrefFavoriteChannelPrefs
  , dmChannelShowPreference
  , groupChannelShowPreference
  , favoriteChannelPreference

  , defaultUserPreferences
  , setUserPreferences

  , WebsocketAction(..)

  , Cmd(..)
  , commandName
  , CmdArgs(..)

  , MH
  , runMHEvent
  , scheduleUserFetches
  , scheduleUserStatusFetches
  , getScheduledUserFetches
  , getScheduledUserStatusFetches
  , mh
  , generateUUID
  , generateUUID_IO
  , mhSuspendAndResume
  , mhZoom
  , mhZoom'
  , mhContinueWithoutRedraw
  , St.gets
  , mhError

  , mhLog
  , mhGetIOLogger
  , ioLogWithManager
  , LogContext(..)
  , withLogContext
  , withLogContextChannelId
  , getLogContext
  , LogMessage(..)
  , LogCommand(..)
  , LogCategory(..)

  , LogManager(..)
  , startLoggingToFile
  , stopLoggingToFile
  , requestLogSnapshot
  , requestLogDestination
  , sendLogMessage

  , requestQuit
  , getMessageForPostId
  , getParentMessage
  , getReplyRootMessage
  , withChannel
  , withChannelOrDefault
  , userList
  , resetAutocomplete
  , isMine
  , setUserStatus
  , myUser
  , myUsername
  , myUserId
  , usernameForUserId
  , userByUsername
  , userByNickname
  , channelIdByChannelName
  , channelIdByUsername
  , userById
  , allUserIds
  , addNewUser
  , useNickname
  , useNickname'
  , displayNameForUserId
  , displayNameForUser
  , raiseInternalEvent
  , getNewMessageCutoff
  , getEditedMessageCutoff

  , HighlightSet(..)
  , UserSet
  , ChannelSet
  , getHighlightSet
  , emptyHSet

  , moveLeft
  , moveRight

  , module Matterhorn.Types.Core
  , module Matterhorn.Types.Channels
  , module Matterhorn.Types.EditState
  , module Matterhorn.Types.Messages
  , module Matterhorn.Types.MessageInterface
  , module Matterhorn.Types.TabbedWindow
  , module Matterhorn.Types.Posts
  , module Matterhorn.Types.Users
  )
where

import           Prelude ()
import           Matterhorn.Prelude

import           GHC.Stack ( HasCallStack )

import qualified Brick
import           Brick ( EventM, Widget(..), Size(..), Result )
import           Brick.Keybindings
import           Brick.Focus ( FocusRing )
import           Brick.Themes ( Theme )
import           Brick.Main ( invalidateCache, invalidateCacheEntry )
import           Brick.AttrMap ( AttrMap )
import qualified Brick.BChan as BCH
import           Brick.Forms (Form)
import           Brick.Widgets.Edit ( Editor, editor )
import           Brick.Widgets.List ( List )
import           Control.Concurrent ( ThreadId )
import           Control.Concurrent.Async ( Async )
import qualified Control.Concurrent.STM as STM
import           Control.Exception ( SomeException )
import qualified Control.Monad.State as St
import qualified Control.Monad.Reader as R
import qualified Data.Set as Set
import qualified Data.Foldable as F
import           Data.Function ( on )
import qualified Data.Kind as K
import           Data.Maybe ( fromJust )
import           Data.Ord ( comparing, Down(..) )
import qualified Data.HashMap.Strict as HM
import           Data.List ( sortBy, elemIndex, partition )
import qualified Data.Sequence as Seq
import qualified Data.Text as T
import           Data.Time.Clock ( getCurrentTime, addUTCTime )
import           Data.UUID ( UUID )
import qualified Data.Vector as Vec
import qualified Graphics.Vty as Vty
import qualified Graphics.Vty.UnicodeWidthTable.Types as Vty
import           Lens.Micro.Platform ( at, makeLenses, lens, (^?!), (.=)
                                     , (%=), (%~), (.~), _Just, Traversal', to
                                     , SimpleGetter, filtered, traversed, singular
                                     , zoom
                                     )
import           Network.Connection ( HostNotResolved, HostCannotConnect )
import           Skylighting.Types ( SyntaxMap )
import           System.Exit ( ExitCode )
import           System.Random ( randomIO )
import           Text.Aspell ( Aspell )

import           Network.Mattermost ( ConnectionData )
import           Network.Mattermost.Exceptions
import           Network.Mattermost.Lenses
import           Network.Mattermost.Types
import           Network.Mattermost.Types.Config
import           Network.Mattermost.WebSocket ( WebsocketEvent, WebsocketActionResponse )

import           Matterhorn.Constants ( normalChannelSigil )
import           Matterhorn.InputHistory
import           Matterhorn.Emoji
import           Matterhorn.Types.Common
import           Matterhorn.Types.Core
import           Matterhorn.Types.Channels
import           Matterhorn.Types.EditState
import           Matterhorn.Types.Messages
import           Matterhorn.Types.MessageInterface
import           Matterhorn.Types.NonemptyStack
import           Matterhorn.Types.Posts
import           Matterhorn.Types.RichText ( TeamBaseURL(..), TeamURLName(..) )
import           Matterhorn.Types.TabbedWindow
import           Matterhorn.Types.Users
import qualified Matterhorn.Zipper as Z


-- * Configuration

-- | A notification version for the external notifier
data NotificationVersion =
    NotifyV1
    | NotifyV2
    | NotifyV3
    deriving (Eq, Read, Show)

-- | A user password is either given to us directly, or a command
-- which we execute to find the password.
data PasswordSource =
    PasswordString Text
    | PasswordCommand Text
    deriving (Eq, Read, Show)

-- | An access token source.
data TokenSource =
    TokenString Text
    | TokenCommand Text
    deriving (Eq, Read, Show)

-- | An OTP token source.
data OTPTokenSource =
    OTPTokenString Text
    | OTPTokenCommand Text
    deriving (Eq, Read, Show)


-- | The type of channel list group headings. Integer arguments indicate
-- total number of channels in the group that have unread activity.
data ChannelListGroup =
    ChannelListGroup { channelListGroupLabel :: ChannelListGroupLabel
                     , channelListGroupUnread :: Int
                     , channelListGroupCollapsed :: Bool
                     , channelListGroupEntries :: Int
                     }
                     deriving (Eq, Show)

nonDMChannelListGroupUnread :: ChannelListGroup -> Int
nonDMChannelListGroupUnread g =
    case channelListGroupLabel g of
        ChannelGroupDirectMessages -> 0
        _ -> channelListGroupUnread g

data ChannelListSorting =
    ChannelListSortDefault
    | ChannelListSortUnreadFirst
    deriving (Eq, Show, Ord)

data TeamListSorting =
    TeamListSortDefault
    | TeamListSortUnreadFirst
    deriving (Eq, Show, Ord)

data ChannelListWidth =
    ChannelListWidthFixed Int
    -- ^ A fixed width in columns.
    | ChannelListWidthAuto
    -- ^ Automatically determine a reasonable width based on the window
    -- dimensions.
    deriving (Eq, Show, Ord)

newtype CharWidths = CharWidths [(Char, Int)]
                   deriving (Eq, Show)

newCharWidths :: [(Char, Int)] -> CharWidths
newCharWidths = CharWidths

buildWidthMap :: CharWidths -> Vty.UnicodeWidthTable
buildWidthMap (CharWidths pairs) =
    Vty.UnicodeWidthTable (mkRange <$> pairs)
    where
        mkRange (ch, w) =
            Vty.WidthTableRange { Vty.rangeStart = toEnum $ fromEnum ch
                                , Vty.rangeSize = 1
                                , Vty.rangeColumns = toEnum w
                                }

-- | This is how we represent the user's configuration. Most fields
-- correspond to configuration file settings (see Config.hs) but some
-- are for internal book-keeping purposes only.
data Config =
    Config { configUser :: Maybe Text
           -- ^ The username to use when connecting.
           , configHost :: Maybe Text
           -- ^ The hostname to use when connecting.
           , configTeam :: Maybe Text
           -- ^ The team name to use when connecting.
           , configPort :: Int
           -- ^ The port to use when connecting.
           , configUrlPath :: Maybe Text
           -- ^ The server path to use when connecting.
           , configPass :: Maybe PasswordSource
           -- ^ The password source to use when connecting.
           , configToken :: Maybe TokenSource
           -- ^ The token source to use when connecting.
           , configOTPToken :: Maybe OTPTokenSource
           -- ^ The OTP token source to use when connecting.
           , configTimeFormat :: Maybe Text
           -- ^ The format string for timestamps.
           , configDateFormat :: Maybe Text
           -- ^ The format string for dates.
           , configTheme :: Maybe Text
           -- ^ The name of the theme to use.
           , configThemeCustomizationFile :: Maybe Text
           -- ^ The path to the theme customization file, if any.
           , configSmartBacktick :: Bool
           -- ^ Whether to enable smart quoting characters.
           , configSmartEditing :: Bool
           -- ^ Whether to enable smart editing behaviors.
           , configURLOpenCommand :: Maybe Text
           -- ^ The command to use to open URLs.
           , configURLOpenCommandInteractive :: Bool
           -- ^ Whether the URL-opening command is interactive (i.e.
           -- whether it should be given control of the terminal).
           , configActivityNotifyCommand :: Maybe T.Text
           -- ^ The command to run for activity notifications.
           , configActivityNotifyVersion :: NotificationVersion
           -- ^ The activity notifier version.
           , configActivityBell :: Bool
           -- ^ Whether to ring the terminal bell on activity.
           , configTruncateVerbatimBlocks :: Maybe Int
           -- ^ Whether to truncate verbatim (and code) blocks past a
           -- reasonable number of lines.
           , configShowMessageTimestamps :: Bool
           -- ^ Whether to show timestamps on messages.
           , configShowBackground :: BackgroundInfo
           -- ^ Whether to show async background worker thread info.
           , configShowMessagePreview :: Bool
           -- ^ Whether to show the message preview area.
           , configShowChannelList :: Bool
           -- ^ Whether to show the channel list.
           , configShowExpandedChannelTopics :: Bool
           -- ^ Whether to show expanded channel topics.
           , configEnableAspell :: Bool
           -- ^ Whether to enable Aspell spell checking.
           , configAspellDictionary :: Maybe Text
           -- ^ A specific Aspell dictionary name to use.
           , configUnsafeUseHTTP :: Bool
           -- ^ Whether to permit an insecure HTTP connection.
           , configValidateServerCertificate :: Bool
           -- ^ Whether to validate TLS certificates.
           , configChannelListWidth :: ChannelListWidth
           -- ^ The width, in columns, of the channel list sidebar.
           , configLogMaxBufferSize :: Int
           -- ^ The maximum size, in log entries, of the internal log
           -- message buffer.
           , configShowOlderEdits :: Bool
           -- ^ Whether to highlight the edit indicator on edits made
           -- prior to the beginning of the current session.
           , configChannelListSorting :: ChannelListSorting
           -- ^ How to sort channels in each channel list group
           , configTeamListSorting :: TeamListSorting
           -- ^ How to sort teams in the team list
           , configShowTypingIndicator :: Bool
           -- ^ Whether to show the typing indicator when other users
           -- are typing
           , configSendTypingNotifications :: Bool
           -- Whether to send typing notifications to other users.
           , configAbsPath :: Maybe FilePath
           -- ^ A book-keeping field for the absolute path to the
           -- configuration. (Not a user setting.)
           , configUserKeys :: KeyConfig KeyEvent
           -- ^ The user's keybinding configuration.
           , configHyperlinkingMode :: Bool
           -- ^ Whether to enable terminal hyperlinking mode.
           , configSyntaxDirs :: [FilePath]
           -- ^ The search path for syntax description XML files.
           , configDirectChannelExpirationDays :: Int
           -- ^ The number of days to show a user in the channel menu after a direct
           -- message with them.
           , configCpuUsagePolicy :: CPUUsagePolicy
           -- ^ The CPU usage policy for the application.
           , configDefaultAttachmentPath :: Maybe FilePath
           -- ^ The default path for browsing attachments
           , configChannelListOrientation :: ChannelListOrientation
           -- ^ The orientation of the channel list.
           , configThreadOrientation :: ThreadOrientation
           -- ^ The orientation of the thread window relative to the
           -- main channel message window.
           , configMouseMode :: Bool
           -- ^ Whether to enable mouse support in matterhorn
           , configShowLastOpenThread :: Bool
           -- ^ Whether to re-open a thread that was open the last time
           -- Matterhorn quit
           , configChannelSelectCaseInsensitive :: Bool
           -- ^ Whether channel selection input is always matched
           -- case-insensitively
           , configCharacterWidths :: Maybe CharWidths
           -- ^ Map of Unicode characters to widths to configure Vty
           } deriving (Eq, Show)

-- | The policy for CPU usage.
--
-- The idea is that Matterhorn can benefit from using multiple CPUs,
-- but the exact number is application-determined. We expose this policy
-- setting to the user in the configuration.
data CPUUsagePolicy =
    SingleCPU
    -- ^ Constrain the application to use one CPU.
    | MultipleCPUs
    -- ^ Permit the usage of multiple CPUs (the exact number is
    -- determined by the application).
    deriving (Eq, Show)

-- | The state of the UI diagnostic indicator for the async worker
-- thread.
data BackgroundInfo =
    Disabled
    -- ^ Disable (do not show) the indicator.
    | Active
    -- ^ Show the indicator when the thread is working.
    | ActiveCount
    -- ^ Show the indicator when the thread is working, but include the
    -- thread's work queue length.
    deriving (Eq, Show)

data UserPreferences =
    UserPreferences { _userPrefShowJoinLeave :: Bool
                    , _userPrefFlaggedPostList :: Seq FlaggedPost
                    , _userPrefGroupChannelPrefs :: HashMap ChannelId Bool
                    , _userPrefDirectChannelPrefs :: HashMap UserId Bool
                    , _userPrefFavoriteChannelPrefs :: HashMap ChannelId Bool
                    , _userPrefTeammateNameDisplayMode :: Maybe TeammateNameDisplayMode
                    , _userPrefTeamOrder :: Maybe [TeamId]
                    }

hasUnread :: ClientChannel -> Type -> Bool
hasUnread chan ty =
    let info = _ccInfo chan
        hasMentions = countMentions && _cdMentionCount info > 0
        hasNewMessages = _cdTotalMessageCount info > _cdViewedMessageCount info
        hasEditThreshold = isJust $ _cdEditedMessageThreshold info
        countMentions = case ty of
            Direct -> False
            Group -> False
            _ -> True
    in hasMentions ||
       (not (isMuted chan) && (hasNewMessages || hasEditThreshold))

mkChannelZipperList :: ChannelListSorting
                    -> UTCTime
                    -> Config
                    -> TeamId
                    -> Maybe ClientConfig
                    -> UserPreferences
                    -> HM.HashMap TeamId (Set ChannelListGroupLabel)
                    -> ClientChannels
                    -> Users
                    -> [(ChannelListGroup, [ChannelListEntry])]
mkChannelZipperList sorting now config tId cconfig prefs hidden cs us =
    let (privFavs, privEntries) = partitionFavorites $ getChannelEntriesByType tId prefs cs Private
        (normFavs, normEntries) = partitionFavorites $ getChannelEntriesByType tId prefs cs Ordinary
        (dmFavs,   dmEntries)   = partitionFavorites $ getDMChannelEntries now config cconfig prefs us cs
        favEntries              = privFavs <> normFavs <> dmFavs
        isHidden label =
            case HM.lookup tId hidden of
                Nothing -> False
                Just s -> Set.member label s
        mkGroup (ty, entries, sortEntries) =
            let unread = length $ filter channelListEntryUnread entries
                coll = isHidden ty
            in ( ChannelListGroup ty unread coll (length entries)
               , if coll then mempty else sortEntries entries
               )
    in mkGroup <$>
       [ (ChannelGroupFavoriteChannels, favEntries, sortChannelListEntries sorting)
       , (ChannelGroupPublicChannels, normEntries, sortChannelListEntries sorting)
       , (ChannelGroupPrivateChannels, privEntries, sortChannelListEntries sorting)
       , (ChannelGroupDirectMessages, dmEntries, sortDMChannelListEntries)
       ]

sortChannelListEntries :: ChannelListSorting -> [ChannelListEntry] -> [ChannelListEntry]
sortChannelListEntries ChannelListSortDefault =
    sortBy (comparing (\c -> (channelListEntryMuted c, channelListEntrySortValue c)))
sortChannelListEntries ChannelListSortUnreadFirst =
    sortBy (comparing (not . channelListEntryUnread)) .
    sortChannelListEntries ChannelListSortDefault

sortDMChannelListEntries :: [ChannelListEntry] -> [ChannelListEntry]
sortDMChannelListEntries = sortBy compareDMChannelListEntries

partitionFavorites :: [ChannelListEntry] -> ([ChannelListEntry], [ChannelListEntry])
partitionFavorites = partition channelListEntryFavorite

getChannelEntriesByType :: TeamId -> UserPreferences -> ClientChannels -> Type -> [ChannelListEntry]
getChannelEntriesByType tId prefs cs ty =
    let matches (_, info) = info^.ccInfo.cdType == ty &&
                            info^.ccInfo.cdTeamId == Just tId
        pairs = filteredChannels matches cs
        entries = mkEntry <$> pairs
        mkEntry (cId, ch) = ChannelListEntry { channelListEntryChannelId = cId
                                             , channelListEntryType = CLChannel
                                             , channelListEntryMuted = isMuted ch
                                             , channelListEntryUnread = hasUnread ch ty
                                             , channelListEntrySortValue = ch^.ccInfo.cdDisplayName.to T.toLower
                                             , channelListEntryFavorite = isFavorite prefs cId
                                             }
    in entries

getDMChannelEntries :: UTCTime
                    -> Config
                    -> Maybe ClientConfig
                    -> UserPreferences
                    -> Users
                    -> ClientChannels
                    -> [ChannelListEntry]
getDMChannelEntries now config cconfig prefs us cs =
    let oneOnOneDmChans = getSingleDMChannelEntries now config cconfig prefs us cs
        groupChans = getGroupDMChannelEntries now config prefs cs
    in groupChans <> oneOnOneDmChans

compareDMChannelListEntries :: ChannelListEntry -> ChannelListEntry -> Ordering
compareDMChannelListEntries e1 e2 =
    let u1 = channelListEntryUnread e1
        u2 = channelListEntryUnread e2
        n1 = channelListEntrySortValue e1
        n2 = channelListEntrySortValue e2
    in if u1 == u2
       then compare n1 n2
       else if u1 && not u2
            then LT
            else GT

useNickname' :: Maybe ClientConfig -> UserPreferences -> Bool
useNickname' clientConfig prefs =
    let serverSetting = case clientConfig^?_Just.to clientConfigTeammateNameDisplay of
            Just TMNicknameOrFullname -> Just True
            _                         -> Nothing
        accountSetting = (== TMNicknameOrFullname) <$> (_userPrefTeammateNameDisplayMode prefs)
        fallback = False
    in fromMaybe fallback $ accountSetting <|> serverSetting

displayNameForUser :: UserInfo -> Maybe ClientConfig -> UserPreferences -> Text
displayNameForUser u clientConfig prefs
    | useNickname' clientConfig prefs =
        fromMaybe (u^.uiName) (u^.uiNickName)
    | otherwise =
        u^.uiName

getGroupDMChannelEntries :: UTCTime
                         -> Config
                         -> UserPreferences
                         -> ClientChannels
                         -> [ChannelListEntry]
getGroupDMChannelEntries now config prefs cs =
    let matches (_, info) = info^.ccInfo.cdType == Group &&
                            info^.ccInfo.cdTeamId == Nothing &&
                            groupChannelShouldAppear now config prefs info
    in fmap (\(cId, ch) -> ChannelListEntry { channelListEntryChannelId = cId
                                            , channelListEntryType = CLGroupDM
                                            , channelListEntryMuted = isMuted ch
                                            , channelListEntryUnread = hasUnread ch Group
                                            , channelListEntrySortValue = ch^.ccInfo.cdDisplayName
                                            , channelListEntryFavorite = isFavorite prefs cId
                                            }) $
       filteredChannels matches cs

getSingleDMChannelEntries :: UTCTime
                          -> Config
                          -> Maybe ClientConfig
                          -> UserPreferences
                          -> Users
                          -> ClientChannels
                          -> [ChannelListEntry]
getSingleDMChannelEntries now config cconfig prefs us cs =
    let mapping = allDmChannelMappings cs
        mappingWithUserInfo = catMaybes $ getInfo <$> mapping
        getInfo (uId, cId) = do
            c <- findChannelById cId cs
            u <- findUserById uId us
            case u^.uiDeleted of
                True -> Nothing
                False ->
                    if dmChannelShouldAppear now config prefs c
                    then return (ChannelListEntry { channelListEntryChannelId = cId
                                                  , channelListEntryType = CLUserDM uId
                                                  , channelListEntryMuted = isMuted c
                                                  , channelListEntryUnread = hasUnread c Direct
                                                  , channelListEntrySortValue = displayNameForUser u cconfig prefs
                                                  , channelListEntryFavorite = isFavorite prefs cId
                                                  })
                    else Nothing
    in mappingWithUserInfo

-- | Return whether the specified channel has been marked as a favorite
-- channel.
isFavorite :: UserPreferences -> ChannelId -> Bool
isFavorite prefs cId = favoriteChannelPreference prefs cId == Just True

-- Always show a DM channel if it has unread activity or has been marked
-- as a favorite.
--
-- If it has no unread activity and if the preferences explicitly say to
-- hide it, hide it.
--
-- Otherwise, only show it if at least one of the other conditions are
-- met (see 'or' below).
dmChannelShouldAppear :: UTCTime -> Config -> UserPreferences -> ClientChannel -> Bool
dmChannelShouldAppear now config prefs c =
    let ndays = configDirectChannelExpirationDays config
        localCutoff = addUTCTime (nominalDay * (-(fromIntegral ndays))) now
        cutoff = ServerTime localCutoff
        updated = c^.ccInfo.cdUpdated
        uId = fromJust $ c^.ccInfo.cdDMUserId
        cId = c^.ccInfo.cdChannelId
    in if isFavorite prefs cId
       then True
       else (if hasUnread c Direct || maybe False (>= localCutoff) (c^.ccInfo.cdSidebarShowOverride)
             then True
             else case dmChannelShowPreference prefs uId of
                    Just False -> False
                    _ -> or [
                                -- The channel was updated recently enough
                                updated >= cutoff
                            ])

-- Always show a group DM channel if it has unread activity or has been
-- marked as a favorite.
--
-- If it has no unread activity and if the preferences explicitly say to
-- hide it, hide it.
--
-- Otherwise, only show it if at least one of the other conditions are
-- met (see 'or' below).
groupChannelShouldAppear :: UTCTime -> Config -> UserPreferences -> ClientChannel -> Bool
groupChannelShouldAppear now config prefs c =
    let ndays = configDirectChannelExpirationDays config
        localCutoff = addUTCTime (nominalDay * (-(fromIntegral ndays))) now
        cutoff = ServerTime localCutoff
        updated = c^.ccInfo.cdUpdated
        cId = c^.ccInfo.cdChannelId
    in if isFavorite prefs cId
       then True
       else (if hasUnread c Group || maybe False (>= localCutoff) (c^.ccInfo.cdSidebarShowOverride)
             then True
             else case groupChannelShowPreference prefs cId of
                    Just False -> False
                    _ -> or [
                                -- The channel was updated recently enough
                                updated >= cutoff
                            ])

dmChannelShowPreference :: UserPreferences -> UserId -> Maybe Bool
dmChannelShowPreference ps uId = HM.lookup uId (_userPrefDirectChannelPrefs ps)

groupChannelShowPreference :: UserPreferences -> ChannelId -> Maybe Bool
groupChannelShowPreference ps cId = HM.lookup cId (_userPrefGroupChannelPrefs ps)

favoriteChannelPreference :: UserPreferences -> ChannelId -> Maybe Bool
favoriteChannelPreference ps cId = HM.lookup cId (_userPrefFavoriteChannelPrefs ps)

-- | Types that provide a "semantically equal" operation. Two values may
-- be semantically equal even if they are not equal according to Eq if,
-- for example, they are equal on the basis of some fields that are more
-- pertinent than others.
class (Show a, Eq a, Ord a) => SemEq a where
    semeq :: a -> a -> Bool

instance SemEq Name where
    semeq (ClickableURL mId1 r1 _ t1) (ClickableURL mId2 r2 _ t2) = mId1 == mId2 && t1 == t2 && r1 == r2
    semeq (ClickableUsername mId1 r1 _ n) (ClickableUsername mId2 r2 _ n2) = mId1 == mId2 && n == n2 && r1 == r2
    semeq a b = a == b

instance SemEq a => SemEq (Maybe a) where
    semeq Nothing Nothing = True
    semeq (Just a) (Just b) = a `semeq` b
    semeq _ _ = False

-- | The sum type of exceptions we expect to encounter on authentication
-- failure. We encode them explicitly here so that we can print them in
-- a more user-friendly manner than just 'show'.
data AuthenticationException =
    ConnectError HostCannotConnect
    | ResolveError HostNotResolved
    | AuthIOError IOError
    | LoginError LoginFailureException
    | MattermostServerError MattermostError
    | OtherAuthError SomeException
    deriving (Show)

-- | Our 'ConnectionInfo' contains exactly as much information as is
-- necessary to start a connection with a Mattermost server. This is
-- built up during interactive authentication and then is used to log
-- in.
--
-- If the access token field is non-empty, that value is used and the
-- username and password values are ignored.
data ConnectionInfo =
    ConnectionInfo { _ciHostname :: Text
                   , _ciPort     :: Int
                   , _ciUrlPath  :: Text
                   , _ciUsername :: Text
                   , _ciOTPToken :: Maybe Text
                   , _ciPassword :: Text
                   , _ciAccessToken :: Text
                   , _ciType     :: ConnectionType
                   }

-- | We want to continue referring to posts by their IDs, but we don't
-- want to have to synthesize new valid IDs for messages from the client
-- itself (like error messages or informative client responses). To that
-- end, a PostRef can be either a PostId or a newly-generated client ID.
data PostRef
    = MMId PostId
    | CLId Int
    deriving (Eq, Show)

-- ** Channel-matching types

data ChannelSelectPattern = CSP MatchType Text
                          | CSPAny
                          deriving (Eq, Show)

data MatchType =
    Prefix
    | Suffix
    | Infix
    | Equal
    | PrefixDMOnly
    | PrefixNonDMOnly
    deriving (Eq, Show)

-- * Application State Values

data ProgramOutput =
    ProgramOutput { program :: FilePath
                  , programArgs :: [String]
                  , programStdout :: String
                  , programStderr :: String
                  , programExitCode :: ExitCode
                  }

defaultUserPreferences :: UserPreferences
defaultUserPreferences =
    UserPreferences { _userPrefShowJoinLeave     = True
                    , _userPrefFlaggedPostList   = mempty
                    , _userPrefGroupChannelPrefs = mempty
                    , _userPrefDirectChannelPrefs = mempty
                    , _userPrefFavoriteChannelPrefs = mempty
                    , _userPrefTeammateNameDisplayMode = Nothing
                    , _userPrefTeamOrder = Nothing
                    }

setUserPreferences :: Seq Preference -> UserPreferences -> UserPreferences
setUserPreferences = flip (F.foldr go)
    where go p u
            | Just fp <- preferenceToFlaggedPost p =
              u { _userPrefFlaggedPostList =
                  _userPrefFlaggedPostList u Seq.|> fp
                }
            | Just gp <- preferenceToDirectChannelShowStatus p =
              u { _userPrefDirectChannelPrefs =
                  HM.insert
                    (directChannelShowUserId gp)
                    (directChannelShowValue gp)
                    (_userPrefDirectChannelPrefs u)
                }
            | Just gp <- preferenceToGroupChannelPreference p =
              u { _userPrefGroupChannelPrefs =
                  HM.insert
                    (groupChannelId gp)
                    (groupChannelShow gp)
                    (_userPrefGroupChannelPrefs u)
                }
            | Just fp <- preferenceToFavoriteChannelPreference p =
              u { _userPrefFavoriteChannelPrefs =
                  HM.insert
                    (favoriteChannelId fp)
                    (favoriteChannelShow fp)
                    (_userPrefFavoriteChannelPrefs u)
                }
            | Just tIds <- preferenceToTeamOrder p =
              u { _userPrefTeamOrder = Just tIds
                }
            | preferenceName p == PreferenceName "join_leave" =
              u { _userPrefShowJoinLeave =
                  preferenceValue p /= PreferenceValue "false" }
            | preferenceCategory p == PreferenceCategoryDisplaySettings &&
              preferenceName p == PreferenceName "name_format" =
                  let PreferenceValue txt = preferenceValue p
                  in u { _userPrefTeammateNameDisplayMode = Just $ teammateDisplayModeFromText txt }
            | otherwise = u

-- | Log message tags.
data LogCategory =
    LogGeneral
    | LogAPI
    | LogWebsocket
    | LogError
    | LogUserMark
    | LogAsyncWork
    deriving (Eq, Show)

-- | A log message.
data LogMessage =
    LogMessage { logMessageText :: !Text
               -- ^ The text of the log message.
               , logMessageContext :: !(Maybe LogContext)
               -- ^ The optional context information relevant to the log
               -- message.
               , logMessageCategory :: !LogCategory
               -- ^ The category of the log message.
               , logMessageTimestamp :: !UTCTime
               -- ^ The timestamp of the log message.
               }
               deriving (Eq, Show)

-- | A logging thread command.
data LogCommand =
    LogToFile FilePath
    -- ^ Start logging to the specified path.
    | LogAMessage !LogMessage
    -- ^ Log the specified message.
    | StopLogging
    -- ^ Stop any active logging.
    | ShutdownLogging
    -- ^ Shut down.
    | GetLogDestination
    -- ^ Ask the logging thread about its active logging destination.
    | LogSnapshot FilePath
    -- ^ Ask the logging thread to dump the current buffer to the
    -- specified destination.
    deriving (Show)

-- | A handle to the log manager thread.
data LogManager =
    LogManager { logManagerCommandChannel :: STM.TChan LogCommand
               , logManagerHandle :: Async ()
               }

startLoggingToFile :: LogManager -> FilePath -> IO ()
startLoggingToFile mgr loc = sendLogCommand mgr $ LogToFile loc

stopLoggingToFile :: LogManager -> IO ()
stopLoggingToFile mgr = sendLogCommand mgr StopLogging

requestLogSnapshot :: LogManager -> FilePath -> IO ()
requestLogSnapshot mgr path = sendLogCommand mgr $ LogSnapshot path

requestLogDestination :: LogManager -> IO ()
requestLogDestination mgr = sendLogCommand mgr GetLogDestination

sendLogMessage :: LogManager -> LogMessage -> IO ()
sendLogMessage mgr lm = sendLogCommand mgr $ LogAMessage lm

sendLogCommand :: LogManager -> LogCommand -> IO ()
sendLogCommand mgr c =
    STM.atomically $ STM.writeTChan (logManagerCommandChannel mgr) c

-- | 'ChatResources' represents configuration and connection-related
-- information, as opposed to current model or view information.
-- Information that goes in the 'ChatResources' value should be limited
-- to information that we read or set up prior to setting up the bulk of
-- the application state.
data ChatResources =
    ChatResources { _crSession             :: Session
                  , _crWebsocketThreadId   :: Maybe ThreadId
                  , _crConn                :: ConnectionData
                  , _crRequestQueue        :: RequestChan
                  , _crEventQueue          :: BCH.BChan MHEvent
                  , _crSubprocessLog       :: STM.TChan ProgramOutput
                  , _crWebsocketActionChan :: STM.TChan WebsocketAction
                  , _crTheme               :: AttrMap
                  , _crThemeOriginal       :: Theme
                  , _crStatusUpdateChan    :: STM.TChan [UserId]
                  , _crConfiguration       :: Config
                  , _crFlaggedPosts        :: Set PostId
                  , _crUserPreferences     :: UserPreferences
                  , _crSyntaxMap           :: SyntaxMap
                  , _crLogManager          :: LogManager
                  , _crEmoji               :: EmojiCollection
                  , _crSpellChecker        :: Maybe Aspell
                  , _crWindowSize          :: (Int, Int)
                  }

-- | The 'GlobalEditState' value contains state not specific to any
-- single editor.
data GlobalEditState =
    GlobalEditState { _gedYankBuffer :: Text
                    }

emptyGlobalEditState :: GlobalEditState
emptyGlobalEditState =
    GlobalEditState { _gedYankBuffer   = ""
                    }

-- | A 'RequestChan' is a queue of operations we have to perform in the
-- background to avoid blocking on the main loop
type RequestChan = STM.TChan (IO (Maybe Work))

-- | Help topics
data HelpTopic =
    HelpTopic { helpTopicName         :: Text
              , helpTopicDescription  :: Text
              , helpTopicScreen       :: HelpScreen
              }
              deriving (Eq, Show)

-- | Mode type for the current contents of the post list window
data PostListContents =
    PostListFlagged
    | PostListPinned ChannelId
    | PostListSearch Text Bool -- for the query and search status
    deriving (Eq, Show)

-- | The 'Mode' represents the current dominant UI activity
data Mode =
    Main
    | ShowHelp HelpTopic
    | ChannelSelect
    | LeaveChannelConfirm
    | DeleteChannelConfirm
    | MessageSelectDeleteConfirm MessageInterfaceTarget
    | PostListWindow PostListContents
    | UserListWindow
    | ReactionEmojiListWindow
    | ChannelListWindow
    | ThemeListWindow
    | ViewMessage
    | EditNotifyPrefs
    | ChannelTopicWindow
    deriving (Eq, Show)

-- | We're either connected or we're not.
data ConnectionStatus = Connected | Disconnected deriving (Eq)

type ThreadInterface = MessageInterface Name PostId
type ChannelMessageInterface = MessageInterface Name ()

data ChannelListOrientation =
    ChannelListLeft
    -- ^ Show the channel list to the left of the message area.
    | ChannelListRight
    -- ^ Show the channel list to the right of the message area.
    deriving (Eq, Show)

data ThreadOrientation =
    ThreadBelow
    -- ^ Show the thread below the channel message area.
    | ThreadAbove
    -- ^ Show the thread above the channel message area.
    | ThreadLeft
    -- ^ Show the thread to the left of the channel message area.
    | ThreadRight
    -- ^ Show the thread to the right of the channel message area.
    deriving (Eq, Show)

-- | This type represents the current state of our application at any
-- given time.
data ChatState =
    ChatState { _csResources :: ChatResources
              -- ^ Global application-wide resources that don't change
              -- much.
              , _csLastMouseDownEvent :: Maybe (Brick.BrickEvent Name MHEvent)
              -- ^ The most recent mouse click event we got. We reset
              -- this on mouse up so we can ignore clicks whenever this
              -- is already set.
              , _csVerbatimTruncateSetting :: Maybe Int
              -- ^ The current verbatim block truncation setting. This
              -- is used to toggle truncation behavior and is updated
              -- from the configTruncateVerbatimBlocks Config field.
              , _csTeams :: HashMap TeamId TeamState
              -- ^ The state for each team that we are in.
              , _csTeamZipper :: Z.Zipper () TeamId
              -- ^ The list of teams we can cycle through.
              , _csChannelListOrientation :: ChannelListOrientation
              -- ^ The orientation of the channel list.
              , _csMe :: User
              -- ^ The authenticated user.
              , _csChannels :: ClientChannels
              -- ^ The channels that we are showing, including their
              -- message lists.
              , _csHiddenChannelGroups :: HM.HashMap TeamId (Set ChannelListGroupLabel)
              -- ^ The set of channel list groups that are currently
              -- collapsed in the sidebar.
              , _csPostMap :: HashMap PostId Message
              -- ^ The map of post IDs to messages. This allows us to
              -- access messages by ID without having to linearly scan
              -- channel message lists.
              , _csUsers :: Users
              -- ^ All of the users we know about.
              , _timeZone :: TimeZoneSeries
              -- ^ The client time zone.
              , _csConnectionStatus :: ConnectionStatus
              -- ^ Our view of the connection status.
              , _csWorkerIsBusy :: Maybe (Maybe Int)
              -- ^ Whether the async worker thread is busy, and its
              -- queue length if so.
              , _csClientConfig :: Maybe ClientConfig
              -- ^ The Mattermost client configuration, as we understand it.
              , _csInputHistory :: InputHistory
              -- ^ The map of per-channel input history for the
              -- application. We don't distribute the per-channel
              -- history into the per-channel states (like we do
              -- for other per-channel state) since keeping it
              -- under the InputHistory banner lets us use a nicer
              -- startup/shutdown disk file management API.
              , _csGlobalEditState :: GlobalEditState
              -- ^ Bits of global state common to all editors.
              }

-- | All application state specific to a team, along with state specific
-- to our user interface's presentation of that team. We include the
-- UI state relevant to the team so that we can easily switch which
-- team the UI is presenting without having to reinitialize the UI from
-- the new team. This allows the user to be engaged in just about any
-- application activity while viewing a team, switch to another team,
-- and return to the original team and resume what they were doing, all
-- without us doing any work.
data TeamState =
    TeamState { _tsFocus :: Z.Zipper ChannelListGroup ChannelListEntry
              -- ^ The channel sidebar zipper that tracks which channel
              -- is selected.
              , _tsTeam :: Team
              -- ^ The team data.
              , _tsRecentChannel :: Maybe ChannelId
              -- ^ The most recently-selected channel, if any.
              , _tsReturnChannel :: Maybe ChannelId
              -- ^ The channel to return to after visiting one or more
              -- unread channels.
              , _tsModeStack :: NonemptyStack Mode
              -- ^ The current application mode stack when viewing this
              -- team. This is used to dispatch to different rendering
              -- and event handling routines. The current mode is always
              -- in at the top of the stack.
              , _tsChannelSelectState :: ChannelSelectState
              -- ^ The state of the user's input and selection for
              -- channel selection mode.
              , _tsPendingChannelChange :: Maybe PendingChannelChange
              -- ^ A pending channel change that we need to apply once
              -- the channel in question is available. We set this up
              -- when we need to change to a channel in the sidebar, but
              -- it isn't even there yet because we haven't loaded its
              -- metadata.
              , _tsViewedMessage :: Maybe (Message, TabbedWindow ChatState MH Name ViewMessageWindowTab)
              -- ^ Set when the ViewMessage mode is active. The message
              -- being viewed. Note that this stores a message, not
              -- a message ID. That's because not all messages have
              -- message IDs (e.g. client messages) and we still
              -- want to support viewing of those messages. It's the
              -- responsibility of code that uses this message to always
              -- consult the chat state for the latest *version* of any
              -- message with an ID here, to be sure that the latest
              -- version is used (e.g. if it gets edited, etc.).
              , _tsPostListWindow :: PostListWindowState
              -- ^ The state of the post list window.
              , _tsUserListWindow :: ListWindowState UserInfo UserSearchScope
              -- ^ The state of the user list window.
              , _tsChannelListWindow :: ListWindowState Channel ChannelSearchScope
              -- ^ The state of the user list window.
              , _tsNotifyPrefs :: Maybe (Form ChannelNotifyProps MHEvent Name)
              -- ^ A form for editing the notification preferences for
              -- the current channel. This is set when entering
              -- EditNotifyPrefs mode and updated when the user
              -- changes the form state.
              , _tsChannelTopicDialog :: ChannelTopicDialogState
              -- ^ The state for the interactive channel topic editor
              -- window.
              , _tsReactionEmojiListWindow :: ListWindowState (Bool, T.Text) ()
              -- ^ The state of the reaction emoji list window.
              , _tsThemeListWindow :: ListWindowState InternalTheme ()
              -- ^ The state of the theme list window.
              , _tsChannelListSorting :: ChannelListSorting
              -- ^ How to sort channels in this team's channel list
              -- groups
              , _tsThreadInterface :: Maybe ThreadInterface
              -- ^ The thread interface for this team for participating
              -- in a single thread
              , _tsMessageInterfaceFocus :: MessageInterfaceFocus
              -- ^ Which message interface is focused for editing input
              }

data MessageInterfaceFocus =
    FocusThread
    | FocusCurrentChannel
    deriving (Eq, Show)

messageInterfaceFocusList :: [MessageInterfaceFocus]
messageInterfaceFocusList =
    [ FocusCurrentChannel
    , FocusThread
    ]

messageInterfaceFocusNext :: TeamState -> TeamState
messageInterfaceFocusNext = messageInterfaceFocusWith messageInterfaceFocusList

messageInterfaceFocusPrev :: TeamState -> TeamState
messageInterfaceFocusPrev = messageInterfaceFocusWith (reverse messageInterfaceFocusList)

messageInterfaceFocusWith :: [MessageInterfaceFocus] -> TeamState -> TeamState
messageInterfaceFocusWith lst ts =
    let next = fromJust $ cycleElemAfter cur lst
        cur = _tsMessageInterfaceFocus ts
        noThread = isNothing $ _tsThreadInterface ts
        newFocus = if next == FocusThread && noThread
                   then fromJust $ cycleElemAfter FocusThread lst
                   else next
    in ts { _tsMessageInterfaceFocus = newFocus }

cycleElemAfter :: (Eq a) => a -> [a] -> Maybe a
cycleElemAfter e es =
    if e `notElem` es
    then Nothing
    else Just $ head $ drop 1 $ dropWhile (/= e) $ cycle es

-- | Handles for the View Message window's tabs.
data ViewMessageWindowTab =
    VMTabMessage
    -- ^ The message tab.
    | VMTabReactions
    -- ^ The reactions tab.
    | VMTabAuthorInfo
    -- ^ The author info tab.
    deriving (Eq, Show)

data PendingChannelChange =
    ChangeByChannelId TeamId ChannelId (Maybe (MH ()))
    | ChangeByUserId UserId

-- | Startup state information that is constructed prior to building a
-- ChatState.
data StartupStateInfo =
    StartupStateInfo { startupStateResources      :: ChatResources
                     , startupStateConnectedUser  :: User
                     , startupStateTeams          :: HM.HashMap TeamId TeamState
                     , startupStateTimeZone       :: TimeZoneSeries
                     , startupStateInitialHistory :: InputHistory
                     , startupStateInitialTeam    :: TeamId
                     }

-- | The state of the channel topic editor window.
data ChannelTopicDialogState =
    ChannelTopicDialogState { _channelTopicDialogEditor :: Editor T.Text Name
                            -- ^ The topic string editor state.
                            , _channelTopicDialogFocus :: FocusRing Name
                            -- ^ The window focus state (editor/buttons)
                            }

sortTeamsAlpha :: [Team] -> [Team]
sortTeamsAlpha =
    sortBy (compare `on` (T.strip . sanitizeUserText . teamName))

matchesTeam :: T.Text -> Team -> Bool
matchesTeam tName t =
    let normalizeUserText = normalize . sanitizeUserText
        normalize = T.strip . T.toLower
        urlName = normalizeUserText $ teamName t
        displayName = normalizeUserText $ teamDisplayName t
    in normalize tName `elem` [displayName, urlName]

mkTeamZipperFromIds :: [TeamId] -> Z.Zipper () TeamId
mkTeamZipperFromIds tIds = Z.fromList [((), tIds)]

teamZipperIds :: Z.Zipper () TeamId -> [TeamId]
teamZipperIds = concat . fmap snd . Z.toList

-- | The state of channel selection mode.
data ChannelSelectState =
    ChannelSelectState { _channelSelectInput :: Editor Text Name
                       , _channelSelectMatches :: Z.Zipper ChannelListGroup ChannelSelectMatch
                       }

emptyChannelSelectState :: TeamId -> ChannelSelectState
emptyChannelSelectState tId =
    ChannelSelectState { _channelSelectInput = editor (ChannelSelectInput tId) (Just 1) ""
                       , _channelSelectMatches = Z.fromList []
                       }

-- | The state of the post list window.
data PostListWindowState =
    PostListWindowState { _postListPosts    :: Messages
                        , _postListSelected :: Maybe PostId
                        }

data InternalTheme =
    InternalTheme { internalThemeName :: Text
                  , internalTheme :: Theme
                  , internalThemeDesc :: Text
                  }

-- | The state of the search result list window. Type 'a' is the type
-- of data in the list. Type 'b' is the search scope type.
data ListWindowState a b =
    ListWindowState { _listWindowSearchResults :: List Name a
                     -- ^ The list of search results currently shown in
                     -- the window.
                     , _listWindowSearchInput :: Editor Text Name
                     -- ^ The editor for the window's search input.
                     , _listWindowSearchScope :: b
                     -- ^ The window's current search scope.
                     , _listWindowSearching :: Bool
                     -- ^ Whether a search is in progress (i.e. whether
                     -- we are currently awaiting a response from a
                     -- search query to the server).
                     , _listWindowEnterHandler :: a -> MH Bool
                     -- ^ The handler to invoke on the selected element
                     -- when the user presses Enter.
                     , _listWindowNewList :: Vec.Vector a -> List Name a
                     -- ^ The function to build a new brick List from a
                     -- vector of search results.
                     , _listWindowFetchResults :: b -> Session -> Text -> IO (Vec.Vector a)
                     -- ^ The function to call to issue a search query
                     -- to the server.
                     , _listWindowRecordCount :: Maybe Int
                     -- ^ The total number of available records, if known.
                     }

-- | The scope for searching for users in a user list window.
data UserSearchScope =
    ChannelMembers ChannelId TeamId
    | ChannelNonMembers ChannelId TeamId
    | AllUsers (Maybe TeamId)

-- | The scope for searching for channels to join.
data ChannelSearchScope =
    AllChannels

-- | Actions that can be sent on the websocket to the server.
data WebsocketAction =
    UserTyping UTCTime ChannelId (Maybe PostId) -- ^ user typing in the input box
    deriving (Read, Show, Eq, Ord)

-- * MH Monad

-- | Logging context information, in the event that metadata should
-- accompany a log message.
data LogContext =
    LogContext { logContextChannelId :: Maybe ChannelId
               }
               deriving (Eq, Show)

-- | A user fetching strategy.
data UserFetch =
    UserFetchById UserId
    -- ^ Fetch the user with the specified ID.
    | UserFetchByUsername Text
    -- ^ Fetch the user with the specified username.
    | UserFetchByNickname Text
    -- ^ Fetch the user with the specified nickname.
    deriving (Eq, Show)

data MHState =
    MHState { mhUsersToFetch :: [UserFetch]
            , mhPendingStatusList :: Maybe [UserId]
            }

-- | A value of type 'MH' @a@ represents a computation that can
-- manipulate the application state and also request that the
-- application quit
newtype MH a =
    MH { fromMH :: R.ReaderT (Maybe LogContext) (St.StateT MHState (EventM Name ChatState)) a }

-- | Use a modified logging context for the duration of the specified MH
-- action.
withLogContext :: (Maybe LogContext -> Maybe LogContext) -> MH a -> MH a
withLogContext modifyContext act =
    MH $ R.withReaderT modifyContext (fromMH act)

withLogContextChannelId :: ChannelId -> MH a -> MH a
withLogContextChannelId cId act =
    let f Nothing = Just $ LogContext (Just cId)
        f (Just c) = Just $ c { logContextChannelId = Just cId }
    in withLogContext f act

-- | Get the current logging context.
getLogContext :: MH (Maybe LogContext)
getLogContext = MH R.ask

-- | Log a message.
mhLog :: LogCategory -> Text -> MH ()
mhLog cat msg = do
    logger <- mhGetIOLogger
    liftIO $ logger cat msg

-- | Get a logger suitable for use in IO. The logger always logs using
-- the MH monad log context at the time of the call to mhGetIOLogger.
mhGetIOLogger :: MH (LogCategory -> Text -> IO ())
mhGetIOLogger = do
    ctx <- getLogContext
    mgr <- use (to (_crLogManager . _csResources))
    return $ ioLogWithManager mgr ctx

ioLogWithManager :: LogManager -> Maybe LogContext -> LogCategory -> Text -> IO ()
ioLogWithManager mgr ctx cat msg = do
    now <- getCurrentTime
    let lm = LogMessage { logMessageText = msg
                        , logMessageContext = ctx
                        , logMessageCategory = cat
                        , logMessageTimestamp = now
                        }
    sendLogMessage mgr lm

-- | Run an 'MH' computation in 'EventM'.
runMHEvent :: MH () -> EventM Name ChatState ()
runMHEvent (MH mote) = do
  let mhSt = MHState { mhUsersToFetch = []
                     , mhPendingStatusList = Nothing
                     }
  void $ St.runStateT (R.runReaderT mote Nothing) mhSt

scheduleUserFetches :: [UserFetch] -> MH ()
scheduleUserFetches fs = MH $ do
    St.modify $ \s -> s { mhUsersToFetch = fs <> mhUsersToFetch s }

scheduleUserStatusFetches :: [UserId] -> MH ()
scheduleUserStatusFetches is = MH $ do
    St.modify $ \s -> s { mhPendingStatusList = Just is }

getScheduledUserFetches :: MH [UserFetch]
getScheduledUserFetches = MH $ St.gets mhUsersToFetch

getScheduledUserStatusFetches :: MH (Maybe [UserId])
getScheduledUserStatusFetches = MH $ St.gets mhPendingStatusList

-- | lift a computation in 'EventM' into 'MH'
mh :: EventM Name ChatState a -> MH a
mh = MH . R.lift . St.lift

generateUUID :: MH UUID
generateUUID = liftIO generateUUID_IO

generateUUID_IO :: IO UUID
generateUUID_IO = randomIO

mhZoom :: Lens' ChatState b -> (e -> EventM Name b ()) -> e -> MH ()
mhZoom ln f event = MH $ R.lift $ St.lift $ zoom ln (f event)

mhZoom' :: Lens' ChatState b -> (EventM Name b ()) -> MH ()
mhZoom' ln f = MH $ R.lift $ St.lift $ zoom ln f

mhSuspendAndResume :: (ChatState -> IO ChatState) -> MH ()
mhSuspendAndResume act = MH $ R.lift $ St.lift $ do
    st <- St.get
    Brick.suspendAndResume (act st)

mhContinueWithoutRedraw :: MH ()
mhContinueWithoutRedraw = MH $ R.lift $ St.lift $ Brick.continueWithoutRedraw

-- | This will request that after this computation finishes the
-- application should exit
requestQuit :: MH ()
requestQuit = MH $ R.lift $ St.lift $ Brick.halt

instance Functor MH where
    fmap f (MH x) = MH (fmap f x)

instance Applicative MH where
    pure x = MH (pure x)
    MH f <*> MH x = MH (f <*> x)

instance Monad MH where
    return = pure
    MH x >>= f = MH (x >>= \ x' -> fromMH (f x'))

instance St.MonadState ChatState MH where
    get = MH $ R.lift $ St.lift St.get
    put = MH . R.lift . St.lift . St.put

instance St.MonadIO MH where
    liftIO = MH . St.liftIO

data Work = Work String (MH ())

-- | This represents events that we handle in the main application loop.
data MHEvent =
    WSEvent WebsocketEvent
    -- ^ For events that arise from the websocket
    | WSActionResponse WebsocketActionResponse
    -- ^ For responses to websocket actions
    | RespEvent Work
    -- ^ For the result values of async operations
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
    | RateLimitExceeded Int
    -- ^ A request initially failed due to a rate limit but will be
    -- retried if possible. The argument is the number of seconds in
    -- which the retry will be attempted.
    | RateLimitSettingsMissing
    -- ^ A request denied by a rate limit could not be retried because
    -- the response contained no rate limit metadata
    | RequestDropped
    -- ^ A request was reattempted due to a rate limit and was rate
    -- limited again
    | IEvent InternalEvent
    -- ^ MH-internal events

-- | Internal application events.
data InternalEvent =
    DisplayError MHError
    -- ^ Some kind of application error occurred
    | LoggingStarted FilePath
    | LoggingStopped FilePath
    | LogStartFailed FilePath String
    | LogDestination (Maybe FilePath)
    | LogSnapshotSucceeded FilePath
    | LogSnapshotFailed FilePath String
    -- ^ Logging events from the logging thread

-- | Application errors.
data MHError =
    GenericError T.Text
    -- ^ A generic error message constructor
    | NoSuchChannel T.Text
    -- ^ The specified channel does not exist
    | NoSuchUser T.Text
    -- ^ The specified user does not exist
    | AmbiguousName T.Text
    -- ^ The specified name matches both a user and a channel
    | ServerError MattermostError
    -- ^ A Mattermost server error occurred
    | ClipboardError T.Text
    -- ^ A problem occurred trying to deal with yanking or the system
    -- clipboard
    | ConfigOptionMissing T.Text
    -- ^ A missing config option is required to perform an operation
    | ProgramExecutionFailed T.Text T.Text
    -- ^ Args: program name, path to log file. A problem occurred when
    -- running the program.
    | NoSuchScript T.Text
    -- ^ The specified script was not found
    | NoSuchHelpTopic T.Text
    -- ^ The specified help topic was not found
    | AttachmentException SomeException
    -- ^ IO operations for attaching a file threw an exception
    | BadAttachmentPath T.Text
    -- ^ The specified file is either a directory or doesn't exist
    | AsyncErrEvent SomeException
    -- ^ For errors that arise in the course of async IO operations
    deriving (Show)

-- ** Application State Lenses

makeLenses ''ChatResources
makeLenses ''ChatState
makeLenses ''TeamState
makeLenses ''GlobalEditState
makeLenses ''PostListWindowState
makeLenses ''ListWindowState
makeLenses ''ChannelSelectState
makeLenses ''UserPreferences
makeLenses ''ConnectionInfo
makeLenses ''ChannelTopicDialogState
Brick.suffixLenses ''Config

-- | Given a list of event handlers and an event, try to handle the
-- event with the handlers in the specified order. If a handler returns
-- False (indicating it did not handle the event), try the next handler
-- until either a handler returns True or all handlers are tried.
-- Returns True if any handler handled the event or False otherwise.
handleEventWith :: [Vty.Event -> MH Bool] -> Vty.Event -> MH Bool
handleEventWith [] _ =
    return False
handleEventWith (handler:rest) e = do
    handled <- handler e
    if handled
       then return True
       else handleEventWith rest e

applyTeamOrderPref :: Config -> Maybe [TeamId] -> ChatState -> ChatState
applyTeamOrderPref cfg mPrefTIds st =
    let teams = _csTeams st
        ourTids = HM.keys teams
        tIds = case mPrefTIds of
            Nothing -> []
            Just prefTIds -> filter (`elem` ourTids) prefTIds
        curTId = st^.csCurrentTeamId
        unmentioned = filter (not . wasMentioned) $ HM.elems teams
        wasMentioned ts = (teamId $ _tsTeam ts) `elem` tIds
        zipperTidsBeforeConfigSort = tIds <> (teamId <$> sortTeamsAlpha (_tsTeam <$> unmentioned))
        zipperTids = applyTeamSorting st (cfg^.configTeamListSortingL) zipperTidsBeforeConfigSort
    in st { _csTeamZipper = (Z.findRight ((== curTId) . Just) $ mkTeamZipperFromIds zipperTids)
          }

applyTeamSorting :: ChatState -> TeamListSorting -> [TeamId] -> [TeamId]
applyTeamSorting _ TeamListSortDefault tIds = tIds
applyTeamSorting st TeamListSortUnreadFirst tIds =
    let withCount tId = (tId, teamUnreadCount tId st)
        withCounts = withCount <$> tIds
        unreadFirst = sortBy (comparing (Down . (> 0) . snd)) withCounts
    in fst <$> unreadFirst

refreshTeamZipper :: MH ()
refreshTeamZipper = do
    config <- use (csResources.crConfiguration)
    tidOrder <- use (csResources.crUserPreferences.userPrefTeamOrder)
    St.modify (applyTeamOrderPref config tidOrder)

applyTeamOrder :: [TeamId] -> MH ()
applyTeamOrder tIds = do
    config <- use (csResources.crConfiguration)
    St.modify (applyTeamOrderPref config $ Just tIds)

newState :: StartupStateInfo -> ChatState
newState (StartupStateInfo {..}) =
    let config = _crConfiguration startupStateResources
    in applyTeamOrderPref config (_userPrefTeamOrder $ _crUserPreferences startupStateResources) $
       ChatState { _csResources                   = startupStateResources
                 , _csLastMouseDownEvent          = Nothing
                 , _csGlobalEditState             = emptyGlobalEditState
                 , _csVerbatimTruncateSetting     = configTruncateVerbatimBlocks config
                 , _csTeamZipper                  = Z.findRight (== startupStateInitialTeam) $
                                                    mkTeamZipperFromIds $ teamId <$> (_tsTeam <$> snd <$> HM.toList startupStateTeams)
                 , _csTeams                       = startupStateTeams
                 , _csChannelListOrientation      = configChannelListOrientation config
                 , _csMe                          = startupStateConnectedUser
                 , _csChannels                    = noChannels
                 , _csPostMap                     = HM.empty
                 , _csUsers                       = noUsers
                 , _timeZone                      = startupStateTimeZone
                 , _csConnectionStatus            = Connected
                 , _csWorkerIsBusy                = Nothing
                 , _csClientConfig                = Nothing
                 , _csInputHistory                = startupStateInitialHistory
                 , _csHiddenChannelGroups         = mempty
                 }

getServerBaseUrl :: TeamId -> MH TeamBaseURL
getServerBaseUrl tId = do
    st <- use id
    return $ serverBaseUrl st tId

serverBaseUrl :: ChatState -> TeamId -> TeamBaseURL
serverBaseUrl st tId =
    let baseUrl = connectionDataURL $ _crConn $ _csResources st
        tName = teamName $ st^.csTeam(tId).tsTeam
    in TeamBaseURL (TeamURLName $ sanitizeUserText tName) baseUrl

getSession :: MH Session
getSession = use (csResources.crSession)

getResourceSession :: ChatResources -> Session
getResourceSession = _crSession

whenMode :: TeamId -> Mode -> MH () -> MH ()
whenMode tId m act = do
    curMode <- top <$> use (csTeam(tId).tsModeStack)
    when (curMode == m) act

pushMode :: TeamId -> Mode -> MH ()
pushMode tId m = do
    St.modify (pushMode' tId m)
    mh invalidateCache

replaceMode :: TeamId -> Mode -> MH ()
replaceMode tId m = popMode tId >> pushMode tId m

popMode :: TeamId -> MH ()
popMode tId = do
    s <- use (csTeam(tId).tsModeStack)
    let (s', topVal) = pop s
    case topVal of
        Nothing -> return ()
        Just _ -> do
            csTeam(tId).tsModeStack .= s'
            mh invalidateCache

pushMode' :: TeamId -> Mode -> ChatState -> ChatState
pushMode' tId m st =
    let s = st^.csTeam(tId).tsModeStack
    in if top s == m
       then st
       else st & csTeam(tId).tsModeStack %~ (push m)

-- ** Utility Lenses
csCurrentChannelId :: TeamId -> SimpleGetter ChatState (Maybe ChannelId)
csCurrentChannelId tId =
    csTeam(tId).tsFocus.to Z.focus.to (fmap channelListEntryChannelId)

teamUnreadCount :: TeamId -> ChatState -> Int
teamUnreadCount tId st =
    sum $ fmap (nonDMChannelListGroupUnread . fst) $
          Z.toList $
          st^.csTeam(tId).tsFocus

withCurrentTeam :: (TeamId -> MH ()) -> MH ()
withCurrentTeam f = do
    mtId <- use csCurrentTeamId
    case mtId of
        Nothing -> return ()
        Just tId -> f tId

forEachTeam :: (TeamId -> MH ()) -> MH ()
forEachTeam f = do
    ts <- use csTeams
    mapM_ f (HM.keys ts)

withCurrentChannel :: TeamId -> (ChannelId -> ClientChannel -> MH ()) -> MH ()
withCurrentChannel tId f = do
    mcId <- use $ csCurrentChannelId tId
    case mcId of
        Nothing -> return ()
        Just cId -> do
            mChan <- preuse $ csChannel cId
            case mChan of
                Just ch -> f cId ch
                _ -> return ()

withCurrentChannel' :: TeamId -> (ChannelId -> ClientChannel -> MH (Maybe a)) -> MH (Maybe a)
withCurrentChannel' tId f = do
    mcId <- use $ csCurrentChannelId tId
    case mcId of
        Nothing -> return Nothing
        Just cId -> do
            mChan <- preuse $ csChannel cId
            case mChan of
                Just ch -> f cId ch
                _ -> return Nothing

csCurrentTeamId :: SimpleGetter ChatState (Maybe TeamId)
csCurrentTeamId = csTeamZipper.to Z.focus

csChannelMessageInterface :: ChannelId -> Lens' ChatState ChannelMessageInterface
csChannelMessageInterface cId =
    csChannels.maybeChannelByIdL cId.singular _Just.ccMessageInterface

maybeChannelMessageInterface :: ChannelId -> Traversal' ChatState ChannelMessageInterface
maybeChannelMessageInterface cId =
    csChannels.maybeChannelByIdL cId._Just.ccMessageInterface

channelEditor :: ChannelId -> Lens' ChatState (EditState Name)
channelEditor cId =
    csChannels.maybeChannelByIdL cId.singular _Just.ccMessageInterface.miEditor

channelMessageSelect :: ChannelId -> Lens' ChatState MessageSelectState
channelMessageSelect cId =
    csChannels.maybeChannelByIdL cId.singular _Just.ccMessageInterface.miMessageSelect

csTeam :: TeamId -> Lens' ChatState TeamState
csTeam tId =
    lens (\ st -> st ^. csTeams . at tId ^?! _Just)
         (\ st t -> st & csTeams . at tId .~ Just t)

teamMode :: TeamState -> Mode
teamMode = top . _tsModeStack

teamModes :: TeamState -> [Mode]
teamModes = stackToList . _tsModeStack

getTeamMode :: TeamId -> MH Mode
getTeamMode tId = teamMode <$> use (csTeam(tId))

channelListEntryUserId :: ChannelListEntry -> Maybe UserId
channelListEntryUserId e =
    case channelListEntryType e of
        CLUserDM uId -> Just uId
        _ -> Nothing

userIdsFromZipper :: Z.Zipper ChannelListGroup ChannelListEntry -> [UserId]
userIdsFromZipper z =
    concat $ (catMaybes . fmap channelListEntryUserId . snd) <$> Z.toList z

entryIsDMEntry :: ChannelListEntry -> Bool
entryIsDMEntry e =
    case channelListEntryType e of
        CLUserDM {} -> True
        CLGroupDM {} -> True
        CLChannel {} -> False

csChannel :: ChannelId -> Traversal' ChatState ClientChannel
csChannel cId =
    csChannels . channelByIdL cId

csChannelMessages :: ChannelId -> Traversal' ChatState Messages
csChannelMessages cId =
    csChannelMessageInterface(cId).miMessages

withChannel :: ChannelId -> (ClientChannel -> MH ()) -> MH ()
withChannel cId = withChannelOrDefault cId ()

withChannelOrDefault :: ChannelId -> a -> (ClientChannel -> MH a) -> MH a
withChannelOrDefault cId deflt mote = do
    chan <- preuse (csChannel(cId))
    case chan of
        Nothing -> return deflt
        Just c  -> mote c

type MHKeyEventHandler = KeyEventHandler KeyEvent MH

mhHandleKeyboardEvent :: (KeyConfig KeyEvent -> KeyDispatcher KeyEvent MH)
                      -- ^ The function to build a key handler map from
                      -- a key configuration.
                      -> Vty.Event
                      -- ^ The event to handle.
                      -> MH Bool
mhHandleKeyboardEvent mkDispatcher (Vty.EvKey k mods) = do
    config <- use (csResources.crConfiguration)
    handleKey (mkDispatcher $ configUserKeys config) k mods
mhHandleKeyboardEvent _ _ =
    return False

-- | Create a key dispatcher, but convert errors about conflict bindings
-- into a runtime exception with 'error'. Where we use this, it's safe
-- to use because we do a startup check for keybinding conflicts in
-- most application modes, which means that by the time we get around
-- to calling this function, the modes have already been checked and
-- have been found free of conflicts. However, there could be situations
-- in the future where we can't detect collisions at startup due to
-- dynamically built handler lists. In those cases, this would cause
-- the program to crash with a detailed error about the conflicting key
-- binding.
unsafeKeyDispatcher :: (Ord k) => KeyConfig k -> [KeyEventHandler k m] -> KeyDispatcher k m
unsafeKeyDispatcher cfg hs =
    case keyDispatcher cfg hs of
        Right d -> d
        Left conflicts ->
            error $ T.unpack $ "Error: conflicting key bindings:\n" <>
                               bindingConflictMessage cfg conflicts

bindingConflictMessage :: (Ord k) => KeyConfig k -> [(Binding, [KeyHandler k m])] -> T.Text
bindingConflictMessage cfg conflicts = msg
    where
        msg = T.intercalate "\n" sections
        sections = mkSection <$> conflicts
        mkSection (key, handlers) =
            let handlerLines = handlerLine <$> handlers
                handlerLine h =
                    let desc = handlerDescription $ kehHandler $ khHandler h
                        trigger = case kehEventTrigger $ khHandler h of
                            ByEvent e -> "event '" <> fromJust (keyEventName (keyConfigEvents cfg) e) <> "'"
                            ByKey b -> "fixed key '" <> ppBinding b <> "'"
                    in "  '" <> desc <> "', triggered by " <> trigger
            in T.intercalate "\n" $ [ "Conflicting key binding: " <> ppBinding key
                                    , "Handlers:"
                                    ] <> handlerLines

-- ** 'ChatState' Helper Functions

raiseInternalEvent :: InternalEvent -> MH ()
raiseInternalEvent ev = do
    queue <- use (csResources.crEventQueue)
    writeBChan queue (IEvent ev)

writeBChan :: (MonadIO m) => BCH.BChan MHEvent -> MHEvent -> m ()
writeBChan chan e = void $ liftIO $ BCH.writeBChanNonBlocking chan e

-- | Log and raise an error.
mhError :: MHError -> MH ()
mhError err = do
    mhLog LogError $ T.pack $ show err
    raiseInternalEvent (DisplayError err)

isMine :: ChatState -> Message -> Bool
isMine st msg =
    case msg^.mUser of
        UserI _ uid -> uid == myUserId st
        _ -> False

getMessageForPostId :: ChatState -> PostId -> Maybe Message
getMessageForPostId st pId = st^.csPostMap.at(pId)

getParentMessage :: ChatState -> Message -> Maybe Message
getParentMessage st msg
    | InReplyTo pId <- msg^.mInReplyToMsg
      = st^.csPostMap.at(pId)
    | otherwise = Nothing

getReplyRootMessage :: Message -> MH Message
getReplyRootMessage msg = do
    case postRootId =<< (msg^.mOriginalPost) of
        Nothing -> return msg
        Just rootId -> do
            st <- use id
            case getMessageForPostId st rootId of
                -- NOTE: this case should never happen. This is the
                -- case where a message has a root post ID but we
                -- don't have a copy of the root post in storage. This
                -- shouldn't happen because whenever we add a message
                -- to a channel, we always fetch the parent post and
                -- store it if it is in a thread. That should mean that
                -- whenever we reply to a post, if that post is itself
                -- a reply, we should have its root post in storage
                -- and this case should never match. Even though it
                -- shouldn't happen, rather than raising a BUG exception
                -- here we'll just fall back to the input message.
                Nothing -> return msg
                Just m -> return m

setUserStatus :: UserId -> Text -> MH ()
setUserStatus uId t = do
    csUsers %= modifyUserById uId (uiStatus .~ statusFromText t)
    cs <- use csChannels
    forM_ (allTeamIds cs) $ \tId ->
        mh $ invalidateCacheEntry $ ChannelSidebar tId

usernameForUserId :: UserId -> ChatState -> Maybe Text
usernameForUserId uId st = _uiName <$> findUserById uId (st^.csUsers)

displayNameForUserId :: UserId -> ChatState -> Maybe Text
displayNameForUserId uId st = do
    u <- findUserById uId (st^.csUsers)
    return $ displayNameForUser u (st^.csClientConfig) (st^.csResources.crUserPreferences)

-- | Note: this only searches users we have already loaded. Be
-- aware that if you think you need a user we haven't fetched, use
-- withFetchedUser!
userIdForUsername :: Text -> ChatState -> Maybe UserId
userIdForUsername name st =
    fst <$> (findUserByUsername name $ st^.csUsers)

channelIdByChannelName :: TeamId -> Text -> ChatState -> Maybe ChannelId
channelIdByChannelName tId name st =
    let matches (_, cc) = cc^.ccInfo.cdName == (trimChannelSigil name) &&
                          cc^.ccInfo.cdTeamId == (Just tId)
    in listToMaybe $ fst <$> filteredChannels matches (st^.csChannels)

channelIdByUsername :: Text -> ChatState -> Maybe ChannelId
channelIdByUsername name st = do
    uId <- userIdForUsername name st
    getDmChannelFor uId (st^.csChannels)

useNickname :: ChatState -> Bool
useNickname st =
    useNickname' (st^.csClientConfig) (st^.csResources.crUserPreferences)

trimChannelSigil :: Text -> Text
trimChannelSigil n
    | normalChannelSigil `T.isPrefixOf` n = T.tail n
    | otherwise = n

addNewUser :: UserInfo -> MH ()
addNewUser u = do
    csUsers %= addUser u
    -- Invalidate the cache because channel message rendering may need
    -- to get updated if this user authored posts in any channels.
    mh invalidateCache

data SidebarUpdate =
    SidebarUpdateImmediate
    | SidebarUpdateDeferred
    deriving (Eq, Show)


resetAutocomplete :: Traversal' ChatState (EditState n) -> MH ()
resetAutocomplete which = do
    which.esAutocomplete .= Nothing
    which.esAutocompletePending .= Nothing


-- * Slash Commands

-- | The 'CmdArgs' type represents the arguments to a slash-command; the
-- type parameter represents the argument structure.
data CmdArgs :: K.Type -> K.Type where
    NoArg    :: CmdArgs ()
    LineArg  :: Text -> CmdArgs Text
    UserArg  :: CmdArgs rest -> CmdArgs (Text, rest)
    ChannelArg :: CmdArgs rest -> CmdArgs (Text, rest)
    TokenArg :: Text -> CmdArgs rest -> CmdArgs (Text, rest)

-- | A 'CmdExec' value represents the implementation of a command when
-- provided with its arguments
type CmdExec a = a -> MH ()

-- | A 'Cmd' packages up a 'CmdArgs' specifier and the 'CmdExec'
-- implementation with a name and a description.
data Cmd =
    forall a. Cmd { cmdName    :: Text
                  , cmdDescr   :: Text
                  , cmdArgSpec :: CmdArgs a
                  , cmdAction  :: CmdExec a
                  }

-- | Helper function to extract the name out of a 'Cmd' value
commandName :: Cmd -> Text
commandName (Cmd name _ _ _ ) = name

-- *  Channel Updates and Notifications

userList :: ChatState -> [UserInfo]
userList st = filter showUser $ allUsers (st^.csUsers)
    where showUser u = not (isSelf u) && (u^.uiInTeam)
          isSelf u = (myUserId st) == (u^.uiId)

allUserIds :: ChatState -> [UserId]
allUserIds st = getAllUserIds $ st^.csUsers

-- BEWARE: you probably don't want this, but instead
-- State.Users.withFetchedUser, since this only looks up users in the
-- collection we have already loaded rather than all valid users on the
-- server.
userById :: UserId -> ChatState -> Maybe UserInfo
userById uId st = findUserById uId (st^.csUsers)

myUserId :: ChatState -> UserId
myUserId st = myUser st ^. userIdL

myUser :: ChatState -> User
myUser st = st^.csMe

myUsername :: ChatState -> Text
myUsername st = userUsername $ st^.csMe

-- BEWARE: you probably don't want this, but instead
-- State.Users.withFetchedUser, since this only looks up users in the
-- collection we have already loaded rather than all valid users on the
-- server.
userByUsername :: Text -> ChatState -> Maybe UserInfo
userByUsername name st = do
    snd <$> (findUserByUsername name $ st^.csUsers)

-- BEWARE: you probably don't want this, but instead
-- State.Users.withFetchedUser, since this only looks up users in the
-- collection we have already loaded rather than all valid users on the
-- server.
userByNickname :: Text -> ChatState -> Maybe UserInfo
userByNickname name st =
    snd <$> (findUserByNickname name $ st^.csUsers)

getUsers :: MH Users
getUsers = use csUsers

-- * HighlightSet

type UserSet = Set Text
type ChannelSet = Set Text

-- | The set of usernames, channel names, and language names used for
-- highlighting when rendering messages.
data HighlightSet =
    HighlightSet { hUserSet    :: Set Text
                 , hChannelSet :: Set Text
                 , hSyntaxMap  :: SyntaxMap
                 }

emptyHSet :: HighlightSet
emptyHSet = HighlightSet Set.empty Set.empty mempty

getHighlightSet :: ChatState -> TeamId -> HighlightSet
getHighlightSet st tId =
    HighlightSet { hUserSet = addSpecialUserMentions $ getUsernameSet $ st^.csUsers
                 , hChannelSet = getChannelNameSet tId $ st^.csChannels
                 , hSyntaxMap = st^.csResources.crSyntaxMap
                 }

attrNameToConfig :: Brick.AttrName -> Text
attrNameToConfig = T.pack . intercalate "." . Brick.attrNameComponents

-- From: https://docs.mattermost.com/help/messaging/mentioning-teammates.html
specialUserMentions :: [T.Text]
specialUserMentions = ["all", "channel", "here"]

addSpecialUserMentions :: Set Text -> Set Text
addSpecialUserMentions s = foldr Set.insert s specialUserMentions

getNewMessageCutoff :: ChannelId -> ChatState -> Maybe NewMessageIndicator
getNewMessageCutoff cId st = do
    cc <- st^?csChannel(cId)
    return $ cc^.ccInfo.cdNewMessageIndicator

getEditedMessageCutoff :: ChannelId -> ChatState -> Maybe ServerTime
getEditedMessageCutoff cId st = do
    cc <- st^?csChannel(cId)
    cc^.ccInfo.cdEditedMessageThreshold

clearChannelUnreadStatus :: ChannelId -> MH ()
clearChannelUnreadStatus cId = do
    mh $ invalidateCacheEntry (MessageInterfaceMessages $ MessageInput cId)
    csChannel(cId) %= (clearNewMessageIndicator .
                       clearEditedThreshold)

moveLeft :: (Eq a) => a -> [a] -> [a]
moveLeft v as =
    case elemIndex v as of
        Nothing -> as
        Just 0 -> as
        Just i ->
            let (h, t) = splitAt i as
            in init h <> [v, last h] <> tail t

moveRight :: (Eq a) => a -> [a] -> [a]
moveRight v as =
    case elemIndex v as of
        Nothing -> as
        Just i
            | i == length as - 1 -> as
            | otherwise ->
                let (h, t) = splitAt i as
                in h <> [head (tail t), v] <> (tail (tail t))

resultToWidget :: Result n -> Widget n
resultToWidget = Widget Fixed Fixed . return

threadInterface :: (HasCallStack) => TeamId -> Traversal' ChatState ThreadInterface
threadInterface tId = maybeThreadInterface(tId)._Just

-- An unsafe lens to get the specified team's thread interface. Assumes
-- the interface is present; if not, this crashes. Intended for places
-- where you know the interface will be present due to other state and
-- don't want to deal with Maybe.
unsafeThreadInterface :: (HasCallStack) => TeamId -> Lens' ChatState ThreadInterface
unsafeThreadInterface tId = maybeThreadInterface(tId).singular _Just

-- A safe version of unsafeThreadInterface.
maybeThreadInterface :: TeamId -> Lens' ChatState (Maybe ThreadInterface)
maybeThreadInterface tId = csTeam(tId).tsThreadInterface

threadInterfaceEmpty :: TeamId -> MH Bool
threadInterfaceEmpty tId = do
    mLen <- preuse (maybeThreadInterface(tId)._Just.miMessages.to messagesLength)
    case mLen of
        Nothing -> return True
        Just len -> return $ len == 0

withThreadInterface :: TeamId -> ChannelId -> MH () -> MH ()
withThreadInterface tId cId act = do
    mCid <- preuse (maybeThreadInterface(tId)._Just.miChannelId)
    case mCid of
        Just i | i == cId -> act
        _ -> return ()

threadInterfaceDeleteWhere :: TeamId -> ChannelId -> (Message -> Bool) -> MH ()
threadInterfaceDeleteWhere tId cId f =
    withThreadInterface tId cId $ do
        maybeThreadInterface(tId)._Just.miMessages.traversed.filtered f %=
            (& mDeleted .~ True)

modifyThreadMessages :: TeamId -> ChannelId -> (Messages -> Messages) -> MH ()
modifyThreadMessages tId cId f = do
    withThreadInterface tId cId $ do
        maybeThreadInterface(tId)._Just.miMessages %= f

modifyEachThreadMessage :: TeamId -> ChannelId -> (Message -> Message) -> MH ()
modifyEachThreadMessage tId cId f = do
    withThreadInterface tId cId $ do
        maybeThreadInterface(tId)._Just.miMessages.traversed %= f
