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
  , MessageSelectState(..)
  , ProgramOutput(..)
  , MHEvent(..)
  , InternalEvent(..)
  , Name(..)
  , ChannelSelectMatch(..)
  , StartupStateInfo(..)
  , MHError(..)
  , AttachmentData(..)
  , CPUUsagePolicy(..)
  , SemEq(..)
  , tabbedWindow
  , getCurrentTabbedWindowEntry
  , tabbedWindowNextTab
  , tabbedWindowPreviousTab
  , runTabShowHandlerFor
  , getServerBaseUrl
  , serverBaseUrl
  , URLListSource(..)
  , URLList(..)
  , ulList
  , ulSource
  , TabbedWindow(..)
  , TabbedWindowEntry(..)
  , TabbedWindowTemplate(..)
  , ConnectionInfo(..)
  , SidebarUpdate(..)
  , PendingChannelChange(..)
  , ViewMessageWindowTab(..)
  , clearChannelUnreadStatus
  , ChannelListEntry(..)
  , ChannelListEntryType(..)
  , ChannelListSorting(..)
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
  , newChannelTopicDialog
  , ChannelTopicDialogState(..)
  , channelTopicDialogEditor
  , channelTopicDialogFocus

  , resultToWidget

  , newSaveAttachmentDialog
  , SaveAttachmentDialogState(..)
  , attachmentPathEditor
  , attachmentPathDialogFocus

  , Config(..)
  , configUserL
  , configHostL
  , configTeamL
  , configPortL
  , configUrlPathL
  , configPassL
  , configTokenL
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
  , configMouseModeL

  , NotificationVersion(..)
  , HelpScreen(..)
  , PasswordSource(..)
  , TokenSource(..)
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

  , sortTeams
  , matchesTeam
  , mkTeamZipper
  , mkTeamZipperFromIds
  , teamZipperIds
  , mkChannelZipperList
  , ChannelListGroup(..)
  , ChannelListGroupLabel(..)
  , nonDMChannelListGroupUnread
  , channelListGroupNames

  , newThreadInterface
  , ThreadInterface(..)
  , threadMessages
  , threadEditor
  , threadMessageSelect
  , threadRootPostId
  , threadParentChannelId

  , threadInterface
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
  , tsMode
  , tsModeStack
  , tsPendingChannelChange
  , tsRecentChannel
  , tsReturnChannel
  , tsGlobalEditState
  , tsTeam
  , tsChannelSelectState
  , tsUrlList
  , tsViewedMessage
  , tsPostListOverlay
  , tsUserListOverlay
  , tsChannelListOverlay
  , tsNotifyPrefs
  , tsChannelTopicDialog
  , tsReactionEmojiListOverlay
  , tsThemeListOverlay
  , tsSaveAttachmentDialog
  , tsChannelListSorting
  , tsThreadInterface

  , channelEditor
  , channelMessageSelect

  , ChatState
  , newState
  , newTeamState

  , withCurrentChannel
  , withCurrentChannel'
  , withCurrentTeam

  , csTeamZipper
  , csTeams
  , csTeam
  , csChannelListOrientation
  , csResources
  , csLastMouseDownEvent
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

  , EditState
  , emptyEditStateForTeam
  , cedAttachmentList
  , cedFileBrowser
  , unsafeCedFileBrowser
  , cedMisspellings
  , cedEditMode
  , cedEphemeral
  , cedEditor
  , cedAutocomplete
  , cedAutocompletePending
  , cedResetEditMode
  , cedJustCompleted
  , cedShowReplyPrompt

  , GlobalEditState
  , emptyGlobalEditState
  , gedYankBuffer
  , gedSpellChecker

  , AutocompleteState(..)
  , acPreviousSearchString
  , acCompletionList
  , acCachedResponses
  , acType

  , AutocompletionType(..)

  , CompletionSource(..)
  , AutocompleteAlternative(..)
  , autocompleteAlternativeReplacement
  , SpecialMention(..)
  , specialMentionName
  , isSpecialMention

  , PostListOverlayState
  , postListSelected
  , postListPosts

  , UserSearchScope(..)
  , ChannelSearchScope(..)

  , ListOverlayState
  , listOverlaySearchResults
  , listOverlaySearchInput
  , listOverlaySearchScope
  , listOverlaySearching
  , listOverlayEnterHandler
  , listOverlayNewList
  , listOverlayFetchResults
  , listOverlayRecordCount

  , getUsers

  , ChatResources(..)
  , crUserPreferences
  , crEventQueue
  , crTheme
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
  , mhHandleEventLensed
  , mhHandleEventLensed'
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
  , resetSpellCheckTimer
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

  , module Matterhorn.Types.Channels
  , module Matterhorn.Types.Messages
  , module Matterhorn.Types.Posts
  , module Matterhorn.Types.Users
  )
where

import           Prelude ()
import           Matterhorn.Prelude

import qualified Graphics.Vty as Vty
import qualified Brick
import           Brick ( EventM, Next, Widget(..), Size(..), Result )
import           Brick.Focus ( FocusRing, focusRing )
import           Brick.Themes ( Theme )
import           Brick.Main ( invalidateCache, invalidateCacheEntry )
import           Brick.AttrMap ( AttrMap )
import qualified Brick.BChan as BCH
import           Brick.Forms (Form)
import           Brick.Widgets.Edit ( Editor, editor, applyEdit )
import           Brick.Widgets.List ( List, list )
import qualified Brick.Widgets.FileBrowser as FB
import           Control.Concurrent ( ThreadId )
import           Control.Concurrent.Async ( Async )
import qualified Control.Concurrent.STM as STM
import           Control.Exception ( SomeException )
import qualified Control.Monad.Fail as MHF
import qualified Control.Monad.State as St
import qualified Control.Monad.Reader as R
import qualified Data.Set as Set
import qualified Data.ByteString as BS
import qualified Data.Foldable as F
import           Data.Function ( on )
import qualified Data.Kind as K
import           Data.Maybe ( fromJust )
import           Data.Ord ( comparing )
import qualified Data.HashMap.Strict as HM
import           Data.List ( sortBy, nub, elemIndex, partition )
import qualified Data.Sequence as Seq
import qualified Data.Text as T
import qualified Data.Text.Zipper as Z2
import           Data.Time.Clock ( getCurrentTime, addUTCTime )
import           Data.UUID ( UUID )
import qualified Data.Vector as Vec
import           Lens.Micro.Platform ( at, makeLenses, lens, (^?!), (.=)
                                     , (%=), (%~), (.~), _Just, Traversal', to
                                     , SimpleGetter, filtered, traversed, singular
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
import           Matterhorn.Types.Channels
import           Matterhorn.Types.DirectionalSeq ( emptyDirSeq )
import           Matterhorn.Types.KeyEvents
import           Matterhorn.Types.Messages
import           Matterhorn.Types.Posts
import           Matterhorn.Types.RichText ( TeamBaseURL(..), TeamURLName(..) )
import           Matterhorn.Types.Users
import qualified Matterhorn.Zipper as Z


-- * Configuration

-- | A notification version for the external notifier
data NotificationVersion =
    NotifyV1
    | NotifyV2
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

-- | The type of channel list group headings. Integer arguments indicate
-- total number of channels in the group that have unread activity.
data ChannelListGroup =
    ChannelListGroup { channelListGroupLabel :: ChannelListGroupLabel
                     , channelListGroupUnread :: Int
                     , channelListGroupCollapsed :: Bool
                     , channelListGroupEntries :: Int
                     }
                     deriving (Eq, Show)

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

nonDMChannelListGroupUnread :: ChannelListGroup -> Int
nonDMChannelListGroupUnread g =
    case channelListGroupLabel g of
        ChannelGroupDirectMessages -> 0
        _ -> channelListGroupUnread g

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

data ChannelListSorting =
    ChannelListSortDefault
    | ChannelListSortUnreadFirst
    deriving (Eq, Show, Ord)

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
           , configChannelListWidth :: Int
           -- ^ The width, in columns, of the channel list sidebar.
           , configLogMaxBufferSize :: Int
           -- ^ The maximum size, in log entries, of the internal log
           -- message buffer.
           , configShowOlderEdits :: Bool
           -- ^ Whether to highlight the edit indicator on edits made
           -- prior to the beginning of the current session.
           , configChannelListSorting :: ChannelListSorting
           -- ^ How to sort channels in each channel list group
           , configShowTypingIndicator :: Bool
           -- ^ Whether to show the typing indicator when other users
           -- are typing
           , configSendTypingNotifications :: Bool
           -- Whether to send typing notifications to other users.
           , configAbsPath :: Maybe FilePath
           -- ^ A book-keeping field for the absolute path to the
           -- configuration. (Not a user setting.)
           , configUserKeys :: KeyConfig
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
           , configMouseMode :: Bool
           -- ^ Whether to enable mouse support in matterhorn
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

hasUnread' :: ClientChannel -> Bool
hasUnread' chan = fromMaybe False $ do
    let info = _ccInfo chan
    lastViewTime <- _cdViewed info
    return $ _cdMentionCount info > 0 ||
             (not (isMuted chan) &&
              (((_cdUpdated info) > lastViewTime) ||
               (isJust $ _cdEditedMessageThreshold info)))

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
    in [ let unread = length $ filter channelListEntryUnread favEntries
             coll = isHidden ChannelGroupFavoriteChannels
         in ( ChannelListGroup ChannelGroupFavoriteChannels unread coll (length favEntries)
            , if coll then mempty else sortChannelListEntries sorting favEntries
            )
       , let unread = length $ filter channelListEntryUnread normEntries
             coll = isHidden ChannelGroupPublicChannels
         in ( ChannelListGroup ChannelGroupPublicChannels unread coll (length normEntries)
            , if coll then mempty else sortChannelListEntries sorting normEntries
            )
       , let unread = length $ filter channelListEntryUnread privEntries
             coll = isHidden ChannelGroupPrivateChannels
         in ( ChannelListGroup ChannelGroupPrivateChannels unread coll (length privEntries)
            , if coll then mempty else sortChannelListEntries sorting privEntries
            )
       , let unread = length $ filter channelListEntryUnread dmEntries
             coll = isHidden ChannelGroupDirectMessages
         in ( ChannelListGroup ChannelGroupDirectMessages unread coll (length dmEntries)
            , if coll then mempty else sortDMChannelListEntries dmEntries
            )
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
                                             , channelListEntryUnread = hasUnread' ch
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
                                            , channelListEntryUnread = hasUnread' ch
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
                                                  , channelListEntryUnread = hasUnread' c
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
       else (if hasUnread' c || maybe False (>= localCutoff) (c^.ccInfo.cdSidebarShowOverride)
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
       else (if hasUnread' c || maybe False (>= localCutoff) (c^.ccInfo.cdSidebarShowOverride)
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

-- * Internal Names and References

-- | This 'Name' type is the type used in 'brick' to identify various
-- parts of the interface.
data Name =
    ChannelMessages ChannelId
    | MessageInput TeamId
    | ChannelList TeamId
    | HelpViewport
    | HelpText
    | PostList
    | ScriptHelpText
    | ThemeHelpText
    | SyntaxHighlightHelpText
    | KeybindingHelpText
    | ChannelSelectString TeamId
    | ChannelSelectEntry ChannelSelectMatch
    | CompletionAlternatives TeamId
    | CompletionList TeamId
    | JoinChannelList TeamId
    | UrlList TeamId
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
    | AttachmentList TeamId
    | AttachmentFileBrowser TeamId
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
    | ThreadMessageInput TeamId
    | ThreadWindowEditorPreview TeamId
    | ThreadEditorAttachmentList TeamId
    | ThreadWindowMessages TeamId
    | ChannelTopic ChannelId
    | TeamList
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
    | AttachmentPathEditor TeamId
    | AttachmentPathSaveButton TeamId
    | AttachmentPathCancelButton TeamId
    | RenderedMessage MessageId
    | ReactionEmojiListOverlayEntry (Bool, T.Text)
    | SelectedChannelListEntry TeamId
    | VScrollBar Brick.ClickableScrollbarElement Name
    deriving (Eq, Show, Ord)

-- | Types that provide a "semantically equal" operation. Two values may
-- be semantically equal even if they are not equal according to Eq if,
-- for example, they are equal on the basis of some fields that are more
-- pertinent than others.
class (Show a, Eq a, Ord a) => SemEq a where
    semeq :: a -> a -> Bool

instance SemEq Name where
    semeq (ClickableURLInMessage r1 mId1 _ t1) (ClickableURLInMessage r2 mId2 _ t2) = mId1 == mId2 && t1 == t2 && r1 == r2
    semeq (ClickableUsernameInMessage r1 mId1 _ n) (ClickableUsernameInMessage r2 mId2 _ n2) = mId1 == mId2 && n == n2 && r1 == r2
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
                  , _crStatusUpdateChan    :: STM.TChan [UserId]
                  , _crConfiguration       :: Config
                  , _crFlaggedPosts        :: Set PostId
                  , _crUserPreferences     :: UserPreferences
                  , _crSyntaxMap           :: SyntaxMap
                  , _crLogManager          :: LogManager
                  , _crEmoji               :: EmojiCollection
                  }

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

data AutocompleteState =
    AutocompleteState { _acPreviousSearchString :: Text
                      -- ^ The search string used for the
                      -- currently-displayed autocomplete results, for
                      -- use in deciding whether to issue another server
                      -- query
                      , _acCompletionList :: List Name AutocompleteAlternative
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
data EditState =
    EditState { _cedEditor :: Editor Text Name
              , _cedEditMode :: EditMode
              , _cedEphemeral :: EphemeralEditState
              , _cedMisspellings :: Set Text
              , _cedAutocomplete :: Maybe AutocompleteState
              -- ^ The autocomplete state. The autocompletion UI is
              -- showing only when this state is present.
              , _cedResetEditMode :: EditMode
              -- ^ The editing mode to reset to after input is handled.
              , _cedAutocompletePending :: Maybe Text
              -- ^ The search string associated with the latest
              -- in-flight autocompletion request. This is used to
              -- determine whether any (potentially late-arriving) API
              -- responses are for stale queries since the user can type
              -- more quickly than the server can get us the results,
              -- and we wouldn't want to show results associated with
              -- old editor states.
              , _cedAttachmentList :: List Name AttachmentData
              -- ^ The list of attachments to be uploaded with the post
              -- being edited.
              , _cedFileBrowser :: Maybe (FB.FileBrowser Name)
              -- ^ The browser for selecting attachment files. This is
              -- a Maybe because the instantiation of the FileBrowser
              -- causes it to read and ingest the target directory, so
              -- this action is deferred until the browser is needed.
              , _cedJustCompleted :: Bool
              -- A flag that indicates whether the most recent editing
              -- event was a tab-completion. This is used by the smart
              -- trailing space handling.
              , _cedShowReplyPrompt :: Bool
              -- ^ Whether to show the reply prompt when replying
              }

-- | The 'GlobalEditState' value contains state not specific to any
-- single editor.
data GlobalEditState =
    GlobalEditState { _gedYankBuffer :: Text
                    , _gedSpellChecker :: Maybe (Aspell, IO ())
                    }

-- | An attachment.
data AttachmentData =
    AttachmentData { attachmentDataFileInfo :: FB.FileInfo
                   , attachmentDataBytes :: BS.ByteString
                   }
                   deriving (Eq, Show)

-- | We can initialize a new 'EditState' value with just an edit
-- history, which we save locally.
emptyEditStateForTeam :: TeamId -> EditState
emptyEditStateForTeam tId =
    EditState { _cedEditor               = editor (MessageInput tId) Nothing ""
              , _cedEphemeral            = defaultEphemeralEditState
              , _cedEditMode             = NewPost
              , _cedMisspellings         = mempty
              , _cedAutocomplete         = Nothing
              , _cedResetEditMode        = NewPost
              , _cedAutocompletePending  = Nothing
              , _cedAttachmentList       = list (AttachmentList tId) mempty 1
              , _cedFileBrowser          = Nothing
              , _cedJustCompleted        = False
              , _cedShowReplyPrompt      = True
              }

emptyEditStateForThread :: TeamId -> EditMode -> EditState
emptyEditStateForThread tId initialEditMode =
    EditState { _cedEditor               = editor (ThreadMessageInput tId) Nothing ""
              , _cedEphemeral            = defaultEphemeralEditState
              , _cedEditMode             = initialEditMode
              , _cedMisspellings         = mempty
              , _cedAutocomplete         = Nothing
              , _cedResetEditMode        = initialEditMode
              , _cedAutocompletePending  = Nothing
              , _cedAttachmentList       = list (ThreadEditorAttachmentList tId) mempty 1
              , _cedFileBrowser          = Nothing
              , _cedJustCompleted        = False
              , _cedShowReplyPrompt      = False
              }

emptyGlobalEditState :: GlobalEditState
emptyGlobalEditState =
    GlobalEditState { _gedYankBuffer   = ""
                    , _gedSpellChecker = Nothing
                    }

-- | A 'RequestChan' is a queue of operations we have to perform in the
-- background to avoid blocking on the main loop
type RequestChan = STM.TChan (IO (Maybe (MH ())))

-- | The 'HelpScreen' type represents the set of possible 'Help'
-- dialogues we have to choose from.
data HelpScreen =
    MainHelp
    | ScriptHelp
    | ThemeHelp
    | SyntaxHighlightHelp
    | KeybindingHelp
    deriving (Eq, Show)

-- | Help topics
data HelpTopic =
    HelpTopic { helpTopicName         :: Text
              , helpTopicDescription  :: Text
              , helpTopicScreen       :: HelpScreen
              , helpTopicViewportName :: Name
              }
              deriving (Eq, Show)

-- | Mode type for the current contents of the post list overlay
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
    | UrlSelect
    | LeaveChannelConfirm
    | DeleteChannelConfirm
    | ChannelMessageSelect ChannelId
    | ThreadWindow
    | ThreadWindowMessageSelect
    | MessageSelectDeleteConfirm
    | PostListOverlay PostListContents
    | UserListOverlay
    | ReactionEmojiListOverlay
    | ChannelListOverlay
    | ThemeListOverlay
    | ViewMessage
    | ManageAttachments
    | ManageAttachmentsBrowseFiles
    | EditNotifyPrefs
    | ChannelTopicWindow
    | SaveAttachmentWindow LinkChoice
    deriving (Eq, Show)

-- | We're either connected or we're not.
data ConnectionStatus = Connected | Disconnected deriving (Eq)

-- | An entry in a tabbed window corresponding to a tab and its content.
-- Parameterized over an abstract handle type ('a') for the tabs so we
-- can give each a unique handle.
data TabbedWindowEntry a =
    TabbedWindowEntry { tweValue :: a
                      -- ^ The handle for this tab.
                      , tweRender :: a -> ChatState -> Widget Name
                      -- ^ The rendering function to use when this tab
                      -- is selected.
                      , tweHandleEvent :: a -> Vty.Event -> MH ()
                      -- ^ The event-handling function to use when this
                      -- tab is selected.
                      , tweTitle :: a -> Bool -> T.Text
                      -- ^ Title function for this tab, with a boolean
                      -- indicating whether this is the current tab.
                      , tweShowHandler :: a -> MH ()
                      -- ^ A handler to be invoked when this tab is
                      -- shown.
                      }

-- | The definition of a tabbed window. Note that this does not track
-- the *state* of the window; it merely provides a collection of tab
-- window entries (see above). To track the state of a tabbed window,
-- use a TabbedWindow.
--
-- Parameterized over an abstract handle type ('a') for the tabs so we
-- can give each a unique handle.
data TabbedWindowTemplate a =
    TabbedWindowTemplate { twtEntries :: [TabbedWindowEntry a]
                         -- ^ The entries in tabbed windows with this
                         -- structure.
                         , twtTitle :: a -> Widget Name
                         -- ^ The title-rendering function for this kind
                         -- of tabbed window.
                         }

-- | An instantiated tab window. This is based on a template and tracks
-- the state of the tabbed window (current tab).
--
-- Parameterized over an abstract handle type ('a') for the tabs so we
-- can give each a unique handle.
data TabbedWindow a =
    TabbedWindow { twValue :: a
                 -- ^ The handle of the currently-selected tab.
                 , twTemplate :: TabbedWindowTemplate a
                 -- ^ The template to use as a basis for rendering the
                 -- window and handling user input.
                 , twWindowWidth :: Int
                 , twWindowHeight :: Int
                 -- ^ Window dimensions
                 }

-- | A window in which a specific thread is viewed, where the user can
-- send messages implicitly to that thread.
data ThreadInterface =
    ThreadInterface { _threadMessages :: Messages
                    -- ^ The messages in the thread.
                    , _threadEditor :: EditState
                    -- ^ The editor and associated state for composing
                    -- messages in this thread.
                    , _threadMessageSelect :: MessageSelectState
                    -- ^ Message selection state for the thread.
                    , _threadRootPostId :: PostId
                    -- ^ The root post ID for this thread.
                    , _threadParentChannelId :: ChannelId
                    -- ^ The channel that this thread belongs to.
                    }

newThreadInterface :: TeamId -> ChannelId -> Message -> Post -> Messages -> ThreadInterface
newThreadInterface tId cId rootMsg rootPost msgs =
    ThreadInterface { _threadMessages = msgs
                    , _threadRootPostId = postId rootPost
                    , _threadParentChannelId = cId
                    , _threadMessageSelect = MessageSelectState Nothing
                    , _threadEditor = emptyEditStateForThread tId (Replying rootMsg rootPost)
                    }

-- | Construct a new tabbed window from a template. This will raise an
-- exception if the initially-selected tab does not exist in the window
-- template, or if the window template has any duplicated tab handles.
--
-- Note that the caller is responsible for determining whether to call
-- the initially-selected tab's on-show handler.
tabbedWindow :: (Show a, Eq a)
             => a
             -- ^ The handle corresponding to the tab that should be
             -- selected initially.
             -> TabbedWindowTemplate a
             -- ^ The template for the window to construct.
             -> (Int, Int)
             -- ^ The window dimensions (width, height).
             -> TabbedWindow a
tabbedWindow initialVal t (width, height) =
    let handles = tweValue <$> twtEntries t
    in if | null handles ->
              error "BUG: tabbed window template must provide at least one entry"
          | length handles /= length (nub handles) ->
              error "BUG: tabbed window should have one entry per handle"
          | not (initialVal `elem` handles) ->
              error $ "BUG: tabbed window handle " <>
                      show initialVal <> " not present in template"
          | otherwise ->
              TabbedWindow { twTemplate = t
                           , twValue = initialVal
                           , twWindowWidth = width
                           , twWindowHeight = height
                           }

-- | Get the currently-selected tab entry for a tabbed window. Raise
-- an exception if the window's selected tab handle is not found in its
-- template (which is a bug in the tabbed window infrastructure).
getCurrentTabbedWindowEntry :: (Show a, Eq a)
                            => TabbedWindow a
                            -> TabbedWindowEntry a
getCurrentTabbedWindowEntry w =
    lookupTabbedWindowEntry (twValue w) w

-- | Run the on-show handler for the window tab entry with the specified
-- handle.
runTabShowHandlerFor :: (Eq a, Show a) => a -> TabbedWindow a -> MH ()
runTabShowHandlerFor handle w = do
    let entry = lookupTabbedWindowEntry handle w
    tweShowHandler entry handle

-- | Look up a tabbed window entry by handle. Raises an exception if no
-- such entry exists.
lookupTabbedWindowEntry :: (Eq a, Show a)
                        => a
                        -> TabbedWindow a
                        -> TabbedWindowEntry a
lookupTabbedWindowEntry handle w =
    let matchesVal e = tweValue e == handle
    in case filter matchesVal (twtEntries $ twTemplate w) of
        [e] -> e
        _ -> error $ "BUG: tabbed window entry for " <> show (twValue w) <>
                     " should have matched a single entry"

-- | Switch a tabbed window's selected tab to its next tab, cycling back
-- to the first tab if the last tab is the selected tab. This also
-- invokes the on-show handler for the newly-selected tab.
--
-- Note that this does nothing if the window has only one tab.
tabbedWindowNextTab :: (Show a, Eq a)
                    => TabbedWindow a
                    -> MH (TabbedWindow a)
tabbedWindowNextTab w | length (twtEntries $ twTemplate w) == 1 = return w
tabbedWindowNextTab w = do
    let curIdx = case elemIndex (tweValue curEntry) allHandles of
            Nothing ->
                error $ "BUG: tabbedWindowNextTab: could not find " <>
                        "current handle in handle list"
            Just i -> i
        nextIdx = if curIdx == length allHandles - 1
                  then 0
                  else curIdx + 1
        newHandle = allHandles !! nextIdx
        allHandles = tweValue <$> twtEntries (twTemplate w)
        curEntry = getCurrentTabbedWindowEntry w
        newWin = w { twValue = newHandle }

    runTabShowHandlerFor newHandle newWin
    return newWin

-- | Switch a tabbed window's selected tab to its previous tab, cycling
-- to the last tab if the first tab is the selected tab. This also
-- invokes the on-show handler for the newly-selected tab.
--
-- Note that this does nothing if the window has only one tab.
tabbedWindowPreviousTab :: (Show a, Eq a)
                        => TabbedWindow a
                        -> MH (TabbedWindow a)
tabbedWindowPreviousTab w | length (twtEntries $ twTemplate w) == 1 = return w
tabbedWindowPreviousTab w = do
    let curIdx = case elemIndex (tweValue curEntry) allHandles of
            Nothing ->
                error $ "BUG: tabbedWindowPreviousTab: could not find " <>
                        "current handle in handle list"
            Just i -> i
        nextIdx = if curIdx == 0
                  then length allHandles - 1
                  else curIdx - 1
        newHandle = allHandles !! nextIdx
        allHandles = tweValue <$> twtEntries (twTemplate w)
        curEntry = getCurrentTabbedWindowEntry w
        newWin = w { twValue = newHandle }

    runTabShowHandlerFor newHandle newWin
    return newWin

data ChannelListOrientation =
    ChannelListLeft
    -- ^ Show the channel list to the left of the message area.
    | ChannelListRight
    -- ^ Show the channel list to the right of the message area.
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
              }

data URLList =
    URLList { _ulList :: List Name (Int, LinkChoice)
            , _ulSource :: Maybe URLListSource
            }

data URLListSource =
    FromChannel TeamId ChannelId
    | FromThreadIn ChannelId
    deriving (Show, Eq)

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
              , _tsPendingChannelChange :: Maybe PendingChannelChange
              -- ^ A pending channel change that we need to apply once
              -- the channel in question is available. We set this up
              -- when we need to change to a channel in the sidebar, but
              -- it isn't even there yet because we haven't loaded its
              -- metadata.
              , _tsRecentChannel :: Maybe ChannelId
              -- ^ The most recently-selected channel, if any.
              , _tsReturnChannel :: Maybe ChannelId
              -- ^ The channel to return to after visiting one or more
              -- unread channels.
              , _tsEditState :: EditState
              -- ^ The state of the input box used for composing and
              -- editing messages and commands.
              , _tsGlobalEditState :: GlobalEditState
              -- ^ Bits of global state common to all editors.
              , _tsMessageSelect :: MessageSelectState
              -- ^ The state of message selection mode.
              , _tsTeam :: Team
              -- ^ The team data.
              , _tsChannelSelectState :: ChannelSelectState
              -- ^ The state of the user's input and selection for
              -- channel selection mode.
              , _tsUrlList :: URLList
              -- ^ The URL list used to show URLs drawn from messages in
              -- a channel.
              , _tsViewedMessage :: Maybe (Message, TabbedWindow ViewMessageWindowTab)
              -- ^ Set when the ViewMessage mode is active. The message
              -- being viewed. Note that this stores a message, not
              -- a message ID. That's because not all messages have
              -- message IDs (e.g. client messages) and we still
              -- want to support viewing of those messages. It's the
              -- responsibility of code that uses this message to always
              -- consult the chat state for the latest *version* of any
              -- message with an ID here, to be sure that the latest
              -- version is used (e.g. if it gets edited, etc.).
              , _tsPostListOverlay :: PostListOverlayState
              -- ^ The state of the post list overlay.
              , _tsUserListOverlay :: ListOverlayState UserInfo UserSearchScope
              -- ^ The state of the user list overlay.
              , _tsChannelListOverlay :: ListOverlayState Channel ChannelSearchScope
              -- ^ The state of the user list overlay.
              , _tsNotifyPrefs :: Maybe (Form ChannelNotifyProps MHEvent Name)
              -- ^ A form for editing the notification preferences for
              -- the current channel. This is set when entering
              -- EditNotifyPrefs mode and updated when the user
              -- changes the form state.
              , _tsChannelTopicDialog :: ChannelTopicDialogState
              -- ^ The state for the interactive channel topic editor
              -- window.
              , _tsMode :: Mode
              -- ^ The current application mode when viewing this team.
              -- This is used to dispatch to different rendering and
              -- event handling routines.
              , _tsModeStack :: [Mode]
              -- ^ The rest of the mode stack for this team. The current
              -- mode is always in tsMode; "popping" off the stack
              -- means replacing tsMode with the head of this list,
              -- unless this list is empty, in which case the pop does
              -- nothing. Pushing means pushing tsMode onto this list
              -- and replacing tsMode.
              , _tsReactionEmojiListOverlay :: ListOverlayState (Bool, T.Text) ()
              -- ^ The state of the reaction emoji list overlay.
              , _tsThemeListOverlay :: ListOverlayState InternalTheme ()
              -- ^ The state of the theme list overlay.
              , _tsSaveAttachmentDialog :: SaveAttachmentDialogState
              -- ^ The state for the interactive attachment-saving
              -- editor window.
              , _tsChannelListSorting :: ChannelListSorting
              -- ^ How to sort channels in this team's channel list
              -- groups
              , _tsThreadInterface :: Maybe ThreadInterface
              -- ^ The thread interface for this team for participating
              -- in a single thread
              }

-- | Handles for the View Message window's tabs.
data ViewMessageWindowTab =
    VMTabMessage
    -- ^ The message tab.
    | VMTabReactions
    -- ^ The reactions tab.
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

-- | The state of the attachment path window.
data SaveAttachmentDialogState =
    SaveAttachmentDialogState { _attachmentPathEditor :: Editor T.Text Name
                              -- ^ The attachment path editor state.
                              , _attachmentPathDialogFocus :: FocusRing Name
                              -- ^ The window focus state (editor/buttons)
                              }

sortTeams :: [Team] -> [Team]
sortTeams = sortBy (compare `on` (T.strip . sanitizeUserText . teamName))

matchesTeam :: T.Text -> Team -> Bool
matchesTeam tName t =
    let normalizeUserText = normalize . sanitizeUserText
        normalize = T.strip . T.toLower
        urlName = normalizeUserText $ teamName t
        displayName = normalizeUserText $ teamDisplayName t
    in normalize tName `elem` [displayName, urlName]

mkTeamZipper :: HM.HashMap TeamId TeamState -> Z.Zipper () TeamId
mkTeamZipper m =
    let sortedTeams = sortTeams $ _tsTeam <$> HM.elems m
    in mkTeamZipperFromIds $ teamId <$> sortedTeams

mkTeamZipperFromIds :: [TeamId] -> Z.Zipper () TeamId
mkTeamZipperFromIds tIds = Z.fromList [((), tIds)]

teamZipperIds :: Z.Zipper () TeamId -> [TeamId]
teamZipperIds = concat . fmap snd . Z.toList

newTeamState :: Config
             -> Team
             -> Z.Zipper ChannelListGroup ChannelListEntry
             -> Maybe (Aspell, IO ())
             -> TeamState
newTeamState config team chanList spellChecker =
    let tId = teamId team
    in TeamState { _tsMode                     = Main
                 , _tsModeStack                = []
                 , _tsFocus                    = chanList
                 , _tsEditState                = emptyEditStateForTeam tId
                 , _tsGlobalEditState          = emptyGlobalEditState { _gedSpellChecker = spellChecker }
                 , _tsTeam                     = team
                 , _tsUrlList                  = URLList { _ulList = list (UrlList tId) mempty 2
                                                         , _ulSource = Nothing
                                                         }
                 , _tsPostListOverlay          = PostListOverlayState emptyDirSeq Nothing
                 , _tsUserListOverlay          = nullUserListOverlayState tId
                 , _tsChannelListOverlay       = nullChannelListOverlayState tId
                 , _tsChannelSelectState       = emptyChannelSelectState tId
                 , _tsChannelTopicDialog       = newChannelTopicDialog tId ""
                 , _tsMessageSelect            = MessageSelectState Nothing
                 , _tsNotifyPrefs              = Nothing
                 , _tsPendingChannelChange     = Nothing
                 , _tsRecentChannel            = Nothing
                 , _tsReturnChannel            = Nothing
                 , _tsViewedMessage            = Nothing
                 , _tsThemeListOverlay         = nullThemeListOverlayState tId
                 , _tsReactionEmojiListOverlay = nullEmojiListOverlayState tId
                 , _tsSaveAttachmentDialog     = newSaveAttachmentDialog tId ""
                 , _tsChannelListSorting       = configChannelListSorting config
                 , _tsThreadInterface          = Nothing
                 }

-- | Make a new channel topic editor window state.
newChannelTopicDialog :: TeamId -> T.Text -> ChannelTopicDialogState
newChannelTopicDialog tId t =
    ChannelTopicDialogState { _channelTopicDialogEditor = editor (ChannelTopicEditor tId) Nothing t
                            , _channelTopicDialogFocus = focusRing [ ChannelTopicEditor tId
                                                                   , ChannelTopicSaveButton tId
                                                                   , ChannelTopicCancelButton tId
                                                                   ]
                            }

-- | Make a new attachment-saving editor window state.
newSaveAttachmentDialog :: TeamId -> T.Text -> SaveAttachmentDialogState
newSaveAttachmentDialog tId t =
    SaveAttachmentDialogState { _attachmentPathEditor = applyEdit Z2.gotoEOL $
                                                        editor (AttachmentPathEditor tId) (Just 1) t
                              , _attachmentPathDialogFocus = focusRing [ AttachmentPathEditor tId
                                                                       , AttachmentPathSaveButton tId
                                                                       , AttachmentPathCancelButton tId
                                                                       ]
                              }

nullChannelListOverlayState :: TeamId -> ListOverlayState Channel ChannelSearchScope
nullChannelListOverlayState tId =
    let newList rs = list (JoinChannelList tId) rs 2
    in ListOverlayState { _listOverlaySearchResults  = newList mempty
                        , _listOverlaySearchInput    = editor (JoinChannelListSearchInput tId) (Just 1) ""
                        , _listOverlaySearchScope    = AllChannels
                        , _listOverlaySearching      = False
                        , _listOverlayEnterHandler   = const $ return False
                        , _listOverlayNewList        = newList
                        , _listOverlayFetchResults   = const $ const $ const $ return mempty
                        , _listOverlayRecordCount    = Nothing
                        }

nullThemeListOverlayState :: TeamId -> ListOverlayState InternalTheme ()
nullThemeListOverlayState tId =
    let newList rs = list (ThemeListSearchResults tId) rs 3
    in ListOverlayState { _listOverlaySearchResults  = newList mempty
                        , _listOverlaySearchInput    = editor (ThemeListSearchInput tId) (Just 1) ""
                        , _listOverlaySearchScope    = ()
                        , _listOverlaySearching      = False
                        , _listOverlayEnterHandler   = const $ return False
                        , _listOverlayNewList        = newList
                        , _listOverlayFetchResults   = const $ const $ const $ return mempty
                        , _listOverlayRecordCount    = Nothing
                        }

nullUserListOverlayState :: TeamId -> ListOverlayState UserInfo UserSearchScope
nullUserListOverlayState tId =
    let newList rs = list (UserListSearchResults tId) rs 1
    in ListOverlayState { _listOverlaySearchResults  = newList mempty
                        , _listOverlaySearchInput    = editor (UserListSearchInput tId) (Just 1) ""
                        , _listOverlaySearchScope    = AllUsers Nothing
                        , _listOverlaySearching      = False
                        , _listOverlayEnterHandler   = const $ return False
                        , _listOverlayNewList        = newList
                        , _listOverlayFetchResults   = const $ const $ const $ return mempty
                        , _listOverlayRecordCount    = Nothing
                        }

nullEmojiListOverlayState :: TeamId -> ListOverlayState (Bool, T.Text) ()
nullEmojiListOverlayState tId =
    let newList rs = list (ReactionEmojiList tId) rs 1
    in ListOverlayState { _listOverlaySearchResults  = newList mempty
                        , _listOverlaySearchInput    = editor (ReactionEmojiListInput tId) (Just 1) ""
                        , _listOverlaySearchScope    = ()
                        , _listOverlaySearching      = False
                        , _listOverlayEnterHandler   = const $ return False
                        , _listOverlayNewList        = newList
                        , _listOverlayFetchResults   = const $ const $ const $ return mempty
                        , _listOverlayRecordCount    = Nothing
                        }

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

-- | The state of message selection mode.
data MessageSelectState =
    MessageSelectState { selectMessageId :: Maybe MessageId
                       }

-- | The state of the post list overlay.
data PostListOverlayState =
    PostListOverlayState { _postListPosts    :: Messages
                         , _postListSelected :: Maybe PostId
                         }

data InternalTheme =
    InternalTheme { internalThemeName :: Text
                  , internalTheme :: Theme
                  , internalThemeDesc :: Text
                  }

-- | The state of the search result list overlay. Type 'a' is the type
-- of data in the list. Type 'b' is the search scope type.
data ListOverlayState a b =
    ListOverlayState { _listOverlaySearchResults :: List Name a
                     -- ^ The list of search results currently shown in
                     -- the overlay.
                     , _listOverlaySearchInput :: Editor Text Name
                     -- ^ The editor for the overlay's search input.
                     , _listOverlaySearchScope :: b
                     -- ^ The overlay's current search scope.
                     , _listOverlaySearching :: Bool
                     -- ^ Whether a search is in progress (i.e. whether
                     -- we are currently awaiting a response from a
                     -- search query to the server).
                     , _listOverlayEnterHandler :: a -> MH Bool
                     -- ^ The handler to invoke on the selected element
                     -- when the user presses Enter.
                     , _listOverlayNewList :: Vec.Vector a -> List Name a
                     -- ^ The function to build a new brick List from a
                     -- vector of search results.
                     , _listOverlayFetchResults :: b -> Session -> Text -> IO (Vec.Vector a)
                     -- ^ The function to call to issue a search query
                     -- to the server.
                     , _listOverlayRecordCount :: Maybe Int
                     -- ^ The total number of available records, if known.
                     }

-- | The scope for searching for users in a user list overlay.
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
    MHState { mhCurrentState :: ChatState
            , mhNextAction :: ChatState -> EventM Name (Next ChatState)
            , mhUsersToFetch :: [UserFetch]
            , mhPendingStatusList :: Maybe [UserId]
            }

-- | A value of type 'MH' @a@ represents a computation that can
-- manipulate the application state and also request that the
-- application quit
newtype MH a =
    MH { fromMH :: R.ReaderT (Maybe LogContext) (St.StateT MHState (EventM Name)) a }

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

-- | Run an 'MM' computation, choosing whether to continue or halt based
-- on the resulting
runMHEvent :: ChatState -> MH () -> EventM Name (Next ChatState)
runMHEvent st (MH mote) = do
  let mhSt = MHState { mhCurrentState = st
                     , mhNextAction = Brick.continue
                     , mhUsersToFetch = []
                     , mhPendingStatusList = Nothing
                     }
  ((), st') <- St.runStateT (R.runReaderT mote Nothing) mhSt
  (mhNextAction st') (mhCurrentState st')

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
mh :: EventM Name a -> MH a
mh = MH . R.lift . St.lift

generateUUID :: MH UUID
generateUUID = liftIO generateUUID_IO

generateUUID_IO :: IO UUID
generateUUID_IO = randomIO

mhHandleEventLensed :: Lens' ChatState b -> (e -> b -> EventM Name b) -> e -> MH ()
mhHandleEventLensed ln f event = MH $ do
    s <- St.get
    let st = mhCurrentState s
    n <- R.lift $ St.lift $ f event (st ^. ln)
    St.put (s { mhCurrentState = st & ln .~ n })

mhHandleEventLensed' :: Lens' ChatState b -> (b -> EventM Name b) -> MH ()
mhHandleEventLensed' ln f = MH $ do
    s <- St.get
    let st = mhCurrentState s
    n <- R.lift $ St.lift $ f (st ^. ln)
    St.put (s { mhCurrentState = st & ln .~ n })

mhSuspendAndResume :: (ChatState -> IO ChatState) -> MH ()
mhSuspendAndResume mote = MH $ do
    s <- St.get
    St.put $ s { mhNextAction = \ _ -> Brick.suspendAndResume (mote $ mhCurrentState s) }

mhContinueWithoutRedraw :: MH ()
mhContinueWithoutRedraw = MH $ do
    s <- St.get
    St.put $ s { mhNextAction = \ _ -> Brick.continueWithoutRedraw (mhCurrentState s) }

-- | This will request that after this computation finishes the
-- application should exit
requestQuit :: MH ()
requestQuit = MH $ do
    s <- St.get
    St.put $ s { mhNextAction = Brick.halt }

instance Functor MH where
    fmap f (MH x) = MH (fmap f x)

instance Applicative MH where
    pure x = MH (pure x)
    MH f <*> MH x = MH (f <*> x)

instance MHF.MonadFail MH where
    fail = MH . MHF.fail

instance Monad MH where
    return = pure
    MH x >>= f = MH (x >>= \ x' -> fromMH (f x'))

-- We want to pretend that the state is only the ChatState, rather
-- than the ChatState and the Brick continuation
instance St.MonadState ChatState MH where
    get = mhCurrentState `fmap` MH St.get
    put st = MH $ do
        s <- St.get
        St.put $ s { mhCurrentState = st }

instance St.MonadIO MH where
    liftIO = MH . St.liftIO

-- | This represents events that we handle in the main application loop.
data MHEvent =
    WSEvent WebsocketEvent
    -- ^ For events that arise from the websocket
    | WSActionResponse WebsocketActionResponse
    -- ^ For responses to websocket actions
    | RespEvent (MH ())
    -- ^ For the result values of async IO operations
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
makeLenses ''EditState
makeLenses ''GlobalEditState
makeLenses ''AutocompleteState
makeLenses ''PostListOverlayState
makeLenses ''ListOverlayState
makeLenses ''ChannelSelectState
makeLenses ''UserPreferences
makeLenses ''ConnectionInfo
makeLenses ''ChannelTopicDialogState
makeLenses ''SaveAttachmentDialogState
makeLenses ''URLList
makeLenses ''ThreadInterface
Brick.suffixLenses ''Config

applyTeamOrderPref :: Maybe [TeamId] -> ChatState -> ChatState
applyTeamOrderPref Nothing st = st
applyTeamOrderPref (Just prefTIds) st =
    let teams = _csTeams st
        ourTids = HM.keys teams
        tIds = filter (`elem` ourTids) prefTIds
        curTId = st^.csCurrentTeamId
        unmentioned = filter (not . wasMentioned) $ HM.elems teams
        wasMentioned ts = (teamId $ _tsTeam ts) `elem` tIds
        zipperTids = tIds <> (teamId <$> sortTeams (_tsTeam <$> unmentioned))
    in st { _csTeamZipper = (Z.findRight ((== curTId) . Just) $ mkTeamZipperFromIds zipperTids)
          }

refreshTeamZipper :: MH ()
refreshTeamZipper = do
    tidOrder <- use (csResources.crUserPreferences.userPrefTeamOrder)
    St.modify (applyTeamOrderPref tidOrder)

applyTeamOrder :: [TeamId] -> MH ()
applyTeamOrder tIds = St.modify (applyTeamOrderPref $ Just tIds)

newState :: StartupStateInfo -> ChatState
newState (StartupStateInfo {..}) =
    let config = _crConfiguration startupStateResources
    in applyTeamOrderPref (_userPrefTeamOrder $ _crUserPreferences startupStateResources) $
       ChatState { _csResources                   = startupStateResources
                 , _csLastMouseDownEvent          = Nothing
                 , _csVerbatimTruncateSetting     = configTruncateVerbatimBlocks config
                 , _csTeamZipper                  = Z.findRight (== startupStateInitialTeam) $
                                                    mkTeamZipper startupStateTeams
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

unsafeCedFileBrowser :: Lens' EditState (FB.FileBrowser Name)
unsafeCedFileBrowser =
     lens (\st   -> st^.cedFileBrowser ^?! _Just)
          (\st t -> st & cedFileBrowser .~ Just t)

getSession :: MH Session
getSession = use (csResources.crSession)

getResourceSession :: ChatResources -> Session
getResourceSession = _crSession

whenMode :: TeamId -> Mode -> MH () -> MH ()
whenMode tId m act = do
    curMode <- use (csTeam(tId).tsMode)
    when (curMode == m) act

pushMode :: TeamId -> Mode -> MH ()
pushMode tId m = do
    St.modify (pushMode' tId m)
    mh invalidateCache

replaceMode :: TeamId -> Mode -> MH ()
replaceMode tId m = popMode tId >> pushMode tId m

popMode :: TeamId -> MH ()
popMode tId = do
    curStack <- use (csTeam(tId).tsModeStack)
    case curStack of
        [] -> return ()
        (m:ms) -> do
            csTeam(tId).tsMode .= m
            csTeam(tId).tsModeStack .= ms
            mh invalidateCache

pushMode' :: TeamId -> Mode -> ChatState -> ChatState
pushMode' tId m st =
    let cur = st^.csTeam(tId).tsMode
    in st & csTeam(tId).tsMode .~ m
          & csTeam(tId).tsModeStack %~ (cur:)

resetSpellCheckTimer :: GlobalEditState -> IO ()
resetSpellCheckTimer s =
    case s^.gedSpellChecker of
        Nothing -> return ()
        Just (_, reset) -> reset

-- ** Utility Lenses
csCurrentChannelId :: TeamId -> SimpleGetter ChatState (Maybe ChannelId)
csCurrentChannelId tId =
    csTeam(tId).tsFocus.to Z.focus.to (fmap channelListEntryChannelId)

withCurrentTeam :: (TeamId -> MH ()) -> MH ()
withCurrentTeam f = do
    mtId <- use csCurrentTeamId
    case mtId of
        Nothing -> return ()
        Just tId -> f tId

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

channelEditor :: TeamId -> Lens' ChatState EditState
channelEditor tId = csTeam(tId).tsEditState

channelMessageSelect :: TeamId -> Lens' ChatState MessageSelectState
channelMessageSelect tId = csTeam(tId).tsMessageSelect

csTeam :: TeamId -> Lens' ChatState TeamState
csTeam tId =
    lens (\ st -> st ^. csTeams . at tId ^?! _Just)
         (\ st t -> st & csTeams . at tId .~ Just t)

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
    csChannel(cId).ccContents.cdMessages

withChannel :: ChannelId -> (ClientChannel -> MH ()) -> MH ()
withChannel cId = withChannelOrDefault cId ()

withChannelOrDefault :: ChannelId -> a -> (ClientChannel -> MH a) -> MH a
withChannelOrDefault cId deflt mote = do
    chan <- preuse (csChannel(cId))
    case chan of
        Nothing -> return deflt
        Just c  -> mote c

-- ** 'ChatState' Helper Functions

raiseInternalEvent :: InternalEvent -> MH ()
raiseInternalEvent ev = do
    queue <- use (csResources.crEventQueue)
    writeBChan queue (IEvent ev)

writeBChan :: (MonadIO m) => BCH.BChan MHEvent -> MHEvent -> m ()
writeBChan chan e = do
    written <- liftIO $ BCH.writeBChanNonBlocking chan e
    when (not written) $
        error $ "mhSendEvent: BChan full, please report this as a bug!"

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


resetAutocomplete :: Lens' ChatState EditState -> MH ()
resetAutocomplete which = do
    which.cedAutocomplete .= Nothing
    which.cedAutocompletePending .= Nothing


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
    mh $ invalidateCacheEntry (ChannelMessages cId)
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

-- An unsafe lens to get the specified team's thread interface. Assumes
-- the interface is present; if not, this crashes. Intended for places
-- where you know the interface will be present due to other state and
-- don't want to deal with Maybe.
threadInterface :: TeamId -> Lens' ChatState ThreadInterface
threadInterface tId = maybeThreadInterface(tId).singular _Just

-- A safe version of threadInterface.
maybeThreadInterface :: TeamId -> Lens' ChatState (Maybe ThreadInterface)
maybeThreadInterface tId = csTeam(tId).tsThreadInterface

threadInterfaceEmpty :: TeamId -> MH Bool
threadInterfaceEmpty tId = do
    mLen <- preuse (maybeThreadInterface(tId)._Just.threadMessages.to messagesLength)
    case mLen of
        Nothing -> return True
        Just len -> return $ len == 0

threadInterfaceDeleteWhere :: TeamId -> (Message -> Bool) -> MH ()
threadInterfaceDeleteWhere tId f =
    maybeThreadInterface(tId)._Just.threadMessages.traversed.filtered f %=
        (& mDeleted .~ True)

modifyThreadMessages :: TeamId -> (Messages -> Messages) -> MH ()
modifyThreadMessages tId f = do
    maybeThreadInterface(tId)._Just.threadMessages %= f

modifyEachThreadMessage :: TeamId -> (Message -> Message) -> MH ()
modifyEachThreadMessage tId f = do
    maybeThreadInterface(tId)._Just.threadMessages.traversed %= f
