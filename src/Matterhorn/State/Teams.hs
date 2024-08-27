{-# LANGUAGE RankNTypes #-}
module Matterhorn.State.Teams
  ( nextTeam
  , prevTeam
  , handleJoinTeam
  , handleLeaveTeam
  , handleUpdateTeam
  , buildTeamState
  , moveCurrentTeamLeft
  , moveCurrentTeamRight
  , setTeam
  , newSaveAttachmentDialog
  , newChannelTopicDialog
  , newThreadInterface
  , makeClientChannel
  , cycleTeamMessageInterfaceFocus
  )
where

import           Prelude ()
import           Matterhorn.Prelude

import           Brick ( getName )
import qualified Brick.BChan as BCH
import           Brick.Main ( invalidateCache, hScrollToBeginning, viewportScroll, makeVisible )
import           Brick.Widgets.List ( list )
import           Brick.Widgets.Edit ( editor, applyEdit )
import           Brick.Focus ( focusRing )
import qualified Data.Sequence as Seq
import qualified Data.Text as T
import           Data.Time.Clock ( getCurrentTime )
import qualified Data.Text.Zipper as Z2
import qualified Data.HashMap.Strict as HM
import           Lens.Micro.Platform ( (%=), (.=), at )
import           Text.Aspell ( Aspell )

import           Network.Mattermost.Lenses ( userIdL, channelTypeL, channelPurposeL
                                           , channelHeaderL, channelTeamIdL, channelIdL
                                           , channelLastPostAtL
                                           )
import           Network.Mattermost.Types ( TeamId, Team, Channel, User, userId
                                          , getId, channelId, teamId, UserParam(..)
                                          , teamOrderPref, Post, ChannelId, postId
                                          , emptyChannelNotifyProps, UserId
                                          , channelName, Type(..), channelDisplayName
                                          , Channel, ChannelMember
                                          , channelMemberMsgCount
                                          , channelTotalMsgCount
                                          , UserParam(UserById)
                                          )
import qualified Network.Mattermost.Endpoints as MM

import           Matterhorn.Types
import           Matterhorn.Types.Common
import           Matterhorn.Types.DirectionalSeq ( emptyDirSeq )
import           Matterhorn.Types.NonemptyStack
import           Matterhorn.LastRunState
import           Matterhorn.State.Async
import           Matterhorn.State.ChannelList
import           Matterhorn.State.Channels
import {-# SOURCE #-} Matterhorn.State.ThreadWindow
import {-# SOURCE #-} Matterhorn.State.Messages
import           Matterhorn.State.Setup.Threads ( newSpellCheckTimer )
import qualified Matterhorn.Zipper as Z


-- | Move right in the channel list to select the next team.
nextTeam :: MH ()
nextTeam = setTeamFocusWith Z.right

-- | Move left in the channel list to select the previous team.
prevTeam :: MH ()
prevTeam = setTeamFocusWith Z.left

-- | Set the current team directly
setTeam :: TeamId -> MH ()
setTeam tId = setTeamFocusWith $ Z.findRight (== tId)

-- | Change the selected team with the specified team zipper
-- transformation. This function also takes care of book-keeping
-- necessary during team switching.
setTeamFocusWith :: (Z.Zipper () TeamId -> Z.Zipper () TeamId) -> MH ()
setTeamFocusWith f = do
    -- Before we leave this team to view another one, indicate that
    -- we've viewed the current team's currently-selected channel so
    -- that this team doesn't get left with an unread indicator once we
    -- are looking at the other team. We do this when switching channels
    -- within a team in the same way.
    updateViewed True

    csTeamZipper %= f
    withCurrentTeam postChangeTeamCommon

-- | Book-keeping common to all team selection changes.
postChangeTeamCommon :: TeamId -> MH ()
postChangeTeamCommon tId = do
    updateViewed False
    fetchVisibleIfNeeded tId
    mh $ do
        hScrollToBeginning (viewportScroll TeamList)
        makeVisible $ SelectedChannelListEntry tId

-- | Fetch the specified team and add it to the application state.
--
-- This is called in response to a server event indicating that the
-- current user was added to the team.
handleJoinTeam :: TeamId -> MH ()
handleJoinTeam tId = do
    session <- getSession
    cr <- use csResources
    me <- use csMe
    curTs <- use csTeams
    let myTIds = HM.keys curTs

    when (not $ tId `elem` myTIds) $ do
        mhLog LogGeneral $ T.pack $ "Joining team " <> show tId
        doAsyncWith Normal $ do
            t <- MM.mmGetTeam tId session
            (ts, chans) <- buildTeamState cr me t
            return $ Just $ Work "handleJoinTeam" $ do
                    addTeamState ts chans
                    updateSidebar $ Just tId
                    updateWindowTitle
                    refreshTeamZipper

-- | Remove the specified team to the application state.
--
-- This is called in response to a server event indicating that the
-- current user was removed from the team.
handleLeaveTeam :: TeamId -> MH ()
handleLeaveTeam tId =
    doAsyncWith Normal $ return $ Just $ Work "handleLeaveTeam" $ do
        mhLog LogGeneral $ T.pack $ "Leaving team " <> show tId
        removeTeam tId
        updateWindowTitle
        -- Invalidating the cache here expunges any cached message
        -- renderings from the team we are leaving.
        mh invalidateCache

-- | Fetch the specified team's metadata and update it in the
-- application state.
--
-- This is called in response to a server event indicating that the
-- specified team was updated in some way.
handleUpdateTeam :: TeamId -> MH ()
handleUpdateTeam tId = do
    session <- getSession
    mhLog LogGeneral $ T.pack $ "Updating team " <> show tId
    doAsyncWith Normal $ do
        t <- MM.mmGetTeam tId session
        return $ Just $ Work "handleUpdateTeam" $ do
            updateTeam t
            -- Invalidate the cache since we happen to know that the
            -- team name is in the cached sidebar.
            mh invalidateCache

-- | Set the team zipper ordering with the specified transformation,
-- which is expected to be either 'moveLeft' or 'moveRight'.
setTeamOrderWith :: (TeamId -> [TeamId] -> [TeamId]) -> MH ()
setTeamOrderWith transform = do
    session <- getSession
    me <- use csMe

    mtId <- use csCurrentTeamId
    z <- use csTeamZipper
    let tIds = teamZipperIds z
        newList = maybe tIds (\tId -> transform tId tIds) mtId

    doAsyncWith Normal $ do
        let pref = teamOrderPref (me^.userIdL) newList
        MM.mmSaveUsersPreferences UserMe (Seq.singleton pref) session
        return Nothing

-- | Move the selected team left in the team list.
moveCurrentTeamLeft :: MH ()
moveCurrentTeamLeft = setTeamOrderWith moveLeft

-- | Move the selected team right in the team list.
moveCurrentTeamRight :: MH ()
moveCurrentTeamRight = setTeamOrderWith moveRight

-- | Build a new 'TeamState' for the specified team.
--
-- This function starts a new spell checker thread for the team's
-- message editor, loads the last-run state for the team (to ensure that
-- the initially-selected channel is honored), and fetches the channel
-- metadata for the team.
--
-- This returns the resulting team state as well as the channels
-- associated with the team. The caller is responsible for adding the
-- channels and the team state to the application state.
buildTeamState :: ChatResources -> User -> Team -> IO (TeamState, ClientChannels)
buildTeamState cr me team = do
    let tId = teamId team
        session = getResourceSession cr
        config = cr^.crConfiguration
        eventQueue = cr^.crEventQueue

    lrsResult <- readLastRunState tId
    let mLrs = case lrsResult of
            Right lrs | isValidLastRunState cr me lrs -> Just lrs
            _ -> Nothing

        -- Create a predicate to find the last selected channel by
        -- checking the last run state.
        isLastSelectedChannel = maybe isTownSquare (\lrs c -> Just (channelId c) == lrs^.lrsSelectedChannelId) mLrs

    -- Get all channels, but filter down to just the one we want
    -- to start in. We get all, rather than requesting by name or
    -- ID, because we don't know whether the server will give us a
    -- last-viewed preference. We first try to find a channel matching
    -- with the last selected channel ID, failing which we look for the
    -- Town Square channel by name.
    userChans <- MM.mmGetChannelsForUser UserMe tId session
    let lastSelectedChans = Seq.filter isLastSelectedChannel userChans
        chans = if Seq.null lastSelectedChans
                  then Seq.filter isTownSquare userChans
                  else lastSelectedChans

    -- Since the only channel we are dealing with is by construction the
    -- last channel, we don't have to consider other cases here:
    chanPairs <- forM (toList chans) $ \c -> do
        m <- MM.mmGetChannelMember (getId c) (UserById $ userId me) session
        cChannel <- makeClientChannel eventQueue (cr^.crSpellChecker) (userId me) (Just tId) c m
        return (getId c, cChannel)

    now <- getCurrentTime
    let chanIds = mkChannelZipperList (config^.configChannelListSortingL) now config tId
                                          Nothing (cr^.crUserPreferences)
                                          mempty clientChans noUsers
        chanZip = Z.fromList chanIds
        clientChans = foldr (uncurry addChannel) noChannels chanPairs

    -- If the configuration says to open any last open thread, then
    -- schedule it to be opened.
    when (configShowLastOpenThread config) $ do
        let maybeOpenThread = do
                lrs <- mLrs
                (cId, pId) <- lrs^.lrsOpenThread
                return $ scheduleMH cr "buildTeamState" (openThreadWindow tId cId pId)
        fromMaybe (return ()) maybeOpenThread

    let ts = newTeamState config team chanZip
    return (ts, clientChans)

-- | Add a new 'TeamState' and corresponding channels to the application
-- state.
addTeamState :: TeamState -> ClientChannels -> MH ()
addTeamState ts chans = do
    let tId = teamId $ _tsTeam ts
    csTeams.at tId .= Just ts
    csChannels %= (chans <>)

-- | Update the specified team metadata in the application state (only
-- if we are already a member of that team).
updateTeam :: Team -> MH ()
updateTeam t = do
    let tId = teamId t
    ts <- use csTeams
    when (tId `elem` HM.keys ts) $ do
        csTeam(tId).tsTeam .= t

-- | Remove the specified team from the application state.
removeTeam :: TeamId -> MH ()
removeTeam tId = do
    csTeams.at tId .= Nothing
    setTeamFocusWith $ Z.filterZipper (/= tId)

cycleTeamMessageInterfaceFocus :: TeamId -> MH ()
cycleTeamMessageInterfaceFocus tId =
    csTeam(tId) %= messageInterfaceFocusNext

emptyEditStateForChannel :: Maybe Aspell -> BCH.BChan MHEvent -> Maybe TeamId -> ChannelId -> IO (EditState Name)
emptyEditStateForChannel checker eventQueue tId cId = do
    reset <- case checker of
        Nothing -> return Nothing
        Just as -> Just <$> (newSpellCheckTimer as eventQueue $ MIChannel cId)
    let editorName = MessageInput cId
        attachmentListName = AttachmentList cId
        target = EditorForChannel cId
    return $ newEditState editorName attachmentListName target tId cId NewPost True reset

emptyEditStateForThread :: Maybe Aspell -> BCH.BChan MHEvent -> TeamId -> ChannelId -> EditMode -> IO (EditState Name)
emptyEditStateForThread checker eventQueue tId cId initialEditMode = do
    reset <- case checker of
        Nothing -> return Nothing
        Just as -> Just <$> (newSpellCheckTimer as eventQueue $ MITeamThread tId)
    let editorName = ThreadMessageInput cId
        attachmentListName = ThreadEditorAttachmentList cId
        target = EditorForThread tId
    return $ newEditState editorName attachmentListName target (Just tId) cId initialEditMode False reset

newThreadInterface :: Maybe Aspell
                   -> BCH.BChan MHEvent
                   -> TeamId
                   -> ChannelId
                   -> Message
                   -> Post
                   -> Messages
                   -> IO ThreadInterface
newThreadInterface checker eventQueue tId cId rootMsg rootPost msgs = do
    es <- emptyEditStateForThread checker eventQueue tId cId (Replying rootMsg rootPost)
    return $ newMessageInterface cId (postId rootPost) msgs es (MITeamThread tId) (FromThreadIn cId)

newChannelMessageInterface :: Maybe Aspell
                           -> BCH.BChan MHEvent
                           -> Maybe TeamId
                           -> ChannelId
                           -> Messages
                           -> IO ChannelMessageInterface
newChannelMessageInterface checker eventQueue tId cId msgs = do
    es <- emptyEditStateForChannel checker eventQueue tId cId
    return $ newMessageInterface cId () msgs es (MIChannel cId) (FromChannel cId)

newMessageInterface :: ChannelId
                    -> i
                    -> Messages
                    -> EditState Name
                    -> MessageInterfaceTarget
                    -> URLListSource
                    -> MessageInterface Name i
newMessageInterface cId pId msgs es target src =
    let urlListName = UrlList eName
        eName = getName $ es^.esEditor
    in MessageInterface { _miMessages = msgs
                        , _miRootPostId = pId
                        , _miChannelId = cId
                        , _miMessageSelect = MessageSelectState Nothing
                        , _miMode = Compose
                        , _miEditor = es
                        , _miTarget = target
                        , _miUrlListSource = src
                        , _miUrlList = URLList { _ulList = list urlListName mempty 2
                                               , _ulSource = Nothing
                                               }
                        , _miSaveAttachmentDialog = newSaveAttachmentDialog eName "(unused)"
                        }

newTeamState :: Config
             -> Team
             -> Z.Zipper ChannelListGroup ChannelListEntry
             -> TeamState
newTeamState config team chanList =
    let tId = teamId team
    in TeamState { _tsModeStack                = newStack Main
                 , _tsFocus                    = chanList
                 , _tsTeam                     = team
                 , _tsPostListWindow           = PostListWindowState emptyDirSeq Nothing
                 , _tsUserListWindow           = nullUserListWindowState tId
                 , _tsChannelListWindow        = nullChannelListWindowState tId
                 , _tsChannelSelectState       = emptyChannelSelectState tId
                 , _tsChannelTopicDialog       = newChannelTopicDialog tId ""
                 , _tsNotifyPrefs              = Nothing
                 , _tsPendingChannelChange     = Nothing
                 , _tsRecentChannel            = Nothing
                 , _tsReturnChannel            = Nothing
                 , _tsViewedMessage            = Nothing
                 , _tsThemeListWindow          = nullThemeListWindowState tId
                 , _tsReactionEmojiListWindow  = nullEmojiListWindowState tId
                 , _tsChannelListSorting       = configChannelListSorting config
                 , _tsThreadInterface          = Nothing
                 , _tsMessageInterfaceFocus    = FocusCurrentChannel
                 }

nullChannelListWindowState :: TeamId -> ListWindowState Channel ChannelSearchScope
nullChannelListWindowState tId =
    let newList rs = list (JoinChannelList tId) rs 2
    in ListWindowState { _listWindowSearchResults  = newList mempty
                       , _listWindowSearchInput    = editor (JoinChannelListSearchInput tId) (Just 1) ""
                       , _listWindowSearchScope    = AllChannels
                       , _listWindowSearching      = False
                       , _listWindowEnterHandler   = const $ return False
                       , _listWindowNewList        = newList
                       , _listWindowFetchResults   = const $ const $ const $ return mempty
                       , _listWindowRecordCount    = Nothing
                       }

nullThemeListWindowState :: TeamId -> ListWindowState InternalTheme ()
nullThemeListWindowState tId =
    let newList rs = list (ThemeListSearchResults tId) rs 3
    in ListWindowState { _listWindowSearchResults  = newList mempty
                       , _listWindowSearchInput    = editor (ThemeListSearchInput tId) (Just 1) ""
                       , _listWindowSearchScope    = ()
                       , _listWindowSearching      = False
                       , _listWindowEnterHandler   = const $ return False
                       , _listWindowNewList        = newList
                       , _listWindowFetchResults   = const $ const $ const $ return mempty
                       , _listWindowRecordCount    = Nothing
                       }

nullUserListWindowState :: TeamId -> ListWindowState UserInfo UserSearchScope
nullUserListWindowState tId =
    let newList rs = list (UserListSearchResults tId) rs 1
    in ListWindowState { _listWindowSearchResults  = newList mempty
                       , _listWindowSearchInput    = editor (UserListSearchInput tId) (Just 1) ""
                       , _listWindowSearchScope    = AllUsers Nothing
                       , _listWindowSearching      = False
                       , _listWindowEnterHandler   = const $ return False
                       , _listWindowNewList        = newList
                       , _listWindowFetchResults   = const $ const $ const $ return mempty
                       , _listWindowRecordCount    = Nothing
                       }

nullEmojiListWindowState :: TeamId -> ListWindowState (Bool, T.Text) ()
nullEmojiListWindowState tId =
    let newList rs = list (ReactionEmojiList tId) rs 1
    in ListWindowState { _listWindowSearchResults  = newList mempty
                       , _listWindowSearchInput    = editor (ReactionEmojiListInput tId) (Just 1) ""
                       , _listWindowSearchScope    = ()
                       , _listWindowSearching      = False
                       , _listWindowEnterHandler   = const $ return False
                       , _listWindowNewList        = newList
                       , _listWindowFetchResults   = const $ const $ const $ return mempty
                       , _listWindowRecordCount    = Nothing
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
newSaveAttachmentDialog :: Name -> T.Text -> SaveAttachmentDialogState Name
newSaveAttachmentDialog n t =
    SaveAttachmentDialogState { _attachmentPathEditor = applyEdit Z2.gotoEOL $
                                                        editor (AttachmentPathEditor n) (Just 1) t
                              , _attachmentPathDialogFocus = focusRing [ AttachmentPathEditor n
                                                                       , AttachmentPathSaveButton n
                                                                       , AttachmentPathCancelButton n
                                                                       ]
                              }

makeClientChannel :: (MonadIO m)
                  => BCH.BChan MHEvent
                  -> Maybe Aspell
                  -> UserId
                  -> Maybe TeamId
                  -> Channel
                  -> ChannelMember
                  -> m ClientChannel
makeClientChannel eventQueue spellChecker myId tId nc member = do
    msgs <- emptyChannelMessages
    mi <- liftIO $ newChannelMessageInterface spellChecker eventQueue tId (getId nc) msgs
    return ClientChannel { _ccInfo = initialChannelInfo myId nc member
                         , _ccMessageInterface = mi
                         }

initialChannelInfo :: UserId -> Channel -> ChannelMember -> ChannelInfo
initialChannelInfo myId chan member =
    let updated  = chan ^. channelLastPostAtL
    in ChannelInfo { _cdChannelId              = chan^.channelIdL
                   , _cdTeamId                 = chan^.channelTeamIdL
                   , _cdViewed                 = Nothing
                   , _cdNewMessageIndicator    = Hide
                   , _cdEditedMessageThreshold = Nothing
                   , _cdMentionCount           = 0
                   , _cdUpdated                = updated
                   , _cdName                   = preferredChannelName chan
                   , _cdDisplayName            = sanitizeUserText $ channelDisplayName chan
                   , _cdHeader                 = sanitizeUserText $ chan^.channelHeaderL
                   , _cdPurpose                = sanitizeUserText $ chan^.channelPurposeL
                   , _cdType                   = chan^.channelTypeL
                   , _cdNotifyProps            = emptyChannelNotifyProps
                   , _cdDMUserId               = if chan^.channelTypeL == Direct
                                                 then userIdForDMChannel myId $
                                                      sanitizeUserText $ channelName chan
                                                 else Nothing
                   , _cdSidebarShowOverride    = Nothing
                   , _cdFetchPending           = False
                   , _cdTotalMessageCount      = channelTotalMsgCount chan
                   , _cdViewedMessageCount     = channelMemberMsgCount member
                   }
