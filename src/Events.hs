module Events
  ( onEvent
  , globalKeybindings
  , globalKeyHandlers
  )
where

import           Prelude ()
import           Prelude.MH

import           Brick
import qualified Data.Sequence as Seq
import qualified Data.Set as Set
import qualified Data.Text as T
import qualified Graphics.Vty as Vty
import           Lens.Micro.Platform ( (.=), preuse, _2, singular, _Just )

import qualified Network.Mattermost.Endpoints as MM
import           Network.Mattermost.Exceptions ( mattermostErrorMessage )
import           Network.Mattermost.Lenses
import           Network.Mattermost.Types
import           Network.Mattermost.WebSocket

import           Connection
import           Constants ( userSigil, normalChannelSigil )
import           HelpTopics
import           State.Channels
import           State.Common
import           State.Flagging
import           State.Help
import           State.Messages
import           State.Reactions
import           State.Users
import           Types
import           Types.Common

import           Events.ChannelSelect
import           Events.DeleteChannelConfirm
import           Events.Keybindings
import           Events.LeaveChannelConfirm
import           Events.Main
import           Events.MessageSelect
import           Events.ThemeListOverlay
import           Events.PostListOverlay
import           Events.ShowHelp
import           Events.UrlSelect
import           Events.UserListOverlay
import           Events.ChannelListOverlay
import           Events.ReactionEmojiListOverlay
import           Events.TabbedWindow
import           Events.ManageAttachments
import           Events.EditNotifyPrefs


onEvent :: ChatState -> BrickEvent Name MHEvent -> EventM Name (Next ChatState)
onEvent st ev = runMHEvent st $ do
    onBrickEvent ev
    doPendingUserFetches
    doPendingUserStatusFetches

onBrickEvent :: BrickEvent Name MHEvent -> MH ()
onBrickEvent (AppEvent e) =
    onAppEvent e
onBrickEvent (VtyEvent (Vty.EvKey (Vty.KChar 'l') [Vty.MCtrl])) = do
    vty <- mh getVtyHandle
    liftIO $ Vty.refresh vty
onBrickEvent (VtyEvent e) =
    onVtyEvent e
onBrickEvent _ =
    return ()

onAppEvent :: MHEvent -> MH ()
onAppEvent RefreshWebsocketEvent =
    connectWebsockets
onAppEvent WebsocketDisconnect = do
    csConnectionStatus .= Disconnected
    disconnectChannels
onAppEvent WebsocketConnect = do
    csConnectionStatus .= Connected
    refreshChannelsAndUsers
    refreshClientConfig
    fetchVisibleIfNeeded
onAppEvent (RateLimitExceeded winSz) =
    mhError $ GenericError $ T.pack $
        let s = if winSz == 1 then "" else "s"
        in "The server's API request rate limit was exceeded; Matterhorn will " <>
           "retry the failed request in " <> show winSz <> " second" <> s <>
           ". Please contact your Mattermost administrator " <>
           "about API rate limiting issues."
onAppEvent RateLimitSettingsMissing =
    mhError $ GenericError $
        "A request was rate-limited but could not be retried due to rate " <>
        "limit settings missing"
onAppEvent RequestDropped =
    mhError $ GenericError $
        "An API request was retried and dropped due to a rate limit. Matterhorn " <>
        "may now be inconsistent with the server. Please contact your " <>
        "Mattermost administrator about API rate limiting issues."
onAppEvent BGIdle =
    csWorkerIsBusy .= Nothing
onAppEvent (BGBusy n) =
    csWorkerIsBusy .= Just n
onAppEvent (WSEvent we) =
    handleWSEvent we
onAppEvent (WSActionResponse r) =
    handleWSActionResponse r
onAppEvent (RespEvent f) = f
onAppEvent (WebsocketParseError e) = do
    let msg = "A websocket message could not be parsed:\n  " <>
              T.pack e <>
              "\nPlease report this error at https://github.com/matterhorn-chat/matterhorn/issues"
    mhError $ GenericError msg
onAppEvent (IEvent e) = do
    handleIEvent e

handleIEvent :: InternalEvent -> MH ()
handleIEvent (DisplayError e) =
    postErrorMessage' $ formatError e
handleIEvent (LoggingStarted path) =
    postInfoMessage $ "Logging to " <> T.pack path
handleIEvent (LogDestination dest) =
    case dest of
        Nothing ->
            postInfoMessage "Logging is currently disabled. Enable it with /log-start."
        Just path ->
            postInfoMessage $ T.pack $ "Logging to " <> path
handleIEvent (LogSnapshotSucceeded path) =
    postInfoMessage $ "Log snapshot written to " <> T.pack path
handleIEvent (LoggingStopped path) =
    postInfoMessage $ "Stopped logging to " <> T.pack path
handleIEvent (LogStartFailed path err) =
    postErrorMessage' $ "Could not start logging to " <> T.pack path <>
                        ", error: " <> T.pack err
handleIEvent (LogSnapshotFailed path err) =
    postErrorMessage' $ "Could not write log snapshot to " <> T.pack path <>
                        ", error: " <> T.pack err

formatError :: MHError -> T.Text
formatError (GenericError msg) =
    msg
formatError (NoSuchChannel chan) =
    T.pack $ "No such channel: " <> show chan
formatError (NoSuchUser user) =
    T.pack $ "No such user: " <> show user
formatError (AmbiguousName name) =
    (T.pack $ "The input " <> show name <> " matches both channels ") <>
    "and users. Try using '" <> userSigil <> "' or '" <>
    normalChannelSigil <> "' to disambiguate."
formatError (ServerError e) =
    mattermostErrorMessage e
formatError (ClipboardError msg) =
    msg
formatError (ConfigOptionMissing opt) =
    T.pack $ "Config option " <> show opt <> " missing"
formatError (ProgramExecutionFailed progName logPath) =
    T.pack $ "An error occurred when running " <> show progName <>
             "; see " <> show logPath <> " for details."
formatError (NoSuchScript name) =
    "No script named " <> name <> " was found"
formatError (NoSuchHelpTopic topic) =
    let knownTopics = ("  - " <>) <$> helpTopicName <$> helpTopics
    in "Unknown help topic: `" <> topic <> "`. " <>
       (T.unlines $ "Available topics are:" : knownTopics)
formatError (AsyncErrEvent e) =
    "An unexpected error has occurred! The exception encountered was:\n  " <>
    T.pack (show e) <>
    "\nPlease report this error at https://github.com/matterhorn-chat/matterhorn/issues"

onVtyEvent :: Vty.Event -> MH ()
onVtyEvent e = do
    case e of
        (Vty.EvResize _ _) ->
            -- On resize, invalidate the entire rendering cache since
            -- many things depend on the window size.
            --
            -- Note: we fall through after this because it is sometimes
            -- important for modes to have their own additional logic
            -- to run when a resize occurs, so we don't want to stop
            -- processing here.
            mh invalidateCache
        _ -> return ()

    void $ handleKeyboardEvent globalKeybindings handleGlobalEvent e

handleGlobalEvent :: Vty.Event -> MH ()
handleGlobalEvent e = do
    mode <- gets appMode
    case mode of
        Main                       -> onEventMain e
        ShowHelp _ _               -> void $ onEventShowHelp e
        ChannelSelect              -> void $ onEventChannelSelect e
        UrlSelect                  -> void $ onEventUrlSelect e
        LeaveChannelConfirm        -> onEventLeaveChannelConfirm e
        MessageSelect              -> onEventMessageSelect e
        MessageSelectDeleteConfirm -> onEventMessageSelectDeleteConfirm e
        DeleteChannelConfirm       -> onEventDeleteChannelConfirm e
        ThemeListOverlay           -> onEventThemeListOverlay e
        PostListOverlay _          -> onEventPostListOverlay e
        UserListOverlay            -> onEventUserListOverlay e
        ChannelListOverlay         -> onEventChannelListOverlay e
        ReactionEmojiListOverlay   -> onEventReactionEmojiListOverlay e
        ViewMessage                -> void $ handleTabbedWindowEvent (csViewedMessage.singular _Just._2) e
        ManageAttachments          -> onEventManageAttachments e
        ManageAttachmentsBrowseFiles -> onEventManageAttachments e
        EditNotifyPrefs            -> void $ onEventEditNotifyPrefs e

globalKeybindings :: KeyConfig -> KeyHandlerMap
globalKeybindings = mkKeybindings globalKeyHandlers

globalKeyHandlers :: [KeyEventHandler]
globalKeyHandlers =
    [ mkKb ShowHelpEvent
        "Show this help screen"
        (showHelpScreen mainHelpTopic)
    ]

handleWSActionResponse :: WebsocketActionResponse -> MH ()
handleWSActionResponse r =
    case warStatus r of
        WebsocketActionStatusOK -> return ()

handleWSEvent :: WebsocketEvent -> MH ()
handleWSEvent we = do
    myId <- gets myUserId
    myTId <- gets myTeamId
    case weEvent we of
        WMPosted
            | Just p <- wepPost (weData we) ->
                when (wepTeamId (weData we) == Just myTId ||
                      wepTeamId (weData we) == Nothing) $ do
                    let wasMentioned = maybe False (Set.member myId) $ wepMentions (weData we)
                    addNewPostedMessage $ RecentPost p wasMentioned
                    cId <- use csCurrentChannelId
                    when (postChannelId p /= cId) $
                        showChannelInSidebar (p^.postChannelIdL) False
            | otherwise -> return ()

        WMPostEdited
            | Just p <- wepPost (weData we) -> do
                editMessage p
                cId <- use csCurrentChannelId
                when (postChannelId p == cId) (updateViewed False)
                when (postChannelId p /= cId) $
                    showChannelInSidebar (p^.postChannelIdL) False
            | otherwise -> return ()

        WMPostDeleted
            | Just p <- wepPost (weData we) -> do
                deleteMessage p
                cId <- use csCurrentChannelId
                when (postChannelId p == cId) (updateViewed False)
                when (postChannelId p /= cId) $
                    showChannelInSidebar (p^.postChannelIdL) False
            | otherwise -> return ()

        WMStatusChange
            | Just status <- wepStatus (weData we)
            , Just uId <- wepUserId (weData we) ->
                setUserStatus uId status
            | otherwise -> return ()

        WMUserAdded
            | Just cId <- webChannelId (weBroadcast we) ->
                when (wepUserId (weData we) == Just myId &&
                      wepTeamId (weData we) == Just myTId) $
                    handleChannelInvite cId
            | otherwise -> return ()

        WMNewUser
            | Just uId <- wepUserId $ weData we ->
                handleNewUsers (Seq.singleton uId) (return ())
            | otherwise -> return ()

        WMUserRemoved
            | Just cId <- wepChannelId (weData we) ->
                when (webUserId (weBroadcast we) == Just myId) $
                    removeChannelFromState cId
            | otherwise -> return ()

        WMTyping
            | Just uId <- wepUserId $ weData we
            , Just cId <- webChannelId (weBroadcast we) -> handleTypingUser uId cId
            | otherwise -> return ()

        WMChannelDeleted
            | Just cId <- wepChannelId (weData we) ->
                when (webTeamId (weBroadcast we) == Just myTId) $
                    removeChannelFromState cId
            | otherwise -> return ()

        WMDirectAdded
            | Just cId <- webChannelId (weBroadcast we) -> handleChannelInvite cId
            | otherwise -> return ()

        -- An 'ephemeral message' is just Mattermost's version of our
        -- 'client message'. This can be a little bit wacky, e.g.
        -- if the user types '/shortcuts' in the browser, we'll get
        -- an ephemeral message even in MatterHorn with the browser
        -- shortcuts, but it's probably a good idea to handle these
        -- messages anyway.
        WMEphemeralMessage
            | Just p <- wepPost $ weData we -> postInfoMessage (sanitizeUserText $ p^.postMessageL)
            | otherwise -> return ()

        WMPreferenceChanged
            | Just prefs <- wepPreferences (weData we) ->
                mapM_ applyPreferenceChange prefs
            | otherwise -> return ()

        WMPreferenceDeleted
            | Just pref <- wepPreferences (weData we)
            , Just fps <- mapM preferenceToFlaggedPost pref ->
              forM_ fps $ \f ->
                  updateMessageFlag (flaggedPostId f) False
            | otherwise -> return ()

        WMReactionAdded
            | Just r <- wepReaction (weData we)
            , Just cId <- webChannelId (weBroadcast we) -> addReactions cId [r]
            | otherwise -> return ()

        WMReactionRemoved
            | Just r <- wepReaction (weData we)
            , Just cId <- webChannelId (weBroadcast we) -> removeReaction r cId
            | otherwise -> return ()

        WMChannelViewed
            | Just cId <- wepChannelId $ weData we -> refreshChannelById cId
            | otherwise -> return ()

        WMChannelUpdated
            | Just cId <- webChannelId $ weBroadcast we -> do
                mChan <- preuse (csChannel(cId))
                when (isJust mChan) $ do
                    refreshChannelById cId
                    updateSidebar
            | otherwise -> return ()

        WMGroupAdded
            | Just cId <- webChannelId (weBroadcast we) -> handleChannelInvite cId
            | otherwise -> return ()

        WMChannelMemberUpdated
            | Just channelMember <- wepChannelMember $ weData we ->
                  when (channelMemberUserId channelMember == myId) $
                      updateChannelNotifyProps
                      (channelMemberChannelId channelMember)
                      (channelMemberNotifyProps channelMember)
            | otherwise -> return ()

        -- We are pretty sure we should do something about these:
        WMAddedToTeam -> return ()

        -- We aren't sure whether there is anything we should do about
        -- these yet:
        WMUpdateTeam -> return ()
        WMTeamDeleted -> return ()
        WMUserUpdated -> return ()
        WMLeaveTeam -> return ()

        -- We deliberately ignore these events:
        WMChannelCreated -> return ()
        WMEmojiAdded -> return ()
        WMWebRTC -> return ()
        WMHello -> return ()
        WMAuthenticationChallenge -> return ()
        WMUserRoleUpdated -> return ()
        WMPluginStatusesChanged -> return ()
        WMPluginEnabled -> return ()
        WMPluginDisabled -> return ()
        WMUnknownEvent {} -> return ()

-- | Refresh client-accessible server configuration information. This
-- is usually triggered when a reconnect event for the WebSocket to the
-- server occurs.
refreshClientConfig :: MH ()
refreshClientConfig = do
    session <- getSession
    doAsyncWith Preempt $ do
        cfg <- MM.mmGetClientConfiguration (Just "old") session
        return $ Just $ do
            csClientConfig .= Just cfg
            updateSidebar
