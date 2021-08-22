module Matterhorn.Events
  ( onEvent
  , globalKeybindings
  , globalKeyHandlers
  )
where

import           Prelude ()
import           Matterhorn.Prelude

import           Brick
import qualified Data.Text as T
import           GHC.Exception ( fromException )
import qualified Graphics.Vty as Vty
import           Lens.Micro.Platform ( (.=), _2, singular, _Just )
import qualified System.IO.Error as IO

import qualified Network.Mattermost.Endpoints as MM
import           Network.Mattermost.Exceptions ( mattermostErrorMessage )

import           Matterhorn.Connection
import           Matterhorn.Constants ( userSigil, normalChannelSigil )
import           Matterhorn.HelpTopics
import           Matterhorn.State.ChannelList
import           Matterhorn.State.Channels
import           Matterhorn.State.Common
import           Matterhorn.State.Help
import           Matterhorn.State.Messages
import           Matterhorn.State.Teams ( setTeam )
import           Matterhorn.State.ListOverlay ( listOverlayActivate )
import           Matterhorn.Types

import           Matterhorn.Events.ChannelSelect
import           Matterhorn.Events.ChannelTopicWindow
import           Matterhorn.Events.SaveAttachmentWindow
import           Matterhorn.Events.DeleteChannelConfirm
import           Matterhorn.Events.Keybindings
import           Matterhorn.Events.LeaveChannelConfirm
import           Matterhorn.Events.Main
import           Matterhorn.Events.MessageSelect
import           Matterhorn.Events.ThemeListOverlay
import           Matterhorn.Events.PostListOverlay
import           Matterhorn.Events.ShowHelp
import           Matterhorn.Events.UrlSelect
import           Matterhorn.Events.UserListOverlay
import           Matterhorn.Events.ChannelListOverlay
import           Matterhorn.Events.ReactionEmojiListOverlay
import           Matterhorn.State.Reactions ( toggleReaction )
import           Matterhorn.Events.TabbedWindow
import           Matterhorn.Events.ManageAttachments
import           Matterhorn.Events.EditNotifyPrefs
import           Matterhorn.Events.Websocket
import           Matterhorn.State.Links ( openLinkTarget )

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
onBrickEvent (MouseUp {}) = do
    mhLog LogGeneral "MOUSE UP EVENT"
    csLastMouseDownEvent .= Nothing
onBrickEvent e@(MouseDown n button modifier clickLoc) = do
    mhLog LogGeneral $ T.pack $ "MOUSE EVENT: " <> show (n, button, modifier)
    lastClick <- use csLastMouseDownEvent
    let shouldHandle = case lastClick of
            Nothing -> True
            Just (MouseDown prevN _ _ _) -> not $ prevN `semeq` n
            _ -> False
    when shouldHandle $ do
        mhLog LogGeneral "Handling mouse event"
        csLastMouseDownEvent .= Just e
        mode <- use (csCurrentTeam.tsMode)
        mouseHandlerByMode mode n button modifier clickLoc

mouseHandlerByMode :: Mode -> Name -> Vty.Button -> [Vty.Modifier] -> Location -> MH ()
mouseHandlerByMode mode =
    case mode of
        ChannelSelect            -> channelSelectMouseHandler
        EditNotifyPrefs          -> editNotifyPrefsMouseHandler
        ReactionEmojiListOverlay -> reactionEmojiListMouseHandler
        UrlSelect                -> urlListMouseHandler
        _                        -> globalMouseHandler

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
    handleWebsocketEvent we
onAppEvent (WSActionResponse r) =
    handleWebsocketActionResponse r
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

-- Handle mouse click events.
--
-- Note that the handler for each case may need to check the application
-- mode before handling the click. This is because some mouse events
-- only make sense when the UI is displaying certain contents. While
-- it's true that we probably wouldn't even get the click events in the
-- first place (because the UI element would only cause a click event
-- to be reported if it was actually rendered), there are cases when we
-- can get clicks on UI elements that *are* clickable even though those
-- clicks don't make sense for the application mode. A concrete example
-- of this is when we display the current channel's contents in one
-- layer, in monochrome, and then display a modal dialog box on top of
-- that. We probably *should* ignore clicks on the lower layer because
-- that's not the mode the application is in, but getting that right
-- could be hard because we'd have to figure out all possible modes
-- where those lower-layer clicks would be nonsensical. We don't bother
-- doing that in the harder cases; instead we just handle the clicks
-- and do what we would ordinarily do, assuming that there's no real
-- harm done. The worst that could happen is that a user could click
-- accidentally on a grayed-out URL (in a message, say) next to a modal
-- dialog box and then see the URL get opened. That would be weird, but
-- it isn't the end of the world.
globalMouseHandler :: Name -> Vty.Button -> [Vty.Modifier] -> Location -> MH ()
globalMouseHandler (ClickableChannelListEntry channelId) Vty.BLeft [] _ = do
    whenMode Main $ do
        resetReturnChannel
        setFocus channelId
        setMode Main
globalMouseHandler (ClickableTeamListEntry teamId) Vty.BLeft [] _ =
    -- We deliberately handle this event in all modes; this allows us to
    -- switch the UI to another team regardless of what state it is in,
    -- which is by design since all teams have their own UI states.
    setTeam teamId
globalMouseHandler (ClickableURLInMessage _ _ t) Vty.BLeft [] _ =
    void $ openLinkTarget t
globalMouseHandler (ClickableURL _ _ t) Vty.BLeft [] _ =
    void $ openLinkTarget t
globalMouseHandler (ClickableUsernameInMessage _ _ username) Vty.BLeft [] _ =
    changeChannelByName $ userSigil <> username
globalMouseHandler (ClickableUsername _ _ username) Vty.BLeft [] _ =
    changeChannelByName $ userSigil <> username
globalMouseHandler (ClickableAttachment fId) Vty.BLeft [] _ =
    void $ openLinkTarget $ LinkFileId fId
globalMouseHandler (ClickableReactionInMessage pId t uIds) Vty.BLeft [] _ =
    void $ toggleReaction pId t uIds
globalMouseHandler (ClickableReaction pId t uIds) Vty.BLeft [] _ =
    void $ toggleReaction pId t uIds
globalMouseHandler _ _ _ _ =
    return ()

editNotifyPrefsMouseHandler :: Name -> Vty.Button -> [Vty.Modifier] -> Location -> MH ()
editNotifyPrefsMouseHandler n b mods l =
    handleEditNotifyPrefsEvent (MouseDown n b mods l)

urlListMouseHandler :: Name -> Vty.Button -> [Vty.Modifier] -> Location -> MH ()
urlListMouseHandler (ClickableURLListEntry _ t) Vty.BLeft [] _ =
    void $ openLinkTarget t
urlListMouseHandler _ _ _ _ =
    return ()

channelSelectMouseHandler :: Name -> Vty.Button -> [Vty.Modifier] -> Location -> MH ()
channelSelectMouseHandler (ChannelSelectEntry match) Vty.BLeft [] _ = do
    setMode Main
    setFocus $ channelListEntryChannelId $ matchEntry match
channelSelectMouseHandler _ _ _ _ =
    return ()

reactionEmojiListMouseHandler :: Name -> Vty.Button -> [Vty.Modifier] -> Location -> MH ()
reactionEmojiListMouseHandler (ReactionEmojiListOverlayEntry val) Vty.BLeft [] _ =
    listOverlayActivate (csCurrentTeam.tsReactionEmojiListOverlay) val
reactionEmojiListMouseHandler _ _ _ _ =
    return ()

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
formatError (AttachmentException e) =
    case fromException e of
      Just (ioe :: IO.IOError) ->
          if IO.isDoesNotExistError ioe
          then "Error attaching, file does not exist!"
          else if IO.isPermissionError ioe
               then "Error attaching, lacking permissions to read file!"
               else "Unable to attach the requested file.  Check that it exists and has proper permissions."
      Nothing -> "Unknown error attaching file!\n" <>
          "Please report this error at https://github.com/matterhorn-chat/matterhorn/issues"
          -- this case shouldn't be reached
formatError (BadAttachmentPath msg) =
    msg
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
    mode <- use (csCurrentTeam.tsMode)
    globalHandlerByMode mode e

globalHandlerByMode :: Mode -> Vty.Event -> MH ()
globalHandlerByMode mode =
    case mode of
        Main                       -> onEventMain
        ShowHelp _ _               -> void . onEventShowHelp
        ChannelSelect              -> void . onEventChannelSelect
        UrlSelect                  -> void . onEventUrlSelect
        LeaveChannelConfirm        -> onEventLeaveChannelConfirm
        MessageSelect              -> onEventMessageSelect
        MessageSelectDeleteConfirm -> onEventMessageSelectDeleteConfirm
        DeleteChannelConfirm       -> onEventDeleteChannelConfirm
        ThemeListOverlay           -> onEventThemeListOverlay
        PostListOverlay _          -> onEventPostListOverlay
        UserListOverlay            -> onEventUserListOverlay
        ChannelListOverlay         -> onEventChannelListOverlay
        ReactionEmojiListOverlay   -> onEventReactionEmojiListOverlay
        ViewMessage                -> void . handleTabbedWindowEvent
                                             (csCurrentTeam.tsViewedMessage.singular _Just._2)
        ManageAttachments          -> onEventManageAttachments
        ManageAttachmentsBrowseFiles -> onEventManageAttachments
        EditNotifyPrefs            -> void . onEventEditNotifyPrefs
        ChannelTopicWindow         -> onEventChannelTopicWindow
        SaveAttachmentWindow _     -> onEventSaveAttachmentWindow

globalKeybindings :: KeyConfig -> KeyHandlerMap
globalKeybindings = mkKeybindings globalKeyHandlers

globalKeyHandlers :: [KeyEventHandler]
globalKeyHandlers =
    [ mkKb ShowHelpEvent
        "Show this help screen"
        (showHelpScreen mainHelpTopic)
    ]

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
            updateSidebar Nothing
