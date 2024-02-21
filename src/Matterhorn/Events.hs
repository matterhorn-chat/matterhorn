module Matterhorn.Events
  ( onEvent
  , setWindowSize
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

import qualified Network.Mattermost.Types as MM
import qualified Network.Mattermost.Endpoints as MM
import           Network.Mattermost.Exceptions ( mattermostErrorMessage )

import           Matterhorn.Connection
import           Matterhorn.Constants ( userSigil, normalChannelSigil )
import           Matterhorn.HelpTopics
import           Matterhorn.State.ChannelList
import           Matterhorn.State.Channels
import           Matterhorn.State.Common
import           Matterhorn.State.Messages
import           Matterhorn.Types

import           Matterhorn.Events.ChannelSelect
import           Matterhorn.Events.ChannelTopicWindow
import           Matterhorn.Events.DeleteChannelConfirm
import           Matterhorn.Events.Global
import           Matterhorn.Events.LeaveChannelConfirm
import           Matterhorn.Events.Main
import           Matterhorn.Events.MessageSelect
import           Matterhorn.Events.ThemeListWindow
import           Matterhorn.Events.PostListWindow
import           Matterhorn.Events.ShowHelp
import           Matterhorn.Events.UserListWindow
import           Matterhorn.Events.ChannelListWindow
import           Matterhorn.Events.ReactionEmojiListWindow
import           Matterhorn.Events.TabbedWindow
import           Matterhorn.Events.Mouse
import           Matterhorn.Events.EditNotifyPrefs
import           Matterhorn.Events.Websocket

onEvent :: BrickEvent Name MHEvent -> EventM Name ChatState ()
onEvent ev = runMHEvent $ do
    onBrickEvent ev
    doPendingUserFetches
    doPendingUserStatusFetches

onBrickEvent :: BrickEvent Name MHEvent -> MH ()
onBrickEvent (AppEvent e) =
    onAppEvent e
onBrickEvent (VtyEvent (Vty.EvKey (Vty.KChar 'l') [Vty.MCtrl])) = do
    csLastMouseDownEvent .= Nothing
    vty <- mh getVtyHandle
    liftIO $ Vty.refresh vty
onBrickEvent (VtyEvent e) = do
    csLastMouseDownEvent .= Nothing
    onVtyEvent e
onBrickEvent e@(MouseDown n _ _ _) = do
    lastClick <- use csLastMouseDownEvent
    let shouldHandle = case lastClick of
            Nothing -> True
            Just (MouseDown prevN _ _ _) -> not $ prevN `semeq` n
            _ -> False
    when shouldHandle $ do
        csLastMouseDownEvent .= Just e
        withCurrentTeam $ \tId -> do
            mode <- getTeamMode tId
            mouseHandlerByMode tId mode e
onBrickEvent (MouseUp {}) = do
    csLastMouseDownEvent .= Nothing
    mhContinueWithoutRedraw

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
    withCurrentTeam fetchVisibleIfNeeded
    forEachTeam $ \tId -> do
        baseUrl <- getServerBaseUrl tId
        mhLog LogGeneral $ T.pack $ "Team base URL for team ID " <> show tId <> ": " <> show baseUrl
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
    postErrorMessage' $ formatMHError e
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

formatMHError :: MHError -> T.Text
formatMHError (GenericError msg) =
    msg
formatMHError (NoSuchChannel chan) =
    T.pack $ "No such channel: " <> show chan
formatMHError (NoSuchUser user) =
    T.pack $ "No such user: " <> show user
formatMHError (AmbiguousName name) =
    (T.pack $ "The input " <> show name <> " matches both channels ") <>
    "and users. Try using '" <> userSigil <> "' or '" <>
    normalChannelSigil <> "' to disambiguate."
formatMHError (ServerError e) =
    mattermostErrorMessage e
formatMHError (ClipboardError msg) =
    msg
formatMHError (ConfigOptionMissing opt) =
    T.pack $ "Config option " <> show opt <> " missing"
formatMHError (ProgramExecutionFailed progName logPath) =
    T.pack $ "An error occurred when running " <> show progName <>
             "; see " <> show logPath <> " for details."
formatMHError (NoSuchScript name) =
    "No script named " <> name <> " was found"
formatMHError (NoSuchHelpTopic topic) =
    let knownTopics = ("  - " <>) <$> helpTopicName <$> helpTopics
    in "Unknown help topic: `" <> topic <> "`. " <>
       (T.unlines $ "Available topics are:" : knownTopics)
formatMHError (AttachmentException e) =
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
formatMHError (BadAttachmentPath msg) =
    msg
formatMHError (AsyncErrEvent e) =
    "An unexpected error has occurred! The exception encountered was:\n  " <>
    T.pack (show e) <>
    "\nPlease report this error at https://github.com/matterhorn-chat/matterhorn/issues"

onVtyEvent :: Vty.Event -> MH ()
onVtyEvent =
    void .
    handleEventWith [ handleResizeEvent
                    , mhHandleKeyboardEvent globalKeybindings
                    , handleTeamModeEvent
                    ]

handleResizeEvent :: Vty.Event -> MH Bool
handleResizeEvent (Vty.EvResize w h) = do
    setWindowSize w h
    return True
handleResizeEvent _ =
    return False

setWindowSize :: Int -> Int -> MH ()
setWindowSize w h = do
    -- On resize, invalidate the entire rendering cache since many
    -- things depend on the window size.
    --
    -- Note: we fall through after this because it is sometimes
    -- important for modes to have their own additional logic to run
    -- when a resize occurs, so we don't want to stop processing here.
    csResources.crWindowSize .= (w, h)

    mh invalidateCache
    withCurrentTeam $ \tId ->
        mh $ makeVisible $ SelectedChannelListEntry tId

handleTeamModeEvent :: Vty.Event -> MH Bool
handleTeamModeEvent e = do
    withCurrentTeam $ \tId -> do
        mode <- getTeamMode tId
        teamEventHandlerByMode tId mode e
    return True

teamEventHandlerByMode :: MM.TeamId -> Mode -> Vty.Event -> MH ()
teamEventHandlerByMode tId mode e =
    case mode of
        Main                       -> onEventMain tId e
        ShowHelp _                 -> void $ onEventShowHelp tId e
        ChannelSelect              -> void $ onEventChannelSelect tId e
        LeaveChannelConfirm        -> onEventLeaveChannelConfirm tId e
        MessageSelectDeleteConfirm target ->
            case target of
                MITeamThread tmId ->
                    onEventMessageSelectDeleteConfirm tId (unsafeThreadInterface(tmId)) e
                MIChannel cId ->
                    onEventMessageSelectDeleteConfirm tId (csChannelMessageInterface(cId)) e
        DeleteChannelConfirm       -> onEventDeleteChannelConfirm tId e
        ThemeListWindow            -> onEventThemeListWindow tId e
        PostListWindow _           -> onEventPostListWindow tId e
        UserListWindow             -> onEventUserListWindow tId e
        ChannelListWindow          -> onEventChannelListWindow tId e
        ReactionEmojiListWindow    -> onEventReactionEmojiListWindow tId e
        ViewMessage                -> void $ (handleTabbedWindowEvent
                                              (csTeam(tId).tsViewedMessage.singular _Just._2)
                                              tId e)
        EditNotifyPrefs            -> void $ onEventEditNotifyPrefs tId e
        ChannelTopicWindow         -> onEventChannelTopicWindow tId e

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
