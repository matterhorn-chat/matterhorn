{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
module Matterhorn.Command
  ( commandList
  , dispatchCommand
  , printArgSpec
  , toggleMessagePreview
  )
where

import           Prelude ()
import           Matterhorn.Prelude

import           Brick.Main ( invalidateCache )
import qualified Control.Exception as Exn
import qualified Data.Char as Char
import qualified Data.Text as T
import           Lens.Micro.Platform ( (%=) )

import qualified Network.Mattermost.Endpoints as MM
import qualified Network.Mattermost.Exceptions as MM
import qualified Network.Mattermost.Types as MM

import           Matterhorn.State.Attachments
import           Matterhorn.Connection ( connectWebsockets )
import           Matterhorn.Constants ( normalChannelSigil )
import           Matterhorn.HelpTopics
import           Matterhorn.Scripts
import           Matterhorn.State.Help
import           Matterhorn.State.Editing
import           Matterhorn.State.ChannelList
import           Matterhorn.State.Channels
import           Matterhorn.State.ChannelTopicWindow
import           Matterhorn.State.ChannelSelect
import           Matterhorn.State.Logging
import           Matterhorn.State.PostListOverlay
import           Matterhorn.State.UserListOverlay
import           Matterhorn.State.ChannelListOverlay
import           Matterhorn.State.ThemeListOverlay
import           Matterhorn.State.Messages
import           Matterhorn.State.NotifyPrefs
import           Matterhorn.State.Common ( postInfoMessage )
import           Matterhorn.State.Teams
import           Matterhorn.State.Users
import           Matterhorn.Themes ( attrForUsername )
import           Matterhorn.Types


-- | This function skips any initial whitespace and returns the first
-- 'token' (i.e. any sequence of non-whitespace characters) as well as
-- the trailing rest of the string, after any whitespace. This is used
-- for tokenizing the first bits of command input while leaving the
-- subsequent chunks unchanged, preserving newlines and other
-- important formatting.
unwordHead :: Text -> Maybe (Text, Text)
unwordHead t =
  let t' = T.dropWhile Char.isSpace t
      (w, rs)  = T.break Char.isSpace t'
  in if T.null w
       then Nothing
       else Just (w, T.dropWhile Char.isSpace rs)

printArgSpec :: CmdArgs a -> Text
printArgSpec NoArg = ""
printArgSpec (LineArg ts) = "<" <> ts <> ">"
printArgSpec (TokenArg t NoArg) = "<" <> t <> ">"
printArgSpec (UserArg rs) = "<" <> addUserSigil "user" <> ">" <> addSpace (printArgSpec rs)
printArgSpec (ChannelArg rs) = "<" <> normalChannelSigil <> "channel>" <> addSpace (printArgSpec rs)
printArgSpec (TokenArg t rs) = "<" <> t <> ">" <> addSpace (printArgSpec rs)

addSpace :: Text -> Text
addSpace "" = ""
addSpace t = " " <> t

matchArgs :: CmdArgs a -> Text -> Either Text a
matchArgs NoArg t = case unwordHead t of
  Nothing -> return ()
  Just (a, as)
    | not (T.all Char.isSpace as) -> Left ("Unexpected arguments '" <> t <> "'")
    | otherwise -> Left ("Unexpected argument '" <> a <> "'")
matchArgs (LineArg _) t = return t
matchArgs spec@(UserArg rs) t = case unwordHead t of
  Nothing -> case rs of
    NoArg -> Left ("Missing argument: " <> printArgSpec spec)
    _     -> Left ("Missing arguments: " <> printArgSpec spec)
  Just (a, as) -> (,) <$> pure a <*> matchArgs rs as
matchArgs spec@(ChannelArg rs) t = case unwordHead t of
  Nothing -> case rs of
    NoArg -> Left ("Missing argument: " <> printArgSpec spec)
    _     -> Left ("Missing arguments: " <> printArgSpec spec)
  Just (a, as) -> (,) <$> pure a <*> matchArgs rs as
matchArgs spec@(TokenArg _ rs) t = case unwordHead t of
  Nothing -> case rs of
    NoArg -> Left ("Missing argument: " <> printArgSpec spec)
    _     -> Left ("Missing arguments: " <> printArgSpec spec)
  Just (a, as) -> (,) <$> pure a <*> matchArgs rs as

commandList :: [Cmd]
commandList =
  [ Cmd "quit" "Exit Matterhorn" NoArg $ \ () -> requestQuit

  , Cmd "right" "Focus on the next channel" NoArg $ \ () -> do
        withCurrentTeam nextChannel

  , Cmd "left" "Focus on the previous channel" NoArg $ \ () -> do
        withCurrentTeam prevChannel

  , Cmd "create-channel" "Create a new public channel"
    (LineArg "channel name") $ \ name -> do
        withCurrentTeam $ \tId ->
            createOrdinaryChannel tId True name

  , Cmd "create-private-channel" "Create a new private channel"
    (LineArg "channel name") $ \ name -> do
        withCurrentTeam $ \tId ->
            createOrdinaryChannel tId False name

  , Cmd "delete-channel" "Delete the current channel"
    NoArg $ \ () -> do
        withCurrentTeam beginCurrentChannelDeleteConfirm

  , Cmd "hide" "Hide the current DM or group channel from the channel list"
    NoArg $ \ () -> do
        withCurrentTeam $ \tId ->
            withCurrentChannel tId $ \cId _ -> do
                hideDMChannel cId

  , Cmd "reconnect" "Force a reconnection attempt to the server"
    NoArg $ \ () ->
        connectWebsockets

  , Cmd "members" "Show the current channel's members"
    NoArg $ \ () -> do
        withCurrentTeam enterChannelMembersUserList

  , Cmd "leave" "Leave a normal channel or hide a DM channel" NoArg $ \ () -> do
        withCurrentTeam startLeaveCurrentChannel

  , Cmd "join" "Find a channel to join" NoArg $ \ () -> do
        withCurrentTeam enterChannelListOverlayMode

  , Cmd "join" "Join the specified channel" (ChannelArg NoArg) $ \(n, ()) -> do
        withCurrentTeam $ \tId ->
            joinChannelByName tId n

  , Cmd "theme" "List the available themes" NoArg $ \ () -> do
        withCurrentTeam enterThemeListMode

  , Cmd "theme" "Set the color theme"
    (TokenArg "theme" NoArg) $ \ (themeName, ()) -> do
        withCurrentTeam $ \tId ->
            setTheme tId themeName

  , Cmd "topic" "Set the current channel's topic (header) interactively"
    NoArg $ \ () -> do
        withCurrentTeam openChannelTopicWindow

  , Cmd "topic" "Set the current channel's topic (header)"
    (LineArg "topic") $ \ p -> do
        withCurrentTeam $ \tId ->
            if not (T.null p) then setChannelTopic tId p else return ()

  , Cmd "add-user" "Search for a user to add to the current channel"
    NoArg $ \ () -> do
        withCurrentTeam enterChannelInviteUserList

  , Cmd "msg" "Search for a user to enter a private chat"
    NoArg $ \ () -> do
        withCurrentTeam enterDMSearchUserList

  , Cmd "msg" "Chat with the specified user"
    (UserArg NoArg) $ \ (name, ()) -> do
        withCurrentTeam $ \tId ->
            changeChannelByName tId name

  , Cmd "username-attribute" "Display the attribute used to color the specified username"
    (UserArg NoArg) $ \ (name, ()) ->
        displayUsernameAttribute name

  , Cmd "msg" "Go to a user's channel and send the specified message or command"
    (UserArg $ LineArg "message or command") $ \ (name, msg) -> do
        withCurrentTeam $ \tId ->
            withFetchedUserMaybe (UserFetchByUsername name) $ \foundUser -> do
                case foundUser of
                    Just user -> createOrFocusDMChannel tId user $ Just $ \_ -> do
                        handleInputSubmission tId (channelEditor(tId))
                                                  (csCurrentChannelId(tId))
                                                  msg
                    Nothing -> mhError $ NoSuchUser name

  , Cmd "log-start" "Begin logging debug information to the specified path"
    (TokenArg "path" NoArg) $ \ (path, ()) ->
        startLogging $ T.unpack path

  , Cmd "log-snapshot" "Dump the current debug log buffer to the specified path"
    (TokenArg "path" NoArg) $ \ (path, ()) ->
        logSnapshot $ T.unpack path

  , Cmd "log-stop" "Stop logging"
    NoArg $ \ () ->
        stopLogging

  , Cmd "log-mark" "Add a custom marker message to the Matterhorn debug log"
    (LineArg "message") $ \ markMsg ->
        mhLog LogUserMark markMsg

  , Cmd "log-status" "Show current debug logging status"
    NoArg $ \ () ->
        getLogDestination

  , Cmd "add-user" "Add a user to the current channel"
    (UserArg NoArg) $ \ (uname, ()) -> do
        withCurrentTeam $ \tId ->
            addUserByNameToCurrentChannel tId uname

  , Cmd "remove" "Remove a user from the current channel"
    (UserArg NoArg) $ \ (uname, ()) -> do
        withCurrentTeam $ \tId ->
            removeUserFromCurrentChannel tId uname

  , Cmd "user" "Show users to initiate a private DM chat channel"
    -- n.b. this is identical to "msg", but is provided as an
    -- alternative mental model for useability.
    NoArg $ \ () -> do
        withCurrentTeam enterDMSearchUserList

  , Cmd "message-preview" "Toggle preview of the current message" NoArg $ \_ ->
        toggleMessagePreview

  , Cmd "toggle-truncate-verbatim-blocks" "Toggle truncation of verbatim and code blocks" NoArg $ \_ ->
        toggleVerbatimBlockTruncation

  , Cmd "toggle-channel-list" "Toggle channel list visibility" NoArg $ \_ ->
        toggleChannelListVisibility

  , Cmd "toggle-message-timestamps" "Toggle message timestamps" NoArg $ \_ ->
        toggleMessageTimestamps

  , Cmd "toggle-expanded-topics" "Toggle expanded channel topics" NoArg $ \_ ->
        toggleExpandedChannelTopics

  , Cmd "cycle-channel-list-sorting" "Cycle through channel list sorting modes for this team" NoArg $ \_ ->
        withCurrentTeam cycleChannelListSortingMode

  , Cmd "focus" "Focus on a channel or user"
    (ChannelArg NoArg) $ \ (name, ()) -> do
        withCurrentTeam $ \tId ->
            changeChannelByName tId name

  , Cmd "focus" "Select from available channels" NoArg $ \ () -> do
        withCurrentTeam beginChannelSelect

  , Cmd "help" "Show the main help screen" NoArg $ \ _ -> do
        withCurrentTeam $ \tId ->
            showHelpScreen tId mainHelpTopic

  , Cmd "shortcuts" "Show keyboard shortcuts" NoArg $ \ _ -> do
        withCurrentTeam $ \tId ->
            showHelpScreen tId mainHelpTopic

  , Cmd "help" "Show help about a particular topic"
      (TokenArg "topic" NoArg) $ \ (topicName, ()) -> do
          withCurrentTeam $ \tId ->
              case lookupHelpTopic topicName of
                  Nothing -> mhError $ NoSuchHelpTopic topicName
                  Just topic -> showHelpScreen tId topic

  , Cmd "sh" "List the available shell scripts" NoArg $ \ () ->
        listScripts

  , Cmd "group-create" "Create a group chat"
    (LineArg (addUserSigil "user" <> " [" <> addUserSigil "user" <> " ...]")) $ \ t -> do
        withCurrentTeam $ \tId ->
            createGroupChannel tId t

  , Cmd "sh" "Run a prewritten shell script"
    (TokenArg "script" (LineArg "message")) $ \ (script, text) -> do
        withCurrentTeam $ \tId ->
            withCurrentChannel tId $ \cId _ -> do
                findAndRunScript (channelEditor(tId)) cId script text

  , Cmd "flags" "Open a window of your flagged posts" NoArg $ \ () -> do
        withCurrentTeam enterFlaggedPostListMode

  , Cmd "pinned-posts" "Open a window of this channel's pinned posts" NoArg $ \ () -> do
        withCurrentTeam enterPinnedPostListMode

  , Cmd "search" "Search for posts with given terms" (LineArg "terms") $ \t -> do
        withCurrentTeam $ \tId ->
            enterSearchResultPostListMode tId t

  , Cmd "notify-prefs" "Edit the current channel's notification preferences" NoArg $ \_ -> do
        withCurrentTeam enterEditNotifyPrefsMode

  , Cmd "rename-channel-url" "Rename the current channel's URL name" (TokenArg "channel name" NoArg) $ \ (name, _) -> do
        withCurrentTeam $ \tId ->
            renameChannelUrl tId name

  , Cmd "move-team-left" "Move the currently-selected team to the left in the team list" NoArg $ \_ ->
        moveCurrentTeamLeft

  , Cmd "move-team-right" "Move the currently-selected team to the right in the team list" NoArg $ \_ ->
        moveCurrentTeamRight

  , Cmd "attach" "Attach a given file without browsing" (LineArg "path") $ \path -> do
        withCurrentTeam $ \tId ->
            attachFileByPath tId (channelEditor(tId)) path

  , Cmd "toggle-favorite" "Toggle the favorite status of the current channel" NoArg $ \_ -> do
        withCurrentTeam toggleChannelFavoriteStatus

  , Cmd "toggle-sidebar-group" "Toggle the visibility of the current channel's sidebar group" NoArg $ \_ -> do
        withCurrentTeam toggleCurrentChannelChannelListGroup

  , let names = T.intercalate "|" $ fst <$> channelListGroupNames
    in Cmd "toggle-sidebar-group" "Toggle the visibility of the named sidebar group" (LineArg names) $ \name -> do
        withCurrentTeam (toggleCurrentChannelChannelListGroupByName name)
  ]

displayUsernameAttribute :: Text -> MH ()
displayUsernameAttribute name = do
    let an = attrForUsername trimmed
        trimmed = trimUserSigil name
    postInfoMessage $ "The attribute used for " <> addUserSigil trimmed <>
                      " is " <> (attrNameToConfig an)

execMMCommand :: MM.TeamId -> Text -> Text -> MH ()
execMMCommand tId name rest = do
  withCurrentChannel tId $ \cId _ -> do
      session  <- getSession
      em       <- use (channelEditor(tId).cedEditMode)
      let mc = MM.MinCommand
                 { MM.minComChannelId = cId
                 , MM.minComCommand   = "/" <> name <> " " <> rest
                 , MM.minComParentId  = case em of
                     Replying _ p -> Just $ MM.getId p
                     Editing p _  -> MM.postRootId p
                     _            -> Nothing
                 , MM.minComRootId  = case em of
                     Replying _ p -> MM.postRootId p <|> (Just $ MM.postId p)
                     Editing p _  -> MM.postRootId p
                     _            -> Nothing
                 , MM.minComTeamId = tId
                 }
          runCmd = liftIO $ do
            void $ MM.mmExecuteCommand mc session
          handleHTTP (MM.HTTPResponseException err) =
            return (Just (T.pack err))
            -- XXX: this might be a bit brittle in the future, because it
            -- assumes the shape of an error message. We might want to
            -- think about a better way of discovering this error and
            -- reporting it accordingly?
          handleCmdErr (MM.MattermostServerError err) =
            let (_, msg) = T.breakOn ": " err in
              return (Just (T.drop 2 msg))
          handleMMErr (MM.MattermostError
                         { MM.mattermostErrorMessage = msg }) =
            return (Just msg)
      errMsg <- liftIO $ (runCmd >> return Nothing) `Exn.catch` handleHTTP
                                                    `Exn.catch` handleCmdErr
                                                    `Exn.catch` handleMMErr
      case errMsg of
        Nothing -> return ()
        Just err ->
          mhError $ GenericError ("Error running command: " <> err)

dispatchCommand :: MM.TeamId -> Text -> MH ()
dispatchCommand tId cmd =
  case unwordHead cmd of
    Just (x, xs)
      | matchingCmds <- [ c
                        | c@(Cmd name _ _ _) <- commandList
                        , name == x
                        ] -> go [] matchingCmds
      where go [] [] = do
              execMMCommand tId x xs
            go errs [] = do
              let msg = ("error running command /" <> x <> ":\n" <>
                         mconcat [ "    " <> e | e <- errs ])
              mhError $ GenericError msg
            go errs (Cmd _ _ spec exe : cs) =
              case matchArgs spec xs of
                Left e -> go (e:errs) cs
                Right args -> exe args
    _ -> return ()

toggleMessagePreview :: MH ()
toggleMessagePreview = do
    mh invalidateCache
    csResources.crConfiguration.configShowMessagePreviewL %= not
