{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
module Command
  ( commandList
  , dispatchCommand
  , printArgSpec
  )
where

import           Prelude ()
import           Prelude.MH

import qualified Control.Exception as Exn
import qualified Data.Char as Char
import qualified Data.Text as T

import qualified Network.Mattermost.Endpoints as MM
import qualified Network.Mattermost.Exceptions as MM
import qualified Network.Mattermost.Types as MM

import           HelpTopics
import           Scripts
import           State.Help
import           State.Channels
import           State.ChannelSelect
import           State.Editing ( toggleMessagePreview )
import           State.Logging
import           State.PostListOverlay
import           State.Themes
import           State.UserListOverlay
import           Types


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
printArgSpec (LineArg ts) = "[" <> ts <> "]"
printArgSpec (TokenArg t NoArg) = "[" <> t <> "]"
printArgSpec (TokenArg t rs) = "[" <> t <> "] " <> printArgSpec rs

matchArgs :: CmdArgs a -> Text -> Either Text a
matchArgs NoArg t = case unwordHead t of
  Nothing -> return ()
  Just (a, as)
    | not (T.all Char.isSpace as) -> Left ("Unexpected arguments '" <> t <> "'")
    | otherwise -> Left ("Unexpected argument '" <> a <> "'")
matchArgs (LineArg _) t = return t
matchArgs spec@(TokenArg _ rs) t = case unwordHead t of
  Nothing -> case rs of
    NoArg -> Left ("Missing argument: " <> printArgSpec spec)
    _     -> Left ("Missing arguments: " <> printArgSpec spec)
  Just (a, as) -> (,) <$> pure a <*> matchArgs rs as

commandList :: [Cmd]
commandList =
  [ Cmd "quit" "Exit Matterhorn" NoArg $ \ () -> requestQuit

  , Cmd "right" "Focus on the next channel" NoArg $ \ () ->
      nextChannel

  , Cmd "left" "Focus on the previous channel" NoArg $ \ () ->
      prevChannel

  , Cmd "create-channel" "Create a new channel"
    (LineArg "channel name") $ \ name ->
      createOrdinaryChannel name

  , Cmd "delete-channel" "Delete the current channel"
    NoArg $ \ () ->
      beginCurrentChannelDeleteConfirm

  , Cmd "members" "Show the current channel's members"
    NoArg $ \ () ->
      enterChannelMembersUserList

  , Cmd "leave" "Leave the current channel" NoArg $ \ () ->
      startLeaveCurrentChannel

  , Cmd "join" "Browse the list of available channels" NoArg $ \ () ->
      startJoinChannel

  , Cmd "join" "Join the specified channel" (TokenArg "channel" NoArg) $ \(n, ()) ->
      joinChannelByName n

  , Cmd "theme" "List the available themes" NoArg $ \ () ->
      listThemes

  , Cmd "theme" "Set the color theme"
    (TokenArg "theme" NoArg) $ \ (themeName, ()) ->
      setTheme themeName

  , Cmd "topic" "Set the current channel's topic"
    (LineArg "topic") $ \ p ->
      if not (T.null p) then setChannelTopic p else return ()

  , Cmd "add-user" "Search for a user to add to the current channel"
    NoArg $ \ () ->
        enterChannelInviteUserList

  , Cmd "msg" "Chat with a user privately"
    NoArg $ \ () ->
        enterDMSearchUserList

  , Cmd "log-start" "Begin logging to the specified path"
    (TokenArg "path" NoArg) $ \ (path, ()) ->
        startLogging $ T.unpack path

  , Cmd "log-snapshot" "Dump the current log buffer to the specified path"
    (TokenArg "path" NoArg) $ \ (path, ()) ->
        logSnapshot $ T.unpack path

  , Cmd "log-stop" "Stop logging"
    NoArg $ \ () ->
        stopLogging

  , Cmd "log-status" "Show current logging status"
    NoArg $ \ () ->
        getLogDestination

  , Cmd "add-user" "Add a user to the current channel"
    (TokenArg "username" NoArg) $ \ (uname, ()) ->
        addUserToCurrentChannel uname

  , Cmd "remove-user" "Remove a user from the current channel"
    (TokenArg "username" NoArg) $ \ (uname, ()) ->
        removeUserFromCurrentChannel uname

  , Cmd "message-preview" "Toggle preview of the current message" NoArg $ \_ ->
        toggleMessagePreview

  , Cmd "focus" "Focus on a channel or user"
    (TokenArg "channel" NoArg) $ \ (name, ()) ->
        changeChannel name

  , Cmd "focus" "Select from available channels" NoArg $ \ () ->
        beginChannelSelect

  , Cmd "help" "Show this help screen" NoArg $ \ _ ->
        showHelpScreen mainHelpTopic

  , Cmd "help" "Show help about a particular topic"
      (TokenArg "topic" NoArg) $ \ (topicName, ()) ->
          case lookupHelpTopic topicName of
              Nothing -> mhError $ NoSuchHelpTopic topicName
              Just topic -> showHelpScreen topic

  , Cmd "sh" "List the available shell scripts" NoArg $ \ () ->
      listScripts

  , Cmd "group-msg" "Create a group chat"
    (LineArg "user1 user2 ...") createGroupChannel

  , Cmd "sh" "Run a prewritten shell script"
    (TokenArg "script" (LineArg "message")) $ \ (script, text) ->
      findAndRunScript script text

  , Cmd "me" "Send an emote message"
    (LineArg "message") $
    \msg -> execMMCommand "me" msg

  , Cmd "shrug" "Send a message followed by a shrug emoticon"
    (LineArg "message") $
    \msg -> execMMCommand "shrug" msg

  , Cmd "flags" "Open up a pane of flagged posts"  NoArg $ \ () ->
      enterFlaggedPostListMode

  , Cmd "search" "Search for posts with given terms" (LineArg "terms") $
      enterSearchResultPostListMode

  ]

execMMCommand :: Text -> Text -> MH ()
execMMCommand name rest = do
  cId      <- use csCurrentChannelId
  session  <- getSession
  em       <- use (csEditState.cedEditMode)
  tId      <- gets myTeamId
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

dispatchCommand :: Text -> MH ()
dispatchCommand cmd =
  case unwordHead cmd of
    Just (x, xs)
      | matchingCmds <- [ c
                        | c@(Cmd name _ _ _) <- commandList
                        , name == x
                        ] -> go [] matchingCmds
      where go [] [] = do
              execMMCommand x xs
            go errs [] = do
              let msg = ("error running command /" <> x <> ":\n" <>
                         mconcat [ "    " <> e | e <- errs ])
              mhError $ GenericError msg
            go errs (Cmd _ _ spec exe : cs) =
              case matchArgs spec xs of
                Left e -> go (e:errs) cs
                Right args -> exe args
    _ -> return ()
