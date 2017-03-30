{-# LANGUAGE GADTs #-}
module Command where

import Prelude ()
import Prelude.Compat

import Control.Monad (when)
import Control.Monad.IO.Class (liftIO)
import Data.Monoid ((<>))
import qualified Data.Text as T
import System.Exit (ExitCode(..))
import System.Process (readProcessWithExitCode)

import FilePaths (Script(..), getAllScripts, locateScriptPath)
import Lens.Micro.Platform (use)

import State
import State.Common
import State.Editing
import Types

printArgSpec :: CmdArgs a -> T.Text
printArgSpec NoArg = ""
printArgSpec (LineArg ts) = "[" <> ts <> "]"
printArgSpec (TokenArg t NoArg) = "[" <> t <> "]"
printArgSpec (TokenArg t rs) = "[" <> t <> "] " <> printArgSpec rs

matchArgs :: CmdArgs a -> [T.Text] -> Either T.Text a
matchArgs NoArg []  = return ()
matchArgs NoArg [t] = Left ("unexpected argument '" <> t <> "'")
matchArgs NoArg ts  = Left ("unexpected arguments '" <> T.unwords ts <> "'")
matchArgs (LineArg _) ts = return (T.unwords ts)
matchArgs rs@(TokenArg _ NoArg) [] = Left ("missing argument: " <> printArgSpec rs)
matchArgs rs@(TokenArg _ _) [] = Left ("missing arguments: " <> printArgSpec rs)
matchArgs (TokenArg _ rs) (t:ts) = (,) <$> pure t <*> matchArgs rs ts

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
      fetchCurrentChannelMembers
  , Cmd "leave" "Leave the current channel" NoArg $ \ () ->
      startLeaveCurrentChannel
  , Cmd "join" "Join a channel" NoArg $ \ () ->
      startJoinChannel
  , Cmd "theme" "List the available themes" NoArg $ \ () ->
      listThemes
  , Cmd "theme" "Set the color theme"
    (TokenArg "theme" NoArg) $ \ (themeName, ()) ->
      setTheme themeName
  , Cmd "topic" "Set the current channel's topic"
    (LineArg "topic") $ \ p -> do
      when (not $ T.null p) $ do
          st <- use id
          liftIO $ setChannelTopic st p
  , Cmd "add-user" "Add a user to the current channel"
    (TokenArg "username" NoArg) $ \ (uname, ()) ->
        addUserToCurrentChannel uname
  , Cmd "focus" "Focus on a named channel"
    (TokenArg "channel" NoArg) $ \ (name, ()) ->
        changeChannel name
  , Cmd "help" "Show this help screen" NoArg $ \ _ ->
        showHelpScreen MainHelp
  , Cmd "help" "Show help about a particular topic"
      (TokenArg "topic" NoArg) $ \ (topic, ()) ->
        case topic of
          "main"    -> showHelpScreen MainHelp
          "scripts" -> showHelpScreen ScriptHelp
          _         -> do
            let msg = ("Unknown help topic: `" <> topic <> "`. " <>
                      "Available topics are:\n  - main\n  - scripts\n")
            postErrorMessage msg
  , Cmd "sh" "List the available shell scripts" NoArg $ \ () ->
      listScripts
  , Cmd "sh" "Run a prewritten shell script"
    (TokenArg "script" (LineArg "message")) $ \ (script, text) -> do
      fpMb <- liftIO $ locateScriptPath (T.unpack script)
      case fpMb of
        ScriptPath scriptPath -> do
          doAsyncWith Preempt $ runScript scriptPath text
        NonexecScriptPath scriptPath -> do
          let msg = ("The script `" <> T.pack scriptPath <> "` cannot be " <>
               "executed. Try running\n" <>
               "```\n" <>
               "$ chmod u+x " <> T.pack scriptPath <> "\n" <>
               "```\n" <>
               "to correct this error. " <> scriptHelpAddendum)
          postErrorMessage msg
        ScriptNotFound -> do
          let msg = ("No script named " <> script <> " was found")
          postErrorMessage msg
  , Cmd "me" "Send an emote message"
    (LineArg "message") $
    \msg -> execMMCommand "me" msg

  , Cmd "shrug" "Send a message followed by a shrug emoticon"
    (LineArg "message") $
    \msg -> execMMCommand "shrug" msg
  ]

scriptHelpAddendum :: T.Text
scriptHelpAddendum =
  "For more help with scripts, run the command\n" <>
  "```\n/help scripts\n```\n"

runScript :: FilePath -> T.Text -> IO (MH ())
runScript fp text = do
  (code, stdout, stderr) <- readProcessWithExitCode fp [] (T.unpack text)
  case code of
    ExitSuccess -> return $ do
      mode <- use (csEditState.cedEditMode)
      sendMessage mode (T.pack stdout)
    ExitFailure _ -> return $ do
      let msgText = "The script `" <> T.pack fp <> "` exited with a " <>
                    "non-zero exit code."
          msgText' = if stderr == ""
                       then msgText
                       else msgText <> " It also produced the " <>
                            "following output on stderr:\n~~~~~\n" <>
                            T.pack stderr <> "~~~~~\n" <> scriptHelpAddendum
      postErrorMessage msgText'

listScripts :: MH ()
listScripts = do
  (execs, nonexecs) <- liftIO getAllScripts
  let scripts = ("Available scripts are:\n" <>
                 mconcat [ "  - " <> T.pack cmd <> "\n"
                         | cmd <- execs
                         ])
  postInfoMessage scripts
  case nonexecs of
    [] -> return ()
    _  -> do
      let errMsg = ("Some non-executable script files are also " <>
                    "present. If you want to run these as scripts " <>
                    "in Matterhorn, mark them executable with \n" <>
                    "```\n" <>
                    "$ chmod u+x [script path]\n" <>
                    "```\n" <>
                    "\n" <>
                    mconcat [ "  - " <> T.pack cmd <> "\n"
                            | cmd <- nonexecs
                            ] <> "\n" <> scriptHelpAddendum)
      postErrorMessage errMsg

dispatchCommand :: T.Text -> MH ()
dispatchCommand cmd =
  case T.words cmd of
    (x:xs) | matchingCmds <- [ c | c@(Cmd name _ _ _) <- commandList
                             , name == x
                             ] -> go [] matchingCmds
      where go [] [] = do
              let msg = ("error running command /" <> x <> ":\n" <>
                         "no such command")
              postErrorMessage msg
            go errs [] = do
              let msg = ("error running command /" <> x <> ":\n" <>
                         mconcat [ "    " <> e | e <- errs ])
              postErrorMessage msg
            go errs (Cmd _ _ spec exe : cs) =
              case matchArgs spec xs of
                Left e -> go (e:errs) cs
                Right args -> exe args
    _ -> return ()
