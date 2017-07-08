{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
module Command where

import Prelude ()
import Prelude.Compat

import Control.Monad (when)
import Data.Monoid ((<>))
import qualified Data.Text as T

import State
import State.Common
import State.Editing
import State.PostListOverlay
import Types
import HelpTopics
import Scripts

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
      when (not $ T.null p) $ setChannelTopic p
  , Cmd "add-user" "Add a user to the current channel"
    (TokenArg "username" NoArg) $ \ (uname, ()) ->
        addUserToCurrentChannel uname
  , Cmd "focus" "Focus on a named channel"
    (TokenArg "channel" NoArg) $ \ (name, ()) ->
        changeChannel name
  , Cmd "help" "Show this help screen" NoArg $ \ _ ->
        showHelpScreen mainHelpTopic
  , Cmd "help" "Show help about a particular topic"
      (TokenArg "topic" NoArg) $ \ (topicName, ()) ->
          case lookupHelpTopic topicName of
              Nothing -> do
                  let msg = ("Unknown help topic: `" <> topicName <> "`. " <>
                            (T.unlines $ "Available topics are:" : knownTopics))
                      knownTopics = ("  - " <>) <$> helpTopicName <$> helpTopics
                  postErrorMessage msg
              Just topic -> showHelpScreen topic
  , Cmd "sh" "List the available shell scripts" NoArg $ \ () ->
      listScripts
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
  ]

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
