{-# LANGUAGE GADTs #-}
module Command where

import Brick (EventM, Next, continue, halt)
import Control.Applicative
import Control.Monad (when)
import Control.Monad.IO.Class (liftIO)
import Data.Monoid ((<>))
import qualified Data.Text as T
import System.Process (readProcess)

import Prelude

import FilePaths (getAllScripts, locateScriptPath)
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
  [ Cmd "quit" "Exit Matterhorn" NoArg $ \ () st -> halt st
  , Cmd "right" "Focus on the next channel" NoArg $ \ () st ->
      nextChannel st >>= continue
  , Cmd "left" "Focus on the previous channel" NoArg $ \ () st ->
      prevChannel st >>= continue
  , Cmd "create-channel" "Create a new channel"
    (LineArg "channel name") $ \ name st ->
      createOrdinaryChannel name st >>= continue
  , Cmd "leave" "Leave the current channel" NoArg $ \ () st ->
      startLeaveCurrentChannel st >>= continue
  , Cmd "join" "Join a channel" NoArg $ \ () st ->
      startJoinChannel st >>= continue
  , Cmd "theme" "List the available themes" NoArg $ \ () st ->
      listThemes st >>= continue
  , Cmd "theme" "Set the color theme"
    (TokenArg "theme" NoArg) $ \ (themeName, ()) st ->
      setTheme st themeName >>= continue
  , Cmd "topic" "Set the current channel's topic"
    (LineArg "topic") $ \ p st -> do
      when (not $ T.null p) $ do
          liftIO $ setChannelTopic st p
      continue st
  , Cmd "add-user" "Add a user to the current channel"
    (TokenArg "username" NoArg) $ \ (uname, ()) st ->
        addUserToCurrentChannel uname st >>= continue
  , Cmd "focus" "Focus on a named channel"
    (TokenArg "channel" NoArg) $ \ (name, ()) st ->
        changeChannel name st >>= continue
  , Cmd "help" "Show this help screen" NoArg $ \ _ st ->
        showHelpScreen st >>= continue
  , Cmd "sh" "List the available shell scripts" NoArg $ \ () st -> do
      cmds <- liftIO getAllScripts
      msg <- newClientMessage Informative
               ("Available scripts are:\n" <>
               mconcat [ "  - " <> T.pack cmd <> "\n"
                       | cmd <- cmds
                       ])
      addClientMessage msg st >>= continue
  , Cmd "sh" "Run a prewritten shell script"
    (TokenArg "script" (LineArg "message")) $ \ (script, text) st -> do
      fpMb <- liftIO $ locateScriptPath (T.unpack script)
      case fpMb of
        Just scriptPath -> do
          liftIO $ doAsyncWith st $ do
            rs <- readProcess scriptPath [] (T.unpack text)
            return $ \st' -> do
              liftIO $ sendMessage st' (T.pack rs)
              return st'
          continue st
        Nothing -> do
          cmds <- liftIO getAllScripts
          msg <- newClientMessage Error
              ("No script found named `" <> script <> "`! " <>
               "Currently available scripts include:\n" <>
               mconcat [ "  - " <> T.pack cmd <> "\n"
                       | cmd <- cmds
                       ])
          addClientMessage msg st >>= continue
  ]

dispatchCommand :: T.Text -> ChatState -> EventM Name (Next ChatState)
dispatchCommand cmd st =
  case T.words cmd of
    (x:xs) | matchingCmds <- [ c | c@(Cmd name _ _ _) <- commandList
                                 , name == x
                                 ] ->
             case matchingCmds of
               [] -> execMMCommand cmd st >>= continue
               cs -> go [] cs
      where go errs [] = do
              msg <- newClientMessage Error
                     ("error running command /" <> x <> ":\n" <>
                      mconcat [ "    " <> e | e <- errs ])
              addClientMessage msg st >>= continue
            go errs (Cmd _ _ spec exe : cs) =
              case matchArgs spec xs of
                Left e -> go (e:errs) cs
                Right args -> exe args st
    _ -> continue st
