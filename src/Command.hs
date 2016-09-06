module Command where

import Brick (EventM, Next, continue, halt)

import Lens.Micro.Platform

import State
import Types

data Cmd = Cmd
  { commandName   :: String
  , commandDescr  :: String
  , commandAction :: [String] -> ChatState -> EventM Name (Next ChatState)
  }

commandList :: [Cmd]
commandList =
  [ Cmd "quit" "Exit Matterhorn" $ \ _ st -> halt st
  , Cmd "right" "Focus on the next channel" $ \ _ st ->
      nextChannel st >>= continue
  , Cmd "left" "Focus on the previous channel" $ \ _ st ->
      prevChannel st >>= continue
  , Cmd "theme" "Set the color theme" $ \ args st ->
      case args of
          []          -> listThemes st >>= continue
          [themeName] -> setTheme st themeName >>= continue
          _           -> continue st
  , Cmd "focus" "Focus on a named channel" $ \ [name] st ->
      case channelByName st name of
        Just cId -> setFocus cId st >>= continue
        Nothing -> do
          msg <- newClientMessage Error ("No channel or user named " ++ name)
          continue =<< addClientMessage msg st
  , Cmd "help" "Print the help dialogue" $ \ _ st -> do
          continue $ st & csMode .~ ShowHelp
  ]

dispatchCommand :: String -> ChatState -> EventM Name (Next ChatState)
dispatchCommand cmd st =
  case words cmd of
    (x:xs) | [ c ] <- [ c | c <- commandList
                          , commandName c == x
                          ] -> commandAction c xs st
           | otherwise ->
             execMMCommand cmd st >>= continue
    _ -> continue st
