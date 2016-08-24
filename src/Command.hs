module Command where

import Brick (EventM, Next, continue, halt)

import State

data Cmd = Cmd
  { commandName   :: String
  , commandDescr  :: String
  , commandAction :: [String] -> ChatState -> EventM Int (Next ChatState)
  }

commandList :: [Cmd]
commandList =
  [ Cmd "quit" "Exit Matterhorn" $ \ _ st -> halt st
  , Cmd "right" "Focus on the next channel" $ \ _ st ->
      continue (nextChannel st)
  , Cmd "left" "Focus on the previous channel" $ \ _ st ->
      continue (prevChannel st)
  , Cmd "chan" "Focus on a named channel" $ \ [ch] st ->
      continue (setFocus ch st)
  , Cmd "help" "Print the help dialogue [broken]" $ \ _ st ->
      continue st
  ]

dispatchCommand :: String -> ChatState -> EventM Int (Next ChatState)
dispatchCommand cmd st =
  case words cmd of
    (x:xs) | [ c ] <- [ c | c <- commandList
                          , commandName c == x
                          ] -> commandAction c xs st
           | otherwise -> continue st
    _ -> continue st
