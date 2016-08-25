module Command where

import Brick (EventM, Next, continue, halt)

import State

data Cmd = Cmd
  { commandName   :: String
  , commandDescr  :: String
  , commandAction :: [String] -> ChatState -> EventM Name (Next ChatState)
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
  , Cmd "dm" "Focus on a direct message channel" $ \ [dm] st ->
      continue (setDMFocus dm st)
  , Cmd "help" "Print the help dialogue" $ \ _ st -> do
        msg <- newClientMessage (mkHelpText commandList)
        continue (addClientMessage msg st)
  ]

mkHelpText :: [Cmd] -> String
mkHelpText cs = "\n" ++
  unlines [ "  /" ++ cmd ++ ": " ++ desc
          | Cmd { commandName = cmd, commandDescr = desc } <- cs
          ]

dispatchCommand :: String -> ChatState -> EventM Name (Next ChatState)
dispatchCommand cmd st =
  case words cmd of
    (x:xs) | [ c ] <- [ c | c <- commandList
                          , commandName c == x
                          ] -> commandAction c xs st
           | otherwise -> continue st
    _ -> continue st
