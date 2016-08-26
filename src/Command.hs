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
      nextChannel st >>= continue
  , Cmd "left" "Focus on the previous channel" $ \ _ st ->
      prevChannel st >>= continue
  , Cmd "chan" "Focus on a named channel" $ \ [ch] st ->
      if channelExists st ch
        then setFocus ch st >>= continue
        else do
          msg <- newClientMessage ("No channel named #" ++ ch)
          continue (addClientMessage msg st)
  , Cmd "dm" "Focus on a direct message channel" $ \ [dm] st ->
      if userExists st dm
        then setDMFocus dm st >>= continue
        else do
          msg <- newClientMessage ("No user named @" ++ dm)
          continue (addClientMessage msg st)
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
           | otherwise -> do
               let s = unlines [ "\nUnknown command: `/" ++ x ++ "`"
                               , "Type /help for more information"
                               ]
               msg <- newClientMessage s
               continue (addClientMessage msg st)
    _ -> continue st
