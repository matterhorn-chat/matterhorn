module Matterhorn.Command
  ( commandList
  , printArgSpec
  , dispatchCommand
  )
where

import Prelude ()
import Matterhorn.Prelude

import Network.Mattermost.Types ( TeamId )
import Matterhorn.Types ( MH, Cmd, CmdArgs )

commandList :: [Cmd]
printArgSpec :: CmdArgs a -> Text
dispatchCommand :: TeamId -> Text -> MH ()
