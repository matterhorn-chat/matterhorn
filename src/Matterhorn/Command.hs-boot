module Matterhorn.Command
  ( commandList
  , printArgSpec
  , dispatchCommand
  )
where

import Prelude ()
import Matterhorn.Prelude

import Network.Mattermost.Types ( TeamId )
import Matterhorn.Types ( MH, ClientCommand, CommandArgs )

commandList :: [ClientCommand]
printArgSpec :: CommandArgs a -> Text
dispatchCommand :: TeamId -> Text -> MH ()
