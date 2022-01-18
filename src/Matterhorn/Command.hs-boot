module Matterhorn.Command where

import Data.Text ( Text )

import Network.Mattermost.Types ( TeamId )
import Matterhorn.Types ( MH, Cmd, CmdArgs )

commandList :: [Cmd]
printArgSpec :: CmdArgs a -> Text
dispatchCommand :: TeamId -> Text -> MH ()
