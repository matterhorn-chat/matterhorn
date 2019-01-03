module Command where

import Data.Text ( Text )

import Types ( MH, Cmd, CmdArgs )

commandList :: [Cmd]
printArgSpec :: CmdArgs a -> Text
dispatchCommand :: Text -> MH ()
