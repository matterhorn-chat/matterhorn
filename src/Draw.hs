{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE PackageImports #-}
module Draw (draw) where

import Brick
import Lens.Micro.Platform ((^.))

import Types
import Draw.Main
import Draw.ShowHelp

draw :: ChatState -> [Widget Name]
draw st =
    case st^.csMode of
        Main     -> drawMain st
        ShowHelp -> drawShowHelp st
