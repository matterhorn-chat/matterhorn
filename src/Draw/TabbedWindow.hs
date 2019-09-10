module Draw.TabbedWindow
  ( drawTabbedWindow
  )
where

import           Prelude ()
import           Prelude.MH

import           Brick

import           Types

drawTabbedWindow :: (Eq a, Show a)
                 => TabbedWindow a
                 -> ChatState
                 -> Widget Name
drawTabbedWindow w cs =
    let matchesVal e = tweValue e == twValue w
    in case filter matchesVal (twtEntries $ twTemplate w) of
        [e] -> tweRender e (twValue w) cs
        _ -> error $ "BUG: tabbed window entry for " <> show (twValue w) <> " should have matched a single entry"
