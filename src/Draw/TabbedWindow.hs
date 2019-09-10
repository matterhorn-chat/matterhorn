module Draw.TabbedWindow
  ( drawTabbedWindow
  )
where

import           Prelude ()
import           Prelude.MH

import           Brick
import           Brick.Widgets.Border
import           Brick.Widgets.Center

import           Types
import           Themes

tabbedWindowWidth :: Int
tabbedWindowWidth = 60

tabbedWindowHeight :: Int
tabbedWindowHeight = 20

drawTabbedWindow :: (Eq a, Show a)
                 => TabbedWindow a
                 -> ChatState
                 -> Widget Name
drawTabbedWindow w cs =
    let cur = getCurrentEntry w
        body = tabBody <=> fill ' '
        tabBody = tweRender cur (twValue w) cs
        title = twtTitle (twTemplate w) (tweValue cur)
    in centerLayer $
       vLimit tabbedWindowHeight $
       hLimit tabbedWindowWidth $
       borderWithLabel title $
       (tabBar w <=> hBorder <=> body)

tabBar :: (Eq a, Show a)
       => TabbedWindow a
       -> Widget Name
tabBar w =
    let cur = getCurrentEntry w
        entries = twtEntries (twTemplate w)
        renderEntry e =
            let maybeForce = if isCurrent
                             then forceAttr tabSelectedAttr
                             else id
                isCurrent = tweValue e == tweValue cur
            in maybeForce $
               padRight (Pad 3) $
               tweTitle e (tweValue e) isCurrent
        renderings = renderEntry <$> entries
    in hBox renderings

getCurrentEntry :: (Show a, Eq a)
                => TabbedWindow a
                -> TabbedWindowEntry a
getCurrentEntry w =
    let matchesVal e = tweValue e == twValue w
    in case filter matchesVal (twtEntries $ twTemplate w) of
        [e] -> e
        _ -> error $ "BUG: tabbed window entry for " <> show (twValue w) <>
                     " should have matched a single entry"
