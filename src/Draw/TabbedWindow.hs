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

drawTabbedWindow :: (Eq a, Show a)
                 => TabbedWindow a
                 -> ChatState
                 -> Widget Name
drawTabbedWindow w cs =
    let cur = getCurrentTabbedWindowEntry w
        tabBody = tweRender cur (twValue w) cs
        title = forceAttr clientEmphAttr $ twtTitle (twTemplate w) (tweValue cur)
    in centerLayer $
       vLimit (twWindowHeight w) $
       hLimit (twWindowWidth w) $
       joinBorders $
       borderWithLabel title $
       (tabBar w <=> hBorder <=> tabBody)

tabBar :: (Eq a, Show a)
       => TabbedWindow a
       -> Widget Name
tabBar w =
    let cur = getCurrentTabbedWindowEntry w
        entries = twtEntries (twTemplate w)
        renderEntry e =
            let useAttr = if isCurrent
                          then withDefAttr tabSelectedAttr
                          else withDefAttr tabUnselectedAttr
                isCurrent = tweValue e == tweValue cur
            in useAttr $
               padLeftRight 2 $
               tweTitle e (tweValue e) isCurrent
        renderings = renderEntry <$> entries
    in hBox renderings
