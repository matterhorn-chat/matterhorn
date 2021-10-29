module Matterhorn.Draw.TabbedWindow
  ( drawTabbedWindow
  )
where

import           Prelude ()
import           Matterhorn.Prelude

import           Brick
import           Brick.Widgets.Border
import           Brick.Widgets.Center
import           Data.List ( intersperse )
import qualified Graphics.Vty as Vty

import           Network.Mattermost.Types ( TeamId )

import           Matterhorn.Draw.Util ( renderKeybindingHelp )
import           Matterhorn.Types
import           Matterhorn.Themes
import           Matterhorn.Types.KeyEvents

-- | Render a tabbed window.
drawTabbedWindow :: (Eq a, Show a)
                 => TabbedWindow a
                 -> ChatState
                 -> Widget Name
drawTabbedWindow w cs =
    let cur = getCurrentTabbedWindowEntry w
        tabBody = tweRender cur (twValue w) cs
        title = forceAttr clientEmphAttr $ twtTitle (twTemplate w) (tweValue cur)
        tId = cs^.csCurrentTeamId
    in centerLayer $
       vLimit (twWindowHeight w) $
       hLimit (twWindowWidth w) $
       joinBorders $
       borderWithLabel title $
       (tabBar tId w <=> tabBody <=> hBorder <=> hCenter (keybindingHelp cs))

-- | Keybinding help to show at the bottom of a tabbed window.
keybindingHelp :: ChatState -> Widget Name
keybindingHelp st =
    let pairs = [ ("Switch tabs", [SelectNextTabEvent, SelectPreviousTabEvent])
                , ("Scroll", [ScrollUpEvent, ScrollDownEvent, ScrollLeftEvent, ScrollRightEvent, PageLeftEvent, PageRightEvent])
                ]
    in hBox $ intersperse (txt "  ") $ (uncurry (renderKeybindingHelp st)) <$> pairs

-- | The scrollable tab bar to show at the top of a tabbed window.
tabBar :: (Eq a, Show a)
       => TeamId
       -> TabbedWindow a
       -> Widget Name
tabBar tId w =
    let cur = getCurrentTabbedWindowEntry w
        entries = twtEntries (twTemplate w)
        renderEntry e =
            let useAttr = if isCurrent
                          then withDefAttr tabSelectedAttr
                          else withDefAttr tabUnselectedAttr
                isCurrent = tweValue e == tweValue cur
                makeVisible = if isCurrent then visible else id
                decorateTab v = Widget Fixed Fixed $ do
                    result <- render v
                    let width = Vty.imageWidth (result^.imageL)
                    if isCurrent
                       then
                           render $ padBottom (Pad 1) $ resultToWidget result
                       else
                           render $ vBox [ resultToWidget result
                                         , hLimit width hBorder
                                         ]
            in makeVisible $
               decorateTab $
               useAttr $
               padLeftRight 2 $
               txt $
               tweTitle e (tweValue e) isCurrent
        contents = Widget Fixed Fixed $ do
            ctx <- getContext
            let width = ctx^.availWidthL
            render $ hBox $ (intersperse divider $ renderEntry <$> entries) <>
                            [divider, padTop (Pad 1) $ hLimit width hBorder]
        divider = vLimit 1 vBorder <=> joinableBorder (Edges True False False False)
    in vLimit 2 $ viewport (TabbedWindowTabBar tId) Horizontal contents
