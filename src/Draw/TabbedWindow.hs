module Draw.TabbedWindow
  ( drawTabbedWindow
  )
where

import           Prelude ()
import           Prelude.MH

import           Brick
import           Brick.Widgets.Border
import           Brick.Widgets.Center
import           Data.List ( intersperse )
import qualified Graphics.Vty as Vty

import           Types
import           Themes
import           Types.KeyEvents
import           Events.Keybindings ( getFirstDefaultBinding )

-- | Render a tabbed window.
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
       (tabBar w <=> tabBody <=> hBorder <=> hCenter keybindingHelp)

-- | Keybinding help to show at the bottom of a tabbed window.
keybindingHelp :: Widget Name
keybindingHelp =
    let ppPair (label, evs) = hBox $ (intersperse (txt "/") $ ppEv <$> evs) <> [txt (":" <> label)]
        ppEv ev = withDefAttr clientEmphAttr $ txt (ppBinding (getFirstDefaultBinding ev))
        pairs = [ ("Switch tabs", [SelectNextTabEvent, SelectPreviousTabEvent])
                , ("Scroll", [ScrollUpEvent, ScrollDownEvent, ScrollLeftEvent, ScrollRightEvent])
                , ("Close", [CancelEvent])
                ]
    in hBox $ intersperse (txt "  ") $ ppPair <$> pairs

-- | The scrollable tab bar to show at the top of a tabbed window.
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
                makeVisible = if isCurrent then visible else id
                decorateTab v = Widget Fixed Fixed $ do
                    result <- render v
                    let width = Vty.imageWidth (result^.imageL)
                    if isCurrent
                       then
                           render $ padBottom (Pad 1) $ raw $ result^.imageL
                       else
                           render $ vBox [raw $ result^.imageL, hLimit width hBorder]
            in makeVisible $
               decorateTab $
               useAttr $
               padLeftRight 2 $
               tweTitle e (tweValue e) isCurrent
        contents = Widget Fixed Fixed $ do
            ctx <- getContext
            let width = ctx^.availWidthL
            render $ hBox $ (intersperse divider $ renderEntry <$> entries) <>
                            [divider, padTop (Pad 1) $ hLimit width hBorder]
        divider = vLimit 1 vBorder <=> joinableBorder (Edges True False False False)
    in viewport TabbedWindowTabBar Horizontal contents
