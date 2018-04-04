module TeamSelect
  ( interactiveTeamSelection
  ) where

import Prelude ()
import Prelude.MH

import Brick
import Brick.Widgets.List
import Brick.Widgets.Center
import Brick.Widgets.Border
import qualified Data.Vector as V
import Graphics.Vty
import System.Exit (exitSuccess)

import Network.Mattermost.Types

import Markdown

type State = List () Team

interactiveTeamSelection :: [Team] -> IO Team
interactiveTeamSelection teams = do
    let state = list () (V.fromList teams) 1
    finalSt <- defaultMain app state
    let Just (_, t) = listSelectedElement finalSt
    return t

app :: App State e ()
app = App
  { appDraw         = teamSelectDraw
  , appChooseCursor = neverShowCursor
  , appHandleEvent  = onEvent
  , appStartEvent   = return
  , appAttrMap      = const colorTheme
  }

colorTheme :: AttrMap
colorTheme = attrMap defAttr
  [ (listSelectedFocusedAttr, black `on` yellow)
  ]

teamSelectDraw :: State -> [Widget ()]
teamSelectDraw st =
    [ teamSelect st
    ]

teamSelect :: State -> Widget ()
teamSelect st =
    center $ hLimit 50 $ vLimit 15 $
    vBox [ hCenter $ txt "Welcome to Mattermost. Please select a team:"
         , txt " "
         , border theList
         , txt " "
         , renderText "Press Enter to select a team and connect or Esc to exit."
         ]
    where
    theList = renderList renderTeamItem True st

renderTeamItem :: Bool -> Team -> Widget ()
renderTeamItem _ t =
    padRight Max $ txt $ teamName t

onEvent :: State -> BrickEvent () e -> EventM () (Next State)
onEvent _  (VtyEvent (EvKey KEsc [])) = liftIO exitSuccess
onEvent st (VtyEvent (EvKey KEnter [])) = halt st
onEvent st (VtyEvent e) = continue =<< handleListEvent e st
onEvent st _ = continue st
