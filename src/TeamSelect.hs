module TeamSelect
  ( interactiveTeamSelection
  ) where

import Brick
import Brick.Widgets.List
import Brick.Widgets.Center
import Brick.Widgets.Border
import Control.Monad.IO.Class (liftIO)
import qualified Data.Vector as V
import Graphics.Vty
import System.Exit (exitSuccess)

import Network.Mattermost

import Draw.Util

type State = List () Team

interactiveTeamSelection :: [Team] -> IO Team
interactiveTeamSelection teams = do
    let state = list () (V.fromList teams) 1
    finalSt <- defaultMain app state
    let Just (_, t) = listSelectedElement finalSt
    return t

app :: App State Event ()
app = App
  { appDraw         = teamSelectDraw
  , appChooseCursor = neverShowCursor
  , appHandleEvent  = onEvent
  , appStartEvent   = return
  , appAttrMap      = const colorTheme
  , appLiftVtyEvent = id
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
    vBox [ hCenter $ txt "Welcome to MatterMost. Please select a team:"
         , txt " "
         , border theList
         , txt " "
         , wrappedText txt "Press Enter to select a team and connect or Esc to exit."
         ]
    where
    theList = renderList renderTeamItem True st

renderTeamItem :: Bool -> Team -> Widget ()
renderTeamItem _ t =
    padRight Max $ txt $ teamName t

onEvent :: State -> Event -> EventM () (Next State)
onEvent _  (EvKey KEsc []) = liftIO exitSuccess
onEvent st (EvKey KEnter []) = halt st
onEvent st e = continue =<< handleListEvent e st
