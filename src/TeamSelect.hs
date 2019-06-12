module TeamSelect
  ( interactiveTeamSelection
  )
where

import           Prelude ()
import           Prelude.MH

import           Brick
import           Brick.Widgets.Border
import           Brick.Widgets.Center
import           Brick.Widgets.List
import qualified Data.Function as F
import           Data.List ( sortBy )
import qualified Data.Vector as V
import           Graphics.Vty hiding (mkVty)
import qualified Data.Text as T

import           Network.Mattermost.Types

import           Markdown
import           Types.Common


data State =
    State { appList :: List () Team
          , appCancelled :: Bool
          }

interactiveTeamSelection :: Vty -> IO Vty -> [Team] -> IO (Maybe Team, Vty)
interactiveTeamSelection vty mkVty teams = do
    let state = State { appList = list () (V.fromList sortedTeams) 1
                      , appCancelled = False
                      }
        sortedTeams = sortBy (compare `F.on` teamName) teams

    (finalSt, finalVty) <- customMainWithVty vty mkVty Nothing app state

    let result = if appCancelled finalSt
                 then Nothing
                 else snd <$> listSelectedElement (appList finalSt)

    return (result, finalVty)

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
    theList = renderList renderTeamItem True (appList st)

renderTeamItem :: Bool -> Team -> Widget ()
renderTeamItem _ t =
    padRight Max $ txt $ (sanitizeUserText $ teamName t) <>
        if not $ T.null (sanitizeUserText $ teamDisplayName t)
        then " (" <> (sanitizeUserText $ teamDisplayName t) <> ")"
        else ""

onEvent :: State -> BrickEvent () e -> EventM () (Next State)
onEvent st (VtyEvent (EvKey KEsc [])) = do
    halt $ st { appCancelled = True }
onEvent st (VtyEvent (EvKey KEnter [])) = halt st
onEvent st (VtyEvent e) = do
    list' <- handleListEvent e (appList st)
    continue $ st { appList = list' }
onEvent st _ = continue st
