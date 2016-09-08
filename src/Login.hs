module Login
  ( interactiveGatherCredentials
  ) where

import Brick
import Brick.Widgets.Edit
import Brick.Widgets.Center
import Brick.Widgets.Border
import Control.Monad.IO.Class (liftIO)
import qualified Data.Text as T
import Graphics.Vty hiding (Config)
import System.Exit (exitSuccess)

import Draw.Util
import Config

data Name = Username | Password deriving (Ord, Eq, Show)

data State =
    State { usernameEdit :: Editor Name
          , passwordEdit :: Editor Name
          , focus    :: Name
          }

toPassword :: [String] -> Widget a
toPassword s = str $ replicate (length $ concat s) '*'

interactiveGatherCredentials :: Config -> IO (T.Text, T.Text)
interactiveGatherCredentials config = do
    let state = State { usernameEdit = editor Username (str . concat) (Just 1) uStr
                      , passwordEdit = editor Password toPassword     (Just 1) pStr
                      , focus = initialFocus
                      }
        uStr = case configUser config of
            Nothing -> ""
            Just s  -> T.unpack s
        pStr = case configPass config of
            Just (PasswordString s) -> T.unpack s
            _                       -> ""
        initialFocus = if null uStr then Username else Password
    finalSt <- defaultMain app state
    let finalU = concat $ getEditContents $ usernameEdit finalSt
        finalP = concat $ getEditContents $ passwordEdit finalSt
    return (T.pack finalU, T.pack finalP)

app :: App State Event Name
app = App
  { appDraw         = credsDraw
  , appChooseCursor = showFirstCursor
  , appHandleEvent  = onEvent
  , appStartEvent   = return
  , appAttrMap      = const colorTheme
  , appLiftVtyEvent = id
  }

colorTheme :: AttrMap
colorTheme = attrMap defAttr
  [ (editAttr, black `on` white)
  , (editFocusedAttr, black `on` yellow)
  ]

credsDraw :: State -> [Widget Name]
credsDraw st =
    [ credentialsForm st
    ]

credentialsForm :: State -> Widget Name
credentialsForm st =
    center $ hLimit 50 $ vLimit 15 $
    border $
    vBox [ wrappedText txt "Please enter your MatterMost credentials to log in."
         , str " "
         , str "Username:" <+> renderEditor (focus st == Username) (usernameEdit st)
         , str " "
         , str "Password:" <+> renderEditor (focus st == Password) (passwordEdit st)
         , str " "
         , wrappedText txt "Press Enter to log in or Esc to exit."
         ]

onEvent :: State -> Event -> EventM Name (Next State)
onEvent _  (EvKey KEsc []) = liftIO exitSuccess
onEvent st (EvKey (KChar '\t') []) =
    continue $ st { focus = if focus st == Username
                            then Password
                            else Username
                  }
onEvent st (EvKey KEnter []) =
    -- check for valid (non-empty) contents
    let u = concat $ getEditContents $ usernameEdit st
        p = concat $ getEditContents $ passwordEdit st
    in case null u || null p of
        True -> continue st
        False -> halt st
onEvent st e =
    case focus st of
        Username -> do
            e' <- handleEditorEvent e (usernameEdit st)
            continue $ st { usernameEdit = e' }
        Password -> do
            e' <- handleEditorEvent e (passwordEdit st)
            continue $ st { passwordEdit = e' }
