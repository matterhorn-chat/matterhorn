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

import Network.Mattermost.Exceptions (LoginFailureException(..))

import Config
import Markdown
import Types (AuthenticationException(..))

data Name = Username | Password deriving (Ord, Eq, Show)

data State =
    State { usernameEdit :: Editor T.Text Name
          , passwordEdit :: Editor T.Text Name
          , focus    :: Name
          , previousError :: Maybe AuthenticationException
          }

toPassword :: [T.Text] -> Widget a
toPassword s = txt $ T.replicate (T.length $ T.concat s) "*"

interactiveGatherCredentials :: Config
                             -> Maybe AuthenticationException
                             -> IO (T.Text, T.Text)
interactiveGatherCredentials config authError = do
    let state = State { usernameEdit = editor Username (txt . T.concat) (Just 1) uStr
                      , passwordEdit = editor Password toPassword     (Just 1) pStr
                      , focus = initialFocus
                      , previousError = authError
                      }
        uStr = case configUser config of
            Nothing -> ""
            Just s  -> s
        pStr = case configPass config of
            Just (PasswordString s) -> s
            _                       -> ""
        initialFocus = if T.null uStr then Username else Password
    finalSt <- defaultMain app state
    let finalU = T.concat $ getEditContents $ usernameEdit finalSt
        finalP = T.concat $ getEditContents $ passwordEdit finalSt
    return (finalU, finalP)

app :: App State e Name
app = App
  { appDraw         = credsDraw
  , appChooseCursor = showFirstCursor
  , appHandleEvent  = onEvent
  , appStartEvent   = return
  , appAttrMap      = const colorTheme
  }

errorAttr :: AttrName
errorAttr = "errorMessage"

colorTheme :: AttrMap
colorTheme = attrMap defAttr
  [ (editAttr, black `on` white)
  , (editFocusedAttr, black `on` yellow)
  , (errorAttr, fg red)
  ]

credsDraw :: State -> [Widget Name]
credsDraw st =
    [ center (credentialsForm st <=> errorMessageDisplay st)
    ]

errorMessageDisplay :: State -> Widget Name
errorMessageDisplay st = do
    case previousError st of
        Nothing -> emptyWidget
        -- XXX
        Just e -> txt " " <=>
                  (withDefAttr errorAttr $
                   hCenter (str "Error: " <+> renderAuthError e))

renderAuthError :: AuthenticationException -> Widget Name
renderAuthError (ConnectError _) = txt "Could not connect to server"
renderAuthError (ResolveError _) = txt "Could not resolve server hostname"
renderAuthError (OtherAuthError e) = str $ show e
renderAuthError (LoginError (LoginFailureException msg)) = str msg

credentialsForm :: State -> Widget Name
credentialsForm st =
    hCenter $ hLimit 50 $ vLimit 15 $
    border $
    vBox [ renderText "Please enter your MatterMost credentials to log in."
         , txt " "
         , txt "Username:" <+> renderEditor (focus st == Username) (usernameEdit st)
         , txt " "
         , txt "Password:" <+> renderEditor (focus st == Password) (passwordEdit st)
         , txt " "
         , renderText "Press Enter to log in or Esc to exit."
         ]

onEvent :: State -> BrickEvent Name e -> EventM Name (Next State)
onEvent _  (VtyEvent (EvKey KEsc [])) = liftIO exitSuccess
onEvent st (VtyEvent (EvKey (KChar '\t') [])) =
    continue $ st { focus = if focus st == Username
                            then Password
                            else Username
                  }
onEvent st (VtyEvent (EvKey KEnter [])) =
    -- check for valid (non-empty) contents
    let u = T.concat $ getEditContents $ usernameEdit st
        p = T.concat $ getEditContents $ passwordEdit st
    in case T.null u || T.null p of
        True -> continue st
        False -> halt st
onEvent st (VtyEvent e) =
    case focus st of
        Username -> do
            e' <- handleEditorEvent e (usernameEdit st)
            continue $ st { usernameEdit = e' }
        Password -> do
            e' <- handleEditorEvent e (passwordEdit st)
            continue $ st { passwordEdit = e' }
onEvent st _ = continue st
