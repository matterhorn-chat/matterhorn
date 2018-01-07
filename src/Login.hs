{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RankNTypes #-}
module Login
  ( interactiveGatherCredentials
  ) where

import Prelude ()
import Prelude.Compat

import Brick
import Brick.Forms
import Brick.Focus
import Brick.Widgets.Edit
import Brick.Widgets.Center
import Brick.Widgets.Border
import Control.Monad.IO.Class (liftIO)
import Data.Monoid ((<>))
import Lens.Micro.Platform
import qualified Data.Text as T
import Graphics.Vty
import System.Exit (exitSuccess)

import Network.Mattermost.Exceptions (LoginFailureException(..))

import Markdown
import Types ( ConnectionInfo(..), ciPassword, ciUsername, ciHostname
             , ciPort, AuthenticationException(..)
             )

data Name = Hostname | Port | Username | Password deriving (Ord, Eq, Show)

data State =
    State { _loginForm     :: Form ConnectionInfo () Name
          , _previousError :: Maybe AuthenticationException
          }

makeLenses ''State

validHostname :: [T.Text] -> Maybe T.Text
validHostname ls =
    let s = T.unpack t
        t = T.concat ls
    in if all (flip notElem (":/"::String)) s
       then Just t
       else Nothing

interactiveGatherCredentials :: ConnectionInfo
                             -> Maybe AuthenticationException
                             -> IO ConnectionInfo
interactiveGatherCredentials config authError = do
    let state = newState config authError
    finalSt <- defaultMain app state
    return $ formState $ finalSt^.loginForm

newState :: ConnectionInfo -> Maybe AuthenticationException -> State
newState cInfo authError = state
    where
        state = State { _loginForm = form { formFocus = focusSetCurrent initialFocus (formFocus form)
                                          }
                      , _previousError = authError
                      }
        form = mkForm cInfo
        initialFocus = if | T.null (cInfo^.ciHostname) -> Hostname
                          | T.null (cInfo^.ciUsername) -> Username
                          | T.null (cInfo^.ciPassword) -> Password
                          | otherwise                  -> Hostname

app :: App State () Name
app = App
  { appDraw         = credsDraw
  , appChooseCursor = showFirstCursor
  , appHandleEvent  = onEvent
  , appStartEvent   = return
  , appAttrMap      = const colorTheme
  }

editHostname :: (Show n, Ord n) => Lens' s T.Text -> n -> s -> FormFieldState s e n
editHostname stLens n =
    let ini = id
        val = validHostname
        limit = Just 1
        renderTxt = txt . T.unlines
    in editField stLens n limit ini val renderTxt id

mkForm :: ConnectionInfo -> Form ConnectionInfo e Name
mkForm =
    let label s w = padBottom (Pad 1) $
                    (vLimit 1 $ hLimit 15 $ str s <+> fill ' ') <+> w
    in newForm [ label "Hostname:" @@=
                   editHostname ciHostname Hostname
               , label "Port:" @@=
                   editShowableField ciPort Port
               , label "Username:" @@=
                   editTextField ciUsername Username (Just 1)
               , label "Password:" @@=
                   editPasswordField ciPassword Password
               ]

errorAttr :: AttrName
errorAttr = "errorMessage"

colorTheme :: AttrMap
colorTheme = attrMap defAttr
  [ (editAttr, black `on` white)
  , (editFocusedAttr, black `on` yellow)
  , (errorAttr, fg red)
  , (focusedFormInputAttr, black `on` yellow)
  , (invalidFormInputAttr, white `on` red)
  ]

credsDraw :: State -> [Widget Name]
credsDraw st =
    [ center (credentialsForm st <=> errorMessageDisplay st)
    ]

errorMessageDisplay :: State -> Widget Name
errorMessageDisplay st = do
    case st^.previousError of
        Nothing -> emptyWidget
        Just e ->
            hCenter $ hLimit uiWidth $
            padTop (Pad 1) $ renderError $ renderText $
            "Error: " <> renderAuthError e

renderAuthError :: AuthenticationException -> T.Text
renderAuthError (ConnectError _) =
    "Could not connect to server"
renderAuthError (ResolveError _) =
    "Could not resolve server hostname"
renderAuthError (OtherAuthError e) =
    T.pack $ show e
renderAuthError (LoginError (LoginFailureException msg)) =
    T.pack msg

renderError :: Widget a -> Widget a
renderError = withDefAttr errorAttr

uiWidth :: Int
uiWidth = 50

credentialsForm :: State -> Widget Name
credentialsForm st =
    hCenter $ hLimit uiWidth $ vLimit 15 $
    border $
    vBox [ renderText "Please enter your Mattermost credentials to log in."
         , padTop (Pad 1) $ renderForm (st^.loginForm)
         , hCenter $ renderText "Press Enter to log in or Esc to exit."
         ]

onEvent :: State -> BrickEvent Name () -> EventM Name (Next State)
onEvent _  (VtyEvent (EvKey KEsc [])) = liftIO exitSuccess
onEvent st (VtyEvent (EvKey KEnter [])) =
    if allFieldsValid (st^.loginForm) then halt st else continue st
onEvent st e = do
    f' <- handleFormEvent e (st^.loginForm)
    continue $ st & loginForm .~ f'
