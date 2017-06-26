{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RankNTypes #-}
module Login
  ( interactiveGatherCredentials
  ) where

import Prelude ()
import Prelude.Compat

import Brick
import Brick.Widgets.Edit
import Brick.Widgets.Center
import Brick.Widgets.Border
import Control.Monad.IO.Class (liftIO)
import Data.Char (isNumber)
import Data.Maybe (isNothing)
import Data.Monoid ((<>))
import Text.Read (readMaybe)
import Lens.Micro.Platform
import qualified Data.Text as T
import Graphics.Vty hiding (Config)
import System.Exit (exitSuccess)

import Network.Mattermost.Exceptions (LoginFailureException(..))

import Config
import Markdown
import Types (ConnectionInfo(..), AuthenticationException(..))

data Name = Hostname | Port | Username | Password deriving (Ord, Eq, Show)

data State =
    State { _hostnameEdit  :: Editor T.Text Name
          , _portEdit      :: Editor T.Text Name
          , _usernameEdit  :: Editor T.Text Name
          , _passwordEdit  :: Editor T.Text Name
          , _focus         :: Name
          , _previousError :: Maybe AuthenticationException
          }

makeLenses ''State

toPassword :: [T.Text] -> Widget a
toPassword s = txt $ T.replicate (T.length $ T.concat s) "*"

validHostname :: State -> Bool
validHostname st =
    all (flip notElem (":/"::String)) $ T.unpack $ T.concat $ getEditContents $ st^.hostnameEdit

validPort :: State -> Bool
validPort st =
    all isNumber $ T.unpack $ T.concat $ getEditContents $ st^.portEdit

interactiveGatherCredentials :: Config
                             -> Maybe AuthenticationException
                             -> IO ConnectionInfo
interactiveGatherCredentials config authError = do
    let state = newState config authError
    finalSt <- defaultMain app state
    let finalH    = T.concat $ getEditContents $ finalSt^.hostnameEdit
        finalPort = read $ T.unpack $ T.concat $ getEditContents $ finalSt^.portEdit
        finalU    = T.concat $ getEditContents $ finalSt^.usernameEdit
        finalPass = T.concat $ getEditContents $ finalSt^.passwordEdit
    return $ ConnectionInfo finalH finalPort finalU finalPass

newState :: Config -> Maybe AuthenticationException -> State
newState config authError = state
    where
        state = State { _hostnameEdit = editor Hostname (Just 1) hStr
                      , _portEdit = editor Port (Just 1) (T.pack $ show $ configPort config)
                      , _usernameEdit = editor Username (Just 1) uStr
                      , _passwordEdit = editor Password (Just 1) pStr
                      , _focus = initialFocus
                      , _previousError = authError
                      }
        hStr = maybe "" id $ configHost config
        uStr = maybe "" id $ configUser config
        pStr = case configPass config of
            Just (PasswordString s) -> s
            _                       -> ""
        initialFocus = if | T.null hStr -> Hostname
                          | T.null uStr -> Username
                          | T.null pStr -> Password
                          | otherwise   -> Hostname

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
         , padTop (Pad 1) $
           txt "Hostname: " <+> renderEditor (txt . T.concat) (st^.focus == Hostname) (st^.hostnameEdit)
         , if validHostname st
              then txt " "
              else hCenter $ renderError $ txt "Invalid hostname"
         , txt "Port:     " <+> renderEditor (txt . T.concat) (st^.focus == Port) (st^.portEdit)
         , if validPort st
              then txt " "
              else hCenter $ renderError $ txt "Invalid port"
         , txt "Username: " <+> renderEditor (txt . T.concat) (st^.focus == Username) (st^.usernameEdit)
         , padTop (Pad 1) $
           txt "Password: " <+> renderEditor toPassword (st^.focus == Password) (st^.passwordEdit)
         , padTop (Pad 1) $
           hCenter $ renderText "Press Enter to log in or Esc to exit."
         ]

onEvent :: State -> BrickEvent Name e -> EventM Name (Next State)
onEvent _  (VtyEvent (EvKey KEsc [])) = liftIO exitSuccess
onEvent st (VtyEvent (EvKey (KChar '\t') [])) =
    continue $ st & focus %~ nextFocus
onEvent st (VtyEvent (EvKey KEnter [])) =
    if badState st then continue st else halt st
onEvent st (VtyEvent e) = do
    let target :: Lens' State (Editor T.Text Name)
        target = getFocusedEditor st
    continue =<< handleEventLensed st target handleEditorEvent e
onEvent st _ = continue st

nextFocus :: Name -> Name
nextFocus Hostname = Port
nextFocus Port     = Username
nextFocus Username = Password
nextFocus Password = Hostname

getFocusedEditor :: State -> Lens' State (Editor T.Text Name)
getFocusedEditor st =
    case st^.focus of
        Hostname -> hostnameEdit
        Port     -> portEdit
        Username -> usernameEdit
        Password -> passwordEdit

badState :: State -> Bool
badState st = bad
    where
        -- check for valid (non-empty) contents
        h = T.concat $ getEditContents $ st^.hostnameEdit
        u = T.concat $ getEditContents $ st^.usernameEdit
        p = T.concat $ getEditContents $ st^.passwordEdit
        port :: Maybe Int
        port = readMaybe (T.unpack $ T.concat $ getEditContents $ st^.portEdit)
        bad = or [ T.null h
                 , T.null u
                 , T.null p
                 , isNothing port
                 , (not $ validHostname st)
                 , (not $ validPort st)
                 ]
