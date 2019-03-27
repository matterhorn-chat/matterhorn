{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RankNTypes #-}
module Login
  ( LoginAttempt(..)
  , interactiveGetLoginSession
  )
where

import           Prelude ()
import           Prelude.MH

import           Brick
import           Brick.BChan
import           Brick.Focus
import           Brick.Forms
import           Brick.Widgets.Border
import           Brick.Widgets.Center
import           Brick.Widgets.Edit
import           Control.Concurrent ( forkIO )
import           Control.Exception ( catch )
import           System.IO.Error ( catchIOError )
import qualified Data.Text as T
import           Graphics.Vty hiding (mkVty)
import           Lens.Micro.Platform ( (.~), Lens', makeLenses )
import           System.Exit ( exitSuccess )
import qualified System.IO.Error as Err

import           Network.Mattermost ( ConnectionData )
import           Network.Mattermost.Types ( Session, User, Login(..), ConnectionPoolConfig(..)
                                          , initConnectionData, initConnectionDataInsecure )
import           Network.Mattermost.Exceptions ( LoginFailureException(..) )
import           Network.Mattermost.Endpoints ( mmLogin )

import           Markdown
import           Themes ( clientEmphAttr )
import           Types ( ConnectionInfo(..)
                       , ciPassword, ciUsername, ciHostname
                       , ciPort, AuthenticationException(..)
                       )


data Name = Hostname | Port | Username | Password deriving (Ord, Eq, Show)

data LoginAttempt = AttemptFailed AuthenticationException
                  | AttemptSucceeded ConnectionData Session User

data LoginState = Idle
                | Connecting Bool Text
                deriving (Eq)

data LoginRequest = DoLogin Bool ConnectionInfo

data LoginEvent = StartConnect Bool Text
                | LoginResult LoginAttempt

data State =
    State { _loginForm :: Form ConnectionInfo LoginEvent Name
          , _lastAttempt :: Maybe LoginAttempt
          , _currentState :: LoginState
          , _reqChan :: BChan LoginRequest
          }

makeLenses ''State

validHostname :: [Text] -> Maybe Text
validHostname ls =
    let s = T.unpack t
        t = T.concat ls
    in if all (flip notElem (":/"::String)) s
       then Just t
       else Nothing

poolCfg :: ConnectionPoolConfig
poolCfg = ConnectionPoolConfig { cpIdleConnTimeout = 60
                               , cpStripesCount = 1
                               , cpMaxConnCount = 5
                               }

convertLoginExceptions :: IO a -> IO (Either AuthenticationException a)
convertLoginExceptions act =
    (Right <$> act)
        `catch` (\e -> return $ Left $ ResolveError e)
        `catch` (\e -> return $ Left $ ConnectError e)
        `catchIOError` (\e -> return $ Left $ AuthIOError e)
        `catch` (\e -> return $ Left $ OtherAuthError e)

loginWorker :: (ConnectionData -> ConnectionData) -> Bool -> BChan LoginRequest -> BChan LoginEvent -> IO ()
loginWorker setLogger unsafeUseHTTP requestChan respChan = forever $ do
    req <- readBChan requestChan
    case req of
        DoLogin initial connInfo -> do
            writeBChan respChan $ StartConnect initial $ connInfo^.ciHostname

            let connectFunc = if unsafeUseHTTP
                              then initConnectionDataInsecure
                              else initConnectionData
                login = Login { username = connInfo^.ciUsername
                              , password = connInfo^.ciPassword
                              }

            cd <- fmap setLogger $ connectFunc (connInfo^.ciHostname) (fromIntegral (connInfo^.ciPort)) poolCfg

            result <- convertLoginExceptions $ mmLogin cd login
            case result of
                Left e ->
                    writeBChan respChan $ LoginResult $ AttemptFailed e
                Right (Left e) ->
                    writeBChan respChan $ LoginResult $ AttemptFailed $ LoginError e
                Right (Right (sess, user)) ->
                    writeBChan respChan $ LoginResult $ AttemptSucceeded cd sess user

interactiveGetLoginSession :: Vty
                           -> IO Vty
                           -> Bool
                           -> (ConnectionData -> ConnectionData)
                           -> ConnectionInfo
                           -> IO (Maybe LoginAttempt, Vty)
interactiveGetLoginSession vty mkVty unsafeUseHTTP setLogger initialConfig = do
    requestChan <- newBChan 10
    respChan <- newBChan 10

    void $ forkIO $ loginWorker setLogger unsafeUseHTTP requestChan respChan

    let initialState = newState initialConfig requestChan

    when (populatedConnectionInfo initialConfig) $ do
        writeBChan requestChan $ DoLogin True initialConfig

    (finalSt, finalVty) <- customMainWithVty vty mkVty (Just respChan) app initialState

    -- return (formState $ finalSt^.loginForm, finalVty)
    return (finalSt^.lastAttempt, finalVty)

populatedConnectionInfo :: ConnectionInfo -> Bool
populatedConnectionInfo ci =
    and [ not $ T.null $ ci^.ciHostname
        , not $ T.null $ ci^.ciUsername
        , not $ T.null $ ci^.ciPassword
        , ci^.ciPort > 0
        ]

newState :: ConnectionInfo -> BChan LoginRequest -> State
newState cInfo chan = state
    where
        state = State { _loginForm = form { formFocus = focusSetCurrent initialFocus (formFocus form)
                                          }
                      , _currentState = Idle
                      , _lastAttempt = Nothing
                      , _reqChan = chan
                      }
        form = mkForm cInfo
        initialFocus = if | T.null (cInfo^.ciHostname) -> Hostname
                          | T.null (cInfo^.ciUsername) -> Username
                          | T.null (cInfo^.ciPassword) -> Password
                          | otherwise                  -> Hostname

app :: App State LoginEvent Name
app = App
  { appDraw         = credsDraw
  , appChooseCursor = showFirstCursor
  , appHandleEvent  = onEvent
  , appStartEvent   = return
  , appAttrMap      = const colorTheme
  }

editHostname :: (Show n, Ord n) => Lens' s Text -> n -> s -> FormFieldState s e n
editHostname stLens n =
    let ini = id
        val = validHostname
        limit = Just 1
        renderTxt = txt . T.unlines
    in editField stLens n limit ini val renderTxt id

mkForm :: ConnectionInfo -> Form ConnectionInfo e Name
mkForm =
    let label s w = padBottom (Pad 1) $
                    (vLimit 1 $ hLimit 18 $ str s <+> fill ' ') <+> w
    in newForm [ label "Server hostname:" @@=
                   editHostname ciHostname Hostname
               , label "Server port:" @@=
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
  , (clientEmphAttr, fg white `withStyle` bold)
  ]

credsDraw :: State -> [Widget Name]
credsDraw st =
    [ center $ vBox [ if shouldShowForm st then credentialsForm st else emptyWidget
                    , currentStateDisplay (st^.currentState)
                    , lastAttemptDisplay (st^.lastAttempt)
                    ]
    ]

shouldShowForm :: State -> Bool
shouldShowForm st =
    case st^.currentState of
        Connecting initial _ -> not initial
        _ -> True

currentStateDisplay :: LoginState -> Widget Name
currentStateDisplay Idle =
    emptyWidget
currentStateDisplay (Connecting _ host) =
    let msg = txt "Connecting to " <+> withDefAttr clientEmphAttr (txt host) <+> txt "..."
    in padTop (Pad 1) $ hCenter msg

lastAttemptDisplay :: Maybe LoginAttempt -> Widget Name
lastAttemptDisplay Nothing = emptyWidget
lastAttemptDisplay (Just (AttemptSucceeded {})) = emptyWidget
lastAttemptDisplay (Just (AttemptFailed e)) =
    hCenter $ hLimit uiWidth $
    padTop (Pad 1) $ renderError $ renderText $
    "Error: " <> renderAuthError e

renderAuthError :: AuthenticationException -> Text
renderAuthError (ConnectError _) =
    "Could not connect to server"
renderAuthError (ResolveError _) =
    "Could not resolve server hostname"
renderAuthError (AuthIOError err)
  | Err.isDoesNotExistErrorType (Err.ioeGetErrorType err) =
    "Unable to connect to the network"
  | otherwise = "GetAddrInfo: " <> T.pack (Err.ioeGetErrorString err)
renderAuthError (OtherAuthError e) =
    T.pack $ show e
renderAuthError (LoginError (LoginFailureException msg)) =
    T.pack msg

renderError :: Widget a -> Widget a
renderError = withDefAttr errorAttr

uiWidth :: Int
uiWidth = 60

credentialsForm :: State -> Widget Name
credentialsForm st =
    hCenter $ hLimit uiWidth $ vLimit 15 $
    border $
    vBox [ renderText "Please enter your Mattermost credentials to log in."
         , padTop (Pad 1) $ renderForm (st^.loginForm)
         , hCenter $ renderText "Press Enter to log in or Esc to exit."
         ]

onEvent :: State -> BrickEvent Name LoginEvent -> EventM Name (Next State)
onEvent _  (VtyEvent (EvKey KEsc [])) = liftIO exitSuccess
onEvent st (AppEvent (StartConnect initial host)) = do
    continue $ st & currentState .~ Connecting initial host
                  & lastAttempt .~ Nothing
onEvent st (AppEvent (LoginResult attempt)) = do
    let st' = st & lastAttempt .~ Just attempt
                 & currentState .~ Idle

    case attempt of
        AttemptSucceeded {} -> halt st'
        AttemptFailed {} -> continue st'

onEvent st (VtyEvent (EvKey KEnter [])) = do
    let ci = formState $ st^.loginForm
    when (populatedConnectionInfo ci) $ do
        liftIO $ writeBChan (st^.reqChan) $ DoLogin False ci

    continue st
onEvent st e = do
    f' <- handleFormEvent e (st^.loginForm)
    continue $ st & loginForm .~ f'
