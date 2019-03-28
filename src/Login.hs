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
import           Control.Concurrent ( forkIO, threadDelay )
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
                  | AttemptSucceeded ConnectionInfo ConnectionData Session User

data LoginState = Idle
                | Connecting Bool Text
                deriving (Eq)

data LoginRequest = DoLogin Bool ConnectionInfo

data LoginEvent = StartConnect Bool Text
                | LoginResult LoginAttempt
                | StartupTimeout

data State =
    State { _loginForm :: Form ConnectionInfo LoginEvent Name
          , _lastAttempt :: Maybe LoginAttempt
          , _currentState :: LoginState
          , _reqChan :: BChan LoginRequest
          , _timeoutFired :: Bool
          }

makeLenses ''State

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
                    writeBChan respChan $ LoginResult $ AttemptSucceeded connInfo cd sess user

startupTimerMilliseconds :: Int
startupTimerMilliseconds = 750

startupTimer :: BChan LoginEvent -> IO ()
startupTimer respChan = do
    threadDelay $ startupTimerMilliseconds * 1000
    writeBChan respChan StartupTimeout

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
    void $ forkIO $ startupTimer respChan

    let initialState = mkState initialConfig requestChan

    startState <- case (populatedConnectionInfo initialConfig) of
        True -> do
            writeBChan requestChan $ DoLogin True initialConfig
            return $ initialState & currentState .~ Connecting True (initialConfig^.ciHostname)
        False -> do
            return initialState

    (finalSt, finalVty) <- customMainWithVty vty mkVty (Just respChan) app startState

    -- return (formState $ finalSt^.loginForm, finalVty)
    return (finalSt^.lastAttempt, finalVty)

populatedConnectionInfo :: ConnectionInfo -> Bool
populatedConnectionInfo ci =
    and [ not $ T.null $ ci^.ciHostname
        , not $ T.null $ ci^.ciUsername
        , not $ T.null $ ci^.ciPassword
        , ci^.ciPort > 0
        ]

mkState :: ConnectionInfo -> BChan LoginRequest -> State
mkState cInfo chan = state
    where
        state = State { _loginForm = form { formFocus = focusSetCurrent initialFocus (formFocus form)
                                          }
                      , _currentState = Idle
                      , _lastAttempt = Nothing
                      , _reqChan = chan
                      , _timeoutFired = False
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

onEvent :: State -> BrickEvent Name LoginEvent -> EventM Name (Next State)
onEvent _  (VtyEvent (EvKey KEsc [])) = liftIO exitSuccess
onEvent st (AppEvent (StartConnect initial host)) = do
    continue $ st & currentState .~ Connecting initial host
                  & lastAttempt .~ Nothing
onEvent st (AppEvent StartupTimeout) = do
    -- If the startup timer fired and we have already succeeded, halt.
    case st^.lastAttempt of
        Just (AttemptSucceeded {}) -> halt st
        _ -> continue $ st & timeoutFired .~ True
onEvent st (AppEvent (LoginResult attempt)) = do
    let st' = st & lastAttempt .~ Just attempt
                 & currentState .~ Idle

    case attempt of
        AttemptSucceeded {} -> do
            -- If the startup timer already fired, halt. Otherwise wait
            -- until that timer fires.
            case st^.timeoutFired of
                True -> halt st'
                False -> continue st'
        AttemptFailed {} -> continue st'

onEvent st (VtyEvent (EvKey KEnter [])) = do
    case st^.currentState of
        Connecting {} -> return ()
        Idle -> do
            let ci = formState $ st^.loginForm
            when (populatedConnectionInfo ci) $ do
                liftIO $ writeBChan (st^.reqChan) $ DoLogin False ci

    continue st
onEvent st e = do
    f' <- handleFormEvent e (st^.loginForm)
    continue $ st & loginForm .~ f'

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

editHostname :: (Show n, Ord n) => Lens' s Text -> n -> s -> FormFieldState s e n
editHostname stLens n =
    let ini = id
        val = validHostname
        limit = Just 1
        renderTxt = txt . T.unlines
    in editField stLens n limit ini val renderTxt id

validHostname :: [Text] -> Maybe Text
validHostname ls =
    let s = T.unpack t
        t = T.concat ls
    in if all (flip notElem (":/"::String)) s
       then Just t
       else Nothing

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
                    , currentStateDisplay st
                    , lastAttemptDisplay (st^.lastAttempt)
                    ]
    ]

shouldShowForm :: State -> Bool
shouldShowForm st =
    case st^.currentState of
        Connecting initial _ -> not initial
        Idle -> case st^.lastAttempt of
            Just (AttemptSucceeded {}) -> st^.timeoutFired
            _ -> True

currentStateDisplay :: State -> Widget Name
currentStateDisplay st =
    let msg host = padTop (Pad 1) $ hCenter $
                   txt "Connecting to " <+> withDefAttr clientEmphAttr (txt host) <+> txt "..."
    in case st^.currentState of
          Idle -> case st^.lastAttempt of
              Just (AttemptSucceeded ci _ _ _) -> msg (ci^.ciHostname)
              _ -> emptyWidget
          (Connecting _ host) -> msg host

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
