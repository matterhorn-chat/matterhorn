module State.Setup where

import           Brick (EventM)
import           Brick.BChan
import           Brick.Widgets.List (list)
import           Control.Concurrent (threadDelay, forkIO)
import qualified Control.Concurrent.Chan as Chan
import           Control.Concurrent.MVar (newEmptyMVar)
import           Control.Exception (catch)
import           Control.Monad (forM, when, void)
import           Control.Monad.IO.Class (liftIO)
import qualified Data.Foldable as F
import qualified Data.HashMap.Strict as HM
import           Data.List (sort)
import           Data.Maybe (listToMaybe, maybeToList, fromJust)
import           Data.Monoid ((<>))
import qualified Data.Sequence as Seq
import           Data.Time.LocalTime ( TimeZone(..), getCurrentTimeZone )
import           Lens.Micro.Platform
import           System.Exit (exitFailure)
import           System.IO (Handle)

import           Network.Mattermost
import           Network.Mattermost.Lenses
import           Network.Mattermost.Logging (mmLoggerDebug)

import           Config
import           InputHistory
import           Login
import           State.Common
import           TeamSelect
import           Themes
import           Types
import           Zipper (Zipper)
import qualified Zipper as Z

fetchUserStatuses :: ConnectionData -> Token
                  -> IO (ChatState -> EventM Name ChatState)
fetchUserStatuses cd token = do
  statusMap <- mmGetStatuses cd token
  return $ \ appState ->
    return $ HM.foldrWithKey
      (\ uId status st ->
          st & usrMap.ix(uId).uiStatus .~ statusFromText status)
      appState
      statusMap

startTimezoneMonitor :: TimeZone -> RequestChan -> IO ()
startTimezoneMonitor tz requestChan = do
  -- Start the timezone monitor thread
  let timezoneMonitorSleepInterval = minutes 5
      minutes = (* (seconds 60))
      seconds = (* (1000 * 1000))
      timezoneMonitor prevTz = do
        threadDelay timezoneMonitorSleepInterval

        newTz <- getCurrentTimeZone
        when (newTz /= prevTz) $
            Chan.writeChan requestChan $ do
                return $ (return . (& timeZone .~ newTz))

        timezoneMonitor newTz

  void $ forkIO (timezoneMonitor tz)

mkChanNames :: User -> HM.HashMap UserId UserProfile -> Seq.Seq Channel -> MMNames
mkChanNames myUser users chans = MMNames
  { _cnChans = sort
               [ channelName c
               | c <- F.toList chans, channelType c /= Direct ]
  , _cnDMs = sort
             [ channelName c
             | c <- F.toList chans, channelType c == Direct ]
  , _cnToChanId = HM.fromList $
                  [ (channelName c, channelId c) | c <- F.toList chans ] ++
                  [ (userProfileUsername u, c)
                  | u <- HM.elems users
                  , c <- lookupChan (getDMChannelName (getId myUser) (getId u))
                  ]
  , _cnUsers = sort (map userProfileUsername (HM.elems users))
  , _cnToUserId = HM.fromList
                  [ (userProfileUsername u, getId u) | u <- HM.elems users ]
  }
  where lookupChan n = [ c^.channelIdL
                       | c <- F.toList chans, c^.channelNameL == n
                       ]

newState :: ChatResources
         -> Zipper ChannelId
         -> User
         -> Team
         -> TimeZone
         -> InputHistory
         -> ChatState
newState rs i u m tz hist = ChatState
  { _csResources                   = rs
  , _csFocus                       = i
  , _csMe                          = u
  , _csMyTeam                      = m
  , _csNames                       = emptyMMNames
  , _msgMap                        = HM.empty
  , _csPostMap                     = HM.empty
  , _usrMap                        = HM.empty
  , _timeZone                      = tz
  , _csEditState                   = emptyEditState hist
  , _csMode                        = Main
  , _csShowMessagePreview          = configShowMessagePreview $ rs^.crConfiguration
  , _csChannelSelectString         = ""
  , _csChannelSelectChannelMatches = mempty
  , _csChannelSelectUserMatches    = mempty
  , _csRecentChannel               = Nothing
  , _csUrlList                     = list UrlList mempty 2
  , _csConnectionStatus            = Disconnected
  , _csJoinChannelList             = Nothing
  }

setupState :: Maybe Handle -> Config -> RequestChan -> BChan MHEvent -> IO ChatState
setupState logFile config requestChan eventChan = do
  -- If we don't have enough credentials, ask for them.
  (uStr, pStr) <- case getCredentials config of
      Nothing -> interactiveGatherCredentials config Nothing
      Just (u, p) -> return (u, p)

  let setLogger = case logFile of
        Nothing -> id
        Just f  -> \ cd -> cd `withLogger` mmLoggerDebug f

  cd <- setLogger `fmap`
          initConnectionData (configHost config)
                             (fromIntegral (configPort config))

  let loginLoop (u, p) = do
        putStrLn "Authenticating..."

        let login = Login { username = u, password = p }
        result <- (Right <$> mmLogin cd login)
                    `catch` (\e -> return $ Left $ ResolveError e)
                    `catch` (\e -> return $ Left $ ConnectError e)
                    `catch` (\e -> return $ Left $ OtherAuthError e)

        case result of
            Right (Right values) ->
                return values
            Right (Left e) ->
                interactiveGatherCredentials config (Just $ LoginError e) >>=
                    loginLoop
            Left e ->
                interactiveGatherCredentials config (Just e) >>=
                    loginLoop

  (token, myUser) <- loginLoop (uStr, pStr)

  initialLoad <- mmGetInitialLoad cd token
  when (Seq.null $ initialLoadTeams initialLoad) $ do
      putStrLn "Error: your account is not a member of any teams"
      exitFailure

  myTeam <- case configTeam config of
      Nothing -> do
          interactiveTeamSelection $ F.toList $ initialLoadTeams initialLoad
      Just tName -> do
          let matchingTeam = listToMaybe $ filter matches $ F.toList $ initialLoadTeams initialLoad
              matches t = teamName t == tName
          case matchingTeam of
              Nothing -> interactiveTeamSelection (F.toList (initialLoadTeams initialLoad))
              Just t -> return t

  quitCondition <- newEmptyMVar
  let themeName = case configTheme config of
          Nothing -> defaultThemeName
          Just t -> t
      theme = case lookup themeName themes of
          Nothing -> fromJust $ lookup defaultThemeName themes
          Just t -> t
      cr = ChatResources
             { _crTok           = token
             , _crConn          = cd
             , _crRequestQueue  = requestChan
             , _crEventQueue    = eventChan
             , _crTheme         = theme
             , _crQuitCondition = quitCondition
             , _crConfiguration = config
             }
  initializeState cr myTeam myUser

initializeState :: ChatResources -> Team -> User -> IO ChatState
initializeState cr myTeam myUser = do
  let ChatResources token cd requestChan _ _ _ _ = cr
  let myTeamId = getId myTeam

  Chan.writeChan requestChan $ fetchUserStatuses cd token

  putStrLn $ "Loading channels for team " <> show (teamName myTeam) <> "..."
  chans <- mmGetChannels cd token myTeamId

  msgs <- fmap (HM.fromList . F.toList) $ forM (F.toList chans) $ \c -> do
      ChannelWithData _ chanData <- mmGetChannel cd token myTeamId (getId c)

      let viewed   = chanData ^. channelDataLastViewedAtL
          updated  = c ^. channelLastPostAtL
          cInfo    = ChannelInfo
                       { _cdViewed           = viewed
                       , _cdUpdated          = updated
                       , _cdName             = c^.channelNameL
                       , _cdHeader           = c^.channelHeaderL
                       , _cdType             = c^.channelTypeL
                       , _cdCurrentState     = ChanUnloaded
                       , _cdNewMessageCutoff = Just viewed
                       }
          cChannel = ClientChannel
                       { _ccContents = emptyChannelContents
                       , _ccInfo     = cInfo
                       }

      return (getId c, cChannel)

  users <- mmGetProfiles cd token myTeamId
  tz    <- getCurrentTimeZone
  hist  <- do
      result <- readHistory
      case result of
          Left _ -> return newHistory
          Right h -> return h

  startTimezoneMonitor tz requestChan

  let chanNames = mkChanNames myUser users chans
      Just townSqId = chanNames ^. cnToChanId . at "town-square"
      chanIds = [ (chanNames ^. cnToChanId) HM.! i
                | i <- chanNames ^. cnChans ] ++
                [ c
                | i <- chanNames ^. cnUsers
                , c <- maybeToList (HM.lookup i (chanNames ^. cnToChanId)) ]
      chanZip = Z.findRight (== townSqId) (Z.fromList chanIds)
      st = newState cr chanZip myUser myTeam tz hist
             & usrMap .~ fmap userInfoFromProfile users
             & msgMap .~ msgs
             & csNames .~ chanNames

  -- Fetch town-square asynchronously, but put it in the queue early.
  case F.find ((== townSqId) . getId) chans of
      Nothing -> return ()
      Just _ -> doAsync st $ liftIO $ asyncFetchScrollback st townSqId

  F.forM_ chans $ \c ->
      when (getId c /= townSqId && c^.channelTypeL /= Direct) $
          doAsync st $ asyncFetchScrollback st (getId c)

  updateViewedIO st
