{-# LANGUAGE OverloadedStrings #-}
module State where

import           Brick (EventM, str, vBox, Direction(..))
import           Brick.AttrMap (AttrMap)
import           Brick.Widgets.Edit (editor, getEditContents)
import           Control.Concurrent (threadDelay, forkIO)
import qualified Control.Concurrent.Chan as Chan
import           Control.Monad.IO.Class (liftIO)
import           Data.HashMap.Strict ((!))
import           Brick.Main (viewportScroll, vScrollToEnd, vScrollPage)
import           Brick.Widgets.Edit (applyEdit)
import           Control.Exception (SomeException, catch)
import           Control.Monad (forM, when, void)
import           Data.Text.Zipper (clearZipper, insertMany)
import qualified Data.HashMap.Strict as HM
import qualified Data.Sequence as Seq
import           Data.Foldable (toList)
import           Data.List (sort, intercalate)
import           Data.Maybe (listToMaybe, maybeToList, fromJust)
import           Data.Monoid ((<>))
import           Data.Time.Clock ( getCurrentTime )
import           Data.Time.LocalTime ( TimeZone(..), getCurrentTimeZone )
import qualified Data.Text as T
import           Lens.Micro.Platform
import           System.Exit (exitFailure)

import           Network.Connection
import           Network.Mattermost
import           Network.Mattermost.Exceptions
import           Network.Mattermost.Lenses

import           Config
import           Types
import           TeamSelect
import           InputHistory
import           Themes
import           Login
import           Zipper (Zipper)
import qualified Zipper as Z

fromPosts :: ChatState -> Posts -> ChannelContents
fromPosts st p = ChannelContents $ messagesFromPosts st p

messagesFromPosts :: ChatState -> Posts -> Seq.Seq Message
messagesFromPosts st p = msgs
    where
        msgs = clientPostToMessage st <$> toClientPost <$> ps
        ps   = findPost <$> (Seq.reverse $ postsOrder p)
        findPost pId = case HM.lookup pId (postsPosts p) of
            Nothing -> error $ "BUG: could not find post for post ID " <> show pId
            Just post -> post

numScrollbackPosts :: Int
numScrollbackPosts = 100

newState :: Token
         -> ConnectionData
         -> Zipper ChannelId
         -> User
         -> Team
         -> TimeZone
         -> Maybe T.Text
         -> InputHistory
         -> RequestChan
         -> AttrMap
         -> ChatState
newState t c i u m tz fmt hist rq theme = ChatState
  { _csTok                  = t
  , _csConn                 = c
  , _csFocus                = i
  , _csMe                   = u
  , _csMyTeam               = m
  , _csNames                = MMNames [] [] HM.empty [] HM.empty
  , _msgMap                 = HM.empty
  , _usrMap                 = HM.empty
  , _cmdLine                = editor MessageInput (vBox . map str) (Just 1) ""
  , _timeZone               = tz
  , _csRequestQueue         = rq
  , _timeFormat             = fmt
  , _csInputHistory         = hist
  , _csInputHistoryPosition = mempty
  , _csLastChannelInput     = mempty
  , _csCurrentCompletion    = Nothing
  , _csTheme                = theme
  , _csMode                 = Main
  , _csChannelSelect        = ""
  }

runAsync :: ChatState -> IO (ChatState -> EventM Name ChatState) -> IO ()
runAsync st thunk =
  Chan.writeChan (st^.csRequestQueue) thunk

doAsync :: ChatState -> IO () -> IO ()
doAsync st thunk = doAsyncWith st (thunk >> return return)

doAsyncWith :: ChatState -> IO (ChatState -> EventM Name ChatState) -> IO ()
doAsyncWith st thunk =
  Chan.writeChan (st^.csRequestQueue) thunk

hasUnread :: ChatState -> ChannelId -> Bool
hasUnread st cId = maybe False id $ do
  chan <- st^.msgMap.at(cId)
  let u = chan^.ccInfo.cdViewed
      v = chan^.ccInfo.cdUpdated
  return (v > u)

updateViewed :: ChatState -> EventM Name ChatState
updateViewed st = liftIO (updateViewedIO st)

updateViewedIO :: ChatState -> IO ChatState
updateViewedIO st = do
  now <- getCurrentTime
  let cId = currentChannelId st
  runAsync st $ do
    mmUpdateLastViewedAt
      (st^.csConn)
      (st^.csTok)
      (getId (st^.csMyTeam))
      cId
    return (\s -> return (s & msgMap . ix cId . ccInfo . cdViewed .~ now))
  return st

resetHistoryPosition :: ChatState -> EventM a ChatState
resetHistoryPosition st =
    let cId = currentChannelId st
    in return $ st & csInputHistoryPosition.at cId .~ Just Nothing

clearEditor :: ChatState -> EventM a ChatState
clearEditor st = return $ st & cmdLine %~ applyEdit clearZipper

loadLastEdit :: ChatState -> EventM a ChatState
loadLastEdit st =
    let cId = currentChannelId st
    in return $ case st^.csLastChannelInput.at cId of
        Nothing -> st
        Just lastEdit -> st & cmdLine %~ (applyEdit $ insertMany (T.unpack lastEdit))

changeChannelCommon :: ChatState -> EventM Name ChatState
changeChannelCommon st =
    loadLastEdit =<<
    clearEditor =<<
    updateChannelScrollState =<<
    resetHistoryPosition st

preChangeChannelCommon :: ChatState -> EventM Name ChatState
preChangeChannelCommon st = do
    let curEdit = intercalate "\n" $ getEditContents $ st^.cmdLine
        cId = currentChannelId st
    return $ st & csLastChannelInput.at cId .~ Just (T.pack curEdit)

nextChannel :: ChatState -> EventM Name ChatState
nextChannel st = setFocusWith st Z.right

prevChannel :: ChatState -> EventM Name ChatState
prevChannel st = setFocusWith st Z.left

listThemes :: ChatState -> EventM Name ChatState
listThemes cs = do
    let mkThemeList _ = T.unlines $
                        "Available built-in themes:" :
                        (("  " <>) <$> fst <$> themes)
    msg <- newClientMessage Informative (mkThemeList themes)
    addClientMessage msg cs

setTheme :: ChatState -> T.Text -> EventM Name ChatState
setTheme cs name =
    case lookup name themes of
        Nothing -> listThemes cs
        Just t -> return $ cs & csTheme .~ t

updateChannelScrollState :: ChatState -> EventM Name ChatState
updateChannelScrollState st = do
  let cId = currentChannelId st
  vScrollToEnd $ viewportScroll (ChannelMessages cId)
  return st

channelPageUp :: ChatState -> EventM Name ChatState
channelPageUp st = do
  let cId = currentChannelId st
  vScrollPage (viewportScroll (ChannelMessages cId)) Up
  return st

channelPageDown :: ChatState -> EventM Name ChatState
channelPageDown st = do
  let cId = currentChannelId st
  vScrollPage (viewportScroll (ChannelMessages cId)) Down
  return st

currentChannelId :: ChatState -> ChannelId
currentChannelId st = Z.focus (st ^. csFocus)

channelByName :: ChatState -> T.Text -> Maybe ChannelId
channelByName st n
    | "#" `T.isPrefixOf` n = st ^. csNames . cnToChanId . at (T.tail n)
    | "@" `T.isPrefixOf` n = st ^. csNames . cnToChanId . at (T.tail n)
    | otherwise            = st ^. csNames . cnToChanId . at n

setFocus :: ChannelId -> ChatState -> EventM Name ChatState
setFocus cId st = setFocusWith st (Z.findRight (== cId))

setFocusWith :: ChatState -> (Zipper ChannelId -> Zipper ChannelId) -> EventM Name ChatState
setFocusWith st f = changeChannelCommon =<<
                (\st' -> updateViewed (st' & csFocus %~ f)) =<<
                preChangeChannelCommon st

editMessage :: Post -> ChatState -> EventM a ChatState
editMessage new st = do
  now <- liftIO getCurrentTime
  let chan = msgMap . ix (postChannelId new)
      rs = st & chan . ccContents . cdMessages %~ (Seq.|> (clientPostToMessage st $ toClientPost new))
              & chan . ccInfo . cdUpdated .~ now
  return rs

deleteMessage :: Post -> ChatState -> EventM a ChatState
deleteMessage new st = do
  now <- liftIO getCurrentTime
  let isDeletedMessage m = m^.mPostId == Just (new^.postIdL)
      chan = msgMap . ix (postChannelId new)
      rs = st & chan . ccContents . cdMessages . each . filtered isDeletedMessage %~ (& mDeleted .~ True)
              & chan . ccInfo . cdUpdated .~ now
  return rs

addMessage :: Post -> ChatState -> EventM Name ChatState
addMessage new st = do
  now <- liftIO getCurrentTime
  let cp = toClientPost new
      updateTime = if cp^.cpUser == getId (st^.csMe)
                   then id
                   else const now
  let chan = msgMap . ix (postChannelId new)
      rs = st & chan . ccContents . cdMessages %~ (Seq.|> (clientPostToMessage st $ toClientPost new))
              & chan . ccInfo . cdUpdated %~ updateTime
  if postChannelId new == currentChannelId st
    then updateChannelScrollState rs >>= updateViewed
    else return rs

addClientMessage :: ClientMessage -> ChatState -> EventM Name ChatState
addClientMessage msg st = do
  let cid = currentChannelId st
      st' = st & msgMap . ix cid . ccContents . cdMessages %~ (Seq.|> clientMessageToMessage msg)
  updateChannelScrollState st'

newClientMessage :: ClientMessageType -> T.Text -> EventM a ClientMessage
newClientMessage ty msg = do
  now <- liftIO getCurrentTime
  return (ClientMessage msg now ty)

getChannelName :: ChannelId -> ChatState -> T.Text
getChannelName cId st =
  st ^. msgMap . ix cId . ccInfo . cdName

getDMChannelName :: UserId -> UserId -> T.Text
getDMChannelName me you = cname
  where
  [loUser, hiUser] = sort [ you, me ]
  cname = idString loUser <> "__" <> idString hiUser

getChannel :: ChannelId -> ChatState -> Maybe ClientChannel
getChannel cId st = st ^. msgMap . at cId

execMMCommand :: T.Text -> ChatState -> EventM Name ChatState
execMMCommand cmd st = liftIO (runCmd `catch` handler)
  where
  mc = MinCommand
        { minComChannelId = currentChannelId st
        , minComCommand   = "/" <> cmd
        , minComSuggest   = False
        }
  runCmd = do
    _ <- mmExecute
      (st^.csConn)
      (st^.csTok)
      (st^.csMyTeam.teamIdL)
      mc
    return st
  handler (HTTPResponseException err) = do
    now <- liftIO getCurrentTime
    let msg = ClientMessage ("Error running command: " <> (T.pack err)) now Error
    runAsync st (return $ addClientMessage msg)
    return st

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

setupState :: Config -> RequestChan -> IO ChatState
setupState config requestChan = do
  -- If we don't have enough credentials, ask for them.
  (uStr, pStr) <- case (,) <$> configUser config <*> configPass config of
      Nothing -> interactiveGatherCredentials config
      Just (u, PasswordString p) -> return (u, p)
      _ -> error $ "BUG: unexpected password state: " <> show (configPass config)

  ctx <- initConnectionContext
  let cd = mkConnectionData (T.unpack (configHost config))
                            (fromIntegral (configPort config))
                            ctx

  let loginLoop (u, p) = do
        putStrLn "Authenticating..."

        let login = Login { username = u
                          , password = p
                          }
        result <- (Just <$> mmLogin cd login) `catch`
                  (\(_::SomeException) -> return Nothing)
        case result of
            Just (Right values) -> return values
            _ -> interactiveGatherCredentials config >>= loginLoop

  (token, myUser) <- loginLoop (uStr, pStr)

  initialLoad <- mmGetInitialLoad cd token
  when (null $ initialLoadTeams initialLoad) $ do
      putStrLn "Error: your account is not a member of any teams"
      exitFailure

  myTeam <- case configTeam config of
      Nothing -> do
          interactiveTeamSelection $ toList $ initialLoadTeams initialLoad
      Just tName -> do
          let matchingTeam = listToMaybe $ filter matches $ toList $ initialLoadTeams initialLoad
              matches t = teamName t == tName
          case matchingTeam of
              Nothing -> interactiveTeamSelection (toList (initialLoadTeams initialLoad))
              Just t -> return t

  let myTeamId = getId myTeam

  putStrLn $ "Loading channels for team " <> show (teamName myTeam) <> "..."

  Channels chans cm <- mmGetChannels cd token myTeamId

  let lookupChan n = [ c ^. channelIdL
                     | c <- toList chans
                     , c ^. channelNameL == n ]

  Chan.writeChan requestChan $ do
    statusMap <- mmGetStatuses cd token
    return $ \ appState ->
      return $ HM.foldrWithKey
                 (\ uId status st ->
                    st & usrMap.ix(uId).uiStatus .~ statusFromText status)
                 appState
                 statusMap

  msgs <- fmap (HM.fromList . toList) $ forM chans $ \c -> do
    let chanData = cm ! getId c
        viewed   = chanData ^. channelDataLastViewedAtL
        updated  = c ^. channelLastPostAtL
        cInfo    = ChannelInfo
                     { _cdViewed  = viewed
                     , _cdUpdated = updated
                     , _cdName    = c^.channelNameL
                     , _cdHeader  = c^.channelHeaderL
                     , _cdType    = c^.channelTypeL
                     , _cdLoaded  = False
                     }
        cChannel = ClientChannel
                     { _ccContents = emptyChannelContents
                     , _ccInfo     = cInfo
                     }
    when (c^.channelNameL /= "town-square") $ Chan.writeChan requestChan $ do
      posts <- mmGetPosts cd token myTeamId (getId c) 0 numScrollbackPosts
      return $ \ st -> do
          let st' = st & msgMap.ix(getId c).ccContents .~ fromPosts st posts
                       & msgMap.ix(getId c).ccInfo.cdLoaded .~ True
          updateChannelScrollState st'
    return (getId c, cChannel)

  users <- mmGetProfiles cd token myTeamId
  tz    <- getCurrentTimeZone
  hist  <- do
      result <- readHistory
      case result of
          Left _ -> return newHistory
          Right h -> return h

  startTimezoneMonitor tz requestChan

  let chanNames = MMNames
        { _cnChans = sort
                     [ channelName c
                     | c <- toList chans
                     , channelType c == "O"
                     ]
        , _cnDMs = sort
                   [ channelName c
                   | c <- toList chans
                   , channelType c == "D"
                   ]
        , _cnToChanId = HM.fromList $
                          [ (channelName c, channelId c)
                          | c <- toList chans
                          ] ++
                          [ (userProfileUsername u, c)
                          | u <- HM.elems users
                          , c <- lookupChan (getDMChannelName (getId myUser) (getId u))
                          ]
        , _cnUsers = sort (map userProfileUsername (HM.elems users))
        , _cnToUserId = HM.fromList
                          [ (userProfileUsername u, getId u)
                          | u <- HM.elems users
                          ]
        }
      Just townSqId = chanNames ^. cnToChanId . at "town-square"
      chanIds = [ (chanNames ^. cnToChanId) HM.! i
                | i <- chanNames ^. cnChans ] ++
                [ c
                | i <- chanNames ^. cnUsers
                , c <- maybeToList (HM.lookup i (chanNames ^. cnToChanId)) ]
      chanZip = Z.findRight (== townSqId) (Z.fromList chanIds)
      themeName = case configTheme config of
          Nothing -> defaultThemeName
          Just t -> t
      theme = case lookup themeName themes of
          Nothing -> fromJust $ lookup defaultThemeName themes
          Just t -> t
      st = newState token cd chanZip myUser myTeam tz (configTimeFormat config) hist requestChan theme
             & usrMap .~ fmap userInfoFromProfile users
             & msgMap .~ msgs
             & csNames .~ chanNames

  townSquarePosts <- mmGetPosts cd token myTeamId townSqId 0 numScrollbackPosts
  let st' = st & msgMap.ix(townSqId).ccContents .~ fromPosts st townSquarePosts
               & msgMap.ix(townSqId).ccInfo.cdLoaded .~ True

  updateViewedIO st'


debugPrintTimes :: ChatState -> T.Text -> EventM Name ChatState
debugPrintTimes st cn = do
  let Just cId = st^.csNames.cnToChanId.at(cn)
      Just ch = st^.msgMap.at(cId)
      viewed = ch^.ccInfo.cdViewed
      updated = ch^.ccInfo.cdUpdated
  m1 <- newClientMessage Informative $ T.pack ("Viewed: " ++ show viewed)
  m2 <- newClientMessage Informative $ T.pack ("Updated: " ++ show updated)
  addClientMessage m1 st >>= addClientMessage m2
