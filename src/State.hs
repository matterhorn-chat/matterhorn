{-# LANGUAGE OverloadedStrings #-}
module State where

import           Brick (EventM)
import           Brick.Widgets.Edit (getEditContents, editContentsL)
import           Brick.Widgets.List (list)
import           Control.Applicative
import           Control.Concurrent (threadDelay, forkIO)
import qualified Control.Concurrent.Chan as Chan
import           Control.Concurrent.MVar (newEmptyMVar)
import           Control.Monad.IO.Class (MonadIO, liftIO)
import           Data.Char (isAlphaNum)
import           Data.HashMap.Strict ((!))
import           Brick.Main (getVtyHandle, viewportScroll, vScrollToEnd, vScrollToBeginning, vScrollBy)
import           Brick.Widgets.Edit (applyEdit)
import           Control.Exception (SomeException, catch, try)
import           Control.Monad (forM, when, void)
import           Data.Text.Zipper (textZipper, clearZipper, insertMany, gotoEOL)
import qualified Data.HashMap.Strict as HM
import qualified Data.Sequence as Seq
import           Data.List (sort)
import           Data.Maybe (listToMaybe, maybeToList, fromJust, catMaybes)
import           Data.Monoid ((<>))
import           Data.Time.Clock ( getCurrentTime )
import           Data.Time.LocalTime ( TimeZone(..), getCurrentTimeZone )
import qualified Data.Text as T
import qualified Data.Vector as V
import qualified Data.Foldable as F
import           Graphics.Vty (outputIface)
import           Graphics.Vty.Output.Interface (ringTerminalBell)
import           Lens.Micro.Platform
import           System.Exit (exitFailure)
import           System.IO (Handle)
import           System.Process (system)
import           Cheapskate

import           Prelude

import           Network.Mattermost
import           Network.Mattermost.Exceptions
import           Network.Mattermost.Lenses
import           Network.Mattermost.Logging (mmLoggerDebug)

import           Config
import           Types
import           TeamSelect
import           InputHistory
import           Themes
import           Login
import           Zipper (Zipper)
import qualified Zipper as Z

pageAmount :: Int
pageAmount = 15

fromPosts :: ChatState -> Posts -> ChannelContents
fromPosts st p = ChannelContents $ messagesFromPosts st p

messagesFromPosts :: ChatState -> Posts -> Seq.Seq Message
messagesFromPosts st p = msgs
    where
        postMap :: HM.HashMap PostId Message
        postMap = HM.fromList [ ( pId
                                , clientPostToMessage st (toClientPost x Nothing)
                                )
                              | (pId, x) <- HM.toList (p^.postsPostsL)
                              ]
        st' = st & csPostMap %~ (HM.union postMap)
        msgs = clientPostToMessage st' <$> clientPost <$> ps
        ps   = findPost <$> (Seq.reverse $ postsOrder p)
        clientPost :: Post -> ClientPost
        clientPost x = toClientPost x (postId <$> parent x)
        parent x = do
            parentId <- x^.postParentIdL
            HM.lookup parentId (p^.postsPostsL)
        findPost pId = case HM.lookup pId (postsPosts p) of
            Nothing -> error $ "BUG: could not find post for post ID " <> show pId
            Just post -> post

numScrollbackPosts :: Int
numScrollbackPosts = 100

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
  , _csChannelSelectString         = ""
  , _csChannelSelectChannelMatches = mempty
  , _csChannelSelectUserMatches    = mempty
  , _csRecentChannel               = Nothing
  , _csConnectionStatus            = Disconnected
  , _csJoinChannelList             = Nothing
  }

runAsync :: ChatState -> IO (ChatState -> EventM Name ChatState) -> IO ()
runAsync st thunk =
  Chan.writeChan (st^.csRequestQueue) thunk

doAsync :: ChatState -> IO () -> IO ()
doAsync st thunk = doAsyncWith st (thunk >> return return)

doAsyncWith :: ChatState -> IO (ChatState -> EventM Name ChatState) -> IO ()
doAsyncWith st thunk =
  Chan.writeChan (st^.csRequestQueue) thunk

startJoinChannel :: ChatState -> EventM Name ChatState
startJoinChannel st = do
    liftIO $ doAsyncWith st $ do
        MoreChannels chans <- mmGetMoreChannels (st^.csConn) (st^.csTok) (st^.csMyTeam.teamIdL)
        return $ \ st' -> do
            return $ st' & csJoinChannelList .~ (Just $ list JoinChannelList (V.fromList $ F.toList chans) 1)

    return $ st & csMode .~ JoinChannel
                & csJoinChannelList .~ Nothing

joinChannel :: Channel -> ChatState -> EventM Name ChatState
joinChannel chan st = do
    let cId = getId chan
    liftIO $ do
        mmJoinChannel (st^.csConn) (st^.csTok) (st^.csMyTeam.teamIdL) cId
        asyncFetchScrollback st cId
    handleNewChannel (chan^.channelNameL) chan $ st & csMode .~ Main

asyncFetchScrollback :: ChatState -> ChannelId -> IO ()
asyncFetchScrollback st cId =
    doAsyncWith st $ do
        posts <- mmGetPosts (st^.csConn) (st^.csTok) (st^.csMyTeam.teamIdL) cId 0 numScrollbackPosts
        return $ \st' ->
            updateChannelScrollState $
                st' & msgMap.ix cId.ccContents .~ fromPosts st' posts
                    & msgMap.ix cId.ccInfo.cdLoaded .~ True

startLeaveCurrentChannel :: ChatState -> EventM Name ChatState
startLeaveCurrentChannel st = do
    let cId = currentChannelId st
        Just chanInfo = getChannel cId st
        cName = chanInfo^.ccInfo.cdName
    case cName `elem` st^.csNames.cnDMs of
        True -> postErrorMessage "The /leave command cannot be used with direct message channels." st
        False -> return $ st & csMode .~ LeaveChannelConfirm

leaveCurrentChannel :: ChatState -> EventM Name ChatState
leaveCurrentChannel st = do
    let cId = currentChannelId st
        Just chanInfo = getChannel cId st
        cName = chanInfo^.ccInfo.cdName
    -- Leave a normal channel.  If this is a DM channel, do nothing.
    case cName `elem` st^.csNames.cnDMs of
        True -> return st
        False -> do
            -- Issue API call to leave. Once that's done, clean up our state:
            liftIO $ doAsyncWith st $ do
                mmLeaveChannel (st^.csConn) (st^.csTok) (st^.csMyTeam.teamIdL) cId
                return $ \ st' -> do
                    let st'' = st' & csEditState.cedInputHistoryPosition       .at cId .~ Nothing
                                   & csEditState.cedLastChannelInput           .at cId .~ Nothing
                                   & csEditState.cedInputHistory               %~ removeChannelHistory cId
                                     -- ^ Update input history
                                   & csNames.cnToChanId                        .at cName .~ Nothing
                                     -- ^ Flush cnToChanId
                                   & csNames.cnChans                           %~ filter (/= cName)
                                     -- ^ Flush cnChans
                                   & msgMap                                    .at cId .~ Nothing
                                     -- ^ Update msgMap
                                   & csFocus                                   %~ Z.filterZipper (/= cId)
                                     -- ^ Remove from focus zipper
                    updateChannelScrollState st''
            return st

hasUnread :: ChatState -> ChannelId -> Bool
hasUnread st cId = maybe False id $ do
  chan <- st^.msgMap.at(cId)
  let u = chan^.ccInfo.cdViewed
      v = chan^.ccInfo.cdUpdated
  return (v > u)

setLastViewedFor :: ChatState -> ChannelId -> EventM Name ChatState
setLastViewedFor st cId = do
  now <- liftIO getCurrentTime
  return (st & msgMap.ix(cId).ccInfo.cdViewed .~ now)

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

updateStatus :: UserId -> T.Text -> ChatState -> EventM a ChatState
updateStatus uId t st =
  return (st & usrMap.ix(uId).uiStatus .~ statusFromText t)

clearEditor :: ChatState -> EventM a ChatState
clearEditor st = return $ st & cmdLine %~ applyEdit clearZipper

loadLastEdit :: ChatState -> EventM a ChatState
loadLastEdit st =
    let cId = currentChannelId st
    in return $ case st^.csLastChannelInput.at cId of
        Nothing -> st
        Just lastEdit -> st & cmdLine %~ (applyEdit $ insertMany (lastEdit))

changeChannelCommon :: ChatState -> EventM Name ChatState
changeChannelCommon st =
    loadLastEdit =<<
    clearEditor =<<
    updateChannelScrollState =<<
    fetchCurrentScrollback =<<
    resetHistoryPosition st

preChangeChannelCommon :: ChatState -> EventM Name ChatState
preChangeChannelCommon st = do
    let curEdit = T.intercalate "\n" $ getEditContents $ st^.cmdLine
        cId = currentChannelId st
    return $ st & csLastChannelInput.at cId .~ Just curEdit
                & csRecentChannel .~ Just cId

nextChannel :: ChatState -> EventM Name ChatState
nextChannel st =
    setFocusWith st (getNextChannel st Z.right) >>= updateChannelScrollState

prevChannel :: ChatState -> EventM Name ChatState
prevChannel st =
    setFocusWith st (getNextChannel st Z.left) >>= updateChannelScrollState

recentChannel :: ChatState -> EventM Name ChatState
recentChannel st = case st ^. csRecentChannel of
  Nothing  -> return st
  Just cId -> setFocus cId st >>= updateChannelScrollState

nextUnreadChannel :: ChatState -> EventM Name ChatState
nextUnreadChannel st =
    setFocusWith st (getNextUnreadChannel st) >>= updateChannelScrollState

getNextChannel :: ChatState
               -> (Zipper ChannelId -> Zipper ChannelId)
               -> (Zipper ChannelId -> Zipper ChannelId)
getNextChannel st shift z = go (shift z)
  where go z'
          | (st^?msgMap.ix(Z.focus z').ccInfo.cdType) /= Just Direct = z'
          | otherwise = go (shift z')

getNextUnreadChannel :: ChatState
                     -> (Zipper ChannelId -> Zipper ChannelId)
getNextUnreadChannel st = Z.findRight (hasUnread st)

listThemes :: ChatState -> EventM Name ChatState
listThemes cs = do
    let mkThemeList _ = T.intercalate "\n\n" $
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
  vScrollBy (viewportScroll (ChannelMessages cId)) (-1 * pageAmount)
  return st

channelPageDown :: ChatState -> EventM Name ChatState
channelPageDown st = do
  let cId = currentChannelId st
  vScrollBy (viewportScroll (ChannelMessages cId)) pageAmount
  return st

currentChannelId :: ChatState -> ChannelId
currentChannelId st = Z.focus (st ^. csFocus)

channelByName :: ChatState -> T.Text -> Maybe ChannelId
channelByName st n
    | "#" `T.isPrefixOf` n = st ^. csNames . cnToChanId . at (T.tail n)
    | "@" `T.isPrefixOf` n = st ^. csNames . cnToChanId . at (T.tail n)
    | otherwise            = st ^. csNames . cnToChanId . at n

-- | This switches to the named channel or creates it if it is a missing
-- but valid user channel.
changeChannel :: T.Text -> ChatState -> EventM Name ChatState
changeChannel name st =
    case channelByName st name of
      Just cId -> setFocus cId st
      Nothing -> attemptCreateDMChannel name st

setFocus :: ChannelId -> ChatState -> EventM Name ChatState
setFocus cId st = setFocusWith st (Z.findRight (== cId))

setFocusWith :: ChatState -> (Zipper ChannelId -> Zipper ChannelId) -> EventM Name ChatState
setFocusWith st f = do
    let newZipper = f oldZipper
        oldZipper = st^.csFocus
        newFocus = Z.focus newZipper
        oldFocus = Z.focus oldZipper

    -- If we aren't changing anything, skip all the book-keeping because
    -- we'll end up clobbering things like csRecentChannel.
    if (newFocus == oldFocus) then
        return st else do
          preChangeChannelCommon st >>=
              (\st' -> updateViewed (st' & csFocus .~ newZipper)) >>=
              changeChannelCommon

attemptCreateDMChannel :: T.Text -> ChatState -> EventM Name ChatState
attemptCreateDMChannel name st
  | name `elem` (st^.csNames.cnUsers) &&
    not (name `HM.member` (st^.csNames.cnToChanId)) = do
      -- We have a user of that name but no channel. Time to make one!
      let tId = st^.csMyTeam.teamIdL
          Just uId = st^.csNames.cnToUserId.at(name)
      liftIO $ runAsync st $ do
        -- create a new channel
        nc <- mmCreateDirect (st^.csConn) (st^.csTok) tId uId
        return $ handleNewChannel name nc
      return st
  | otherwise = do
    postErrorMessage ("No channel or user named " <> name) st

tryMM :: (MonadIO m)
      => IO a
      -- ^ The action to try (usually a MM API call)
      -> (a -> IO (ChatState -> m ChatState))
      -- ^ What to do on success
      -> IO (ChatState -> m ChatState)
tryMM act onSuccess = do
    result <- liftIO $ try act
    case result of
        Left (MattermostServerError msg) -> return $ postErrorMessage msg
        Right value                      -> liftIO $ onSuccess value

createOrdinaryChannel :: T.Text -> ChatState -> EventM Name ChatState
createOrdinaryChannel name st = do
  let tId = st^.csMyTeam.teamIdL
  liftIO $ runAsync st $ do
    -- create a new chat channel
    let slug = T.map (\ c -> if isAlphaNum c then c else '-') (T.toLower name)
        minChannel = MinChannel
          { minChannelName        = slug
          , minChannelDisplayName = name
          , minChannelPurpose     = Nothing
          , minChannelHeader      = Nothing
          , minChannelType        = Ordinary
          }
    tryMM (mmCreateChannel (st^.csConn) (st^.csTok) tId minChannel)
          (return . handleNewChannel name)
  return st

handleNewChannel :: T.Text -> Channel -> ChatState -> EventM Name ChatState
handleNewChannel name nc st = do
  -- time to do a lot of state updating:
  -- create a new ClientChannel structure
  now <- liftIO getCurrentTime
  let cChannel = ClientChannel
        { _ccContents = emptyChannelContents
        , _ccInfo     = ChannelInfo
                          { _cdViewed  = now
                          , _cdUpdated = now
                          , _cdName    = nc^.channelNameL
                          , _cdHeader  = nc^.channelHeaderL
                          , _cdType    = nc^.channelTypeL
                          , _cdLoaded  = True
                          }
        }
      -- add it to the message map, and to the map so we can look
      -- it up by user name
      st' = st & csNames.cnToChanId.at(name) .~ Just (getId nc)
               & (if nc^.channelTypeL == Direct
                  then id -- For direct channels the username is already
                          -- in the user list so do nothing
                  else csNames.cnChans %~ (sort . (name:)))
               & msgMap.at(getId nc) .~ Just cChannel
      -- we should figure out how to do this better: this adds it to
      -- the channel zipper in such a way that we don't ever change
      -- our focus to something else, which is kind of silly
      newZip = Z.updateList (mkChannelZipperList (st'^.csNames))
      st'' = st' & csFocus %~ newZip
          -- and we finally set our focus to the newly created channel
  setFocus (getId nc) st''

editMessage :: Post -> ChatState -> EventM a ChatState
editMessage new st = do
  now <- liftIO getCurrentTime
  let chan = msgMap . ix (postChannelId new)
      isEditedMessage m = m^.mPostId == Just (new^.postIdL)
      msg = clientPostToMessage st (toClientPost new Nothing)
      rs = st & chan . ccContents . cdMessages . each . filtered isEditedMessage .~ msg
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

maybeRingBell :: ChatState -> EventM Name ()
maybeRingBell st = do
    when (configActivityBell $ st^.csResources.crConfiguration) $ do
        -- This is safe because we only get Nothing in appStartEvent.
        Just vty <- getVtyHandle
        liftIO $ ringTerminalBell $ outputIface vty

addMessage :: Post -> ChatState -> EventM Name ChatState
addMessage new st = do
  now <- liftIO getCurrentTime
  let cp = toClientPost new Nothing
      fromMe = cp^.cpUser == getId (st^.csMe)
      updateTime = if fromMe then id else const now
  let chan = msgMap . ix (postChannelId new)
      st' = st & csPostMap.ix(postId new) .~ msg
      msg = clientPostToMessage st' (toClientPost new (new^.postParentIdL))
      rs = st' & chan . ccContents . cdMessages %~ (Seq.|> msg)
               & chan . ccInfo . cdUpdated %~ updateTime
  when (not fromMe) $ maybeRingBell st
  if postChannelId new == currentChannelId st
    then updateChannelScrollState rs >>= updateViewed
    else return rs

addClientMessage :: ClientMessage -> ChatState -> EventM Name ChatState
addClientMessage msg st = do
  let cid = currentChannelId st
      st' = st & msgMap . ix cid . ccContents . cdMessages %~ (Seq.|> clientMessageToMessage msg)
  updateChannelScrollState st'

newClientMessage :: (MonadIO m) => ClientMessageType -> T.Text -> m ClientMessage
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

mmServerCommandWhitelist :: [T.Text]
mmServerCommandWhitelist =
    [ "me"
    ]

execMMCommand :: T.Text -> ChatState -> EventM Name ChatState
execMMCommand cmd st =
    case T.words cmd of
        (n:_) -> case n `elem` mmServerCommandWhitelist of
            False -> postErrorMessage ("Unknown command: " <> n) st
            True -> liftIO (runCmd `catch` handler)
        _ -> postErrorMessage ("Invalid command: " <> cmd) st
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
    postErrorMessage ("Error running command: " <> (T.pack err)) st

postErrorMessage :: (MonadIO m) => T.Text -> ChatState -> m ChatState
postErrorMessage err st = do
    msg <- newClientMessage Error err
    liftIO $ runAsync st (return $ addClientMessage msg)
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

fetchCurrentScrollback :: ChatState -> EventM a ChatState
fetchCurrentScrollback st = do
  let cId = currentChannelId st
  when (maybe False not (st^?msgMap.ix(cId).ccInfo.cdLoaded)) $
      liftIO $ asyncFetchScrollback st cId
  return st

mkChannelZipperList :: MMNames -> [ChannelId]
mkChannelZipperList chanNames =
  [ (chanNames ^. cnToChanId) HM.! i
  | i <- chanNames ^. cnChans ] ++
  [ c
  | i <- chanNames ^. cnUsers
  , c <- maybeToList (HM.lookup i (chanNames ^. cnToChanId)) ]

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

setupState :: Maybe Handle -> Config -> RequestChan -> Chan.Chan Event -> IO ChatState
setupState logFile config requestChan eventChan = do
  -- If we don't have enough credentials, ask for them.
  (uStr, pStr) <- case getCredentials config of
      Nothing -> interactiveGatherCredentials config
      Just (u, p) -> return (u, p)

  let setLogger = case logFile of
        Nothing -> id
        Just f  -> \ cd -> cd `withLogger` mmLoggerDebug f

  cd <- setLogger `fmap`
          initConnectionData (T.unpack (configHost config))
                             (fromIntegral (configPort config))

  let loginLoop (u, p) = do
        putStrLn "Authenticating..."

        let login = Login { username = u, password = p }
        result <- (Just <$> mmLogin cd login) `catch`
                  (\(_::SomeException) -> return Nothing)
        case result of
            Just (Right values) -> return values
            _ -> interactiveGatherCredentials config >>= loginLoop

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
  Channels chans cm <- mmGetChannels cd token myTeamId

  msgs <- fmap (HM.fromList . F.toList) $ forM (F.toList chans) $ \c -> do
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

setChannelTopic :: ChatState -> T.Text -> IO ()
setChannelTopic st msg = do
    let chanId = currentChannelId st
        theTeamId = st^.csMyTeam.teamIdL
    doAsyncWith st $ do
        void $ mmSetChannelHeader (st^.csConn) (st^.csTok) theTeamId chanId msg
        return $ \st' -> do
            return $ st' & msgMap.at chanId.each.ccInfo.cdHeader .~ msg

channelHistoryForward :: ChatState -> ChatState
channelHistoryForward st =
  let cId = currentChannelId st
  in case st^.csInputHistoryPosition.at cId of
      Just (Just i)
        | i == 0 ->
          -- Transition out of history navigation
          st & cmdLine %~ applyEdit clearZipper
             & csInputHistoryPosition.at cId .~ Just Nothing
        | otherwise ->
          let Just entry = getHistoryEntry cId newI (st^.csInputHistory)
              newI = i - 1
          in st & cmdLine.editContentsL .~ (gotoEOL $ textZipper [entry] (Just 1))
                & csInputHistoryPosition.at cId .~ (Just $ Just newI)
      _ -> st

channelHistoryBackward :: ChatState -> ChatState
channelHistoryBackward st =
  let cId = currentChannelId st
  in case st^.csInputHistoryPosition.at cId of
      Just (Just i) ->
          let newI = i + 1
          in case getHistoryEntry cId newI (st^.csInputHistory) of
              Nothing -> st
              Just entry ->
                  st & cmdLine.editContentsL .~ (gotoEOL $ textZipper [entry] (Just 1))
                     & csInputHistoryPosition.at cId .~ (Just $ Just newI)
      _ ->
          let newI = 0
          in case getHistoryEntry cId newI (st^.csInputHistory) of
              Nothing -> st
              Just entry ->
                  st & cmdLine.editContentsL .~ (gotoEOL $ textZipper [entry] (Just 1))
                     & csInputHistoryPosition.at cId .~ (Just $ Just newI)

showHelpScreen :: ChatState -> EventM Name ChatState
showHelpScreen st = do
    vScrollToBeginning (viewportScroll HelpViewport)
    return $ st & csMode .~ ShowHelp

beginChannelSelect :: ChatState -> EventM Name ChatState
beginChannelSelect st =
    return $ st & csMode                        .~ ChannelSelect
                & csChannelSelectString         .~ ""
                & csChannelSelectChannelMatches .~ mempty
                & csChannelSelectUserMatches    .~ mempty

updateChannelSelectMatches :: ChatState -> ChatState
updateChannelSelectMatches st =
    -- Given the current channel select string, find all the channel and
    -- user matches and then update the match lists.
    let chanNameMatches = channelNameMatch (st^.csChannelSelectString)
        chanMatches = catMaybes $ chanNameMatches <$> st^.csNames.cnChans
        userMatches = catMaybes $ chanNameMatches <$> st^.csNames.cnUsers
        mkMap ms = HM.fromList [(channelNameFromMatch m, m) | m <- ms]
    in st & csChannelSelectChannelMatches .~ mkMap chanMatches
          & csChannelSelectUserMatches    .~ mkMap userMatches

channelNameMatch :: T.Text -> T.Text -> Maybe ChannelSelectMatch
channelNameMatch patStr chanName =
    if T.null patStr
    then Nothing
    else do
        pat <- parseChannelSelectPattern patStr
        applySelectPattern pat chanName

applySelectPattern :: ChannelSelectPattern -> T.Text -> Maybe ChannelSelectMatch
applySelectPattern (CSP ty pat) chanName = do
    let applyType Infix  | pat `T.isInfixOf`  chanName =
            case T.breakOn pat chanName of
                (pre, post) -> return (pre, pat, T.drop (T.length pat) post)

        applyType Prefix | pat `T.isPrefixOf` chanName = do
            let (b, a) = T.splitAt (T.length pat) chanName
            return ("", b, a)

        applyType Suffix | pat `T.isSuffixOf` chanName = do
            let (b, a) = T.splitAt (T.length chanName - T.length pat) chanName
            return (b, a, "")

        applyType Equal  | pat == chanName =
            return ("", chanName, "")

        applyType _ = Nothing

    (pre, m, post) <- applyType ty
    return $ ChannelSelectMatch pre m post

parseChannelSelectPattern :: T.Text -> Maybe ChannelSelectPattern
parseChannelSelectPattern pat = do
    (pat1, pfx) <- case "^" `T.isPrefixOf` pat of
        True  -> return (T.tail pat, Just Prefix)
        False -> return (pat, Nothing)

    (pat2, sfx) <- case "$" `T.isSuffixOf` pat1 of
        True  -> return (T.init pat1, Just Suffix)
        False -> return (pat1, Nothing)

    case (pfx, sfx) of
        (Nothing, Nothing)         -> return $ CSP Infix  pat2
        (Just Prefix, Nothing)     -> return $ CSP Prefix pat2
        (Nothing, Just Suffix)     -> return $ CSP Suffix pat2
        (Just Prefix, Just Suffix) -> return $ CSP Equal  pat2
        tys                        -> error $ "BUG: invalid channel select case: " <> show tys

openMostRecentURL :: ChatState -> EventM Name ChatState
openMostRecentURL st =
    case configURLOpenCommand $ st^.csResources.crConfiguration of
        Nothing -> do
            msg <- newClientMessage Informative "Config option 'urlOpenCommand' missing; cannot open URL."
            addClientMessage msg st
        Just urlOpenCommand -> do
            -- Get the messages for the current channel
            let cId = currentChannelId st
                chan = msgMap . ix cId
                msgs = st ^. chan . ccContents . cdMessages

                msgURLs :: Message -> Seq.Seq T.Text
                msgURLs msg = mconcat $ blockGetURLs <$> (F.toList $ msg^.mText)

                blockGetURLs :: Block -> Seq.Seq T.Text
                blockGetURLs (Para is) = mconcat $ inlineGetURLs <$> F.toList is
                blockGetURLs (Header _ is) = mconcat $ inlineGetURLs <$> F.toList is
                blockGetURLs (Blockquote bs) = mconcat $ blockGetURLs <$> F.toList bs
                blockGetURLs (List _ _ bss) = mconcat $ mconcat $ (blockGetURLs <$>) <$> (F.toList <$> bss)
                blockGetURLs _ = mempty

                inlineGetURLs :: Inline -> Seq.Seq T.Text
                inlineGetURLs (Emph is) = mconcat $ inlineGetURLs <$> F.toList is
                inlineGetURLs (Strong is) = mconcat $ inlineGetURLs <$> F.toList is
                inlineGetURLs (Link is url "") = url Seq.<| (mconcat $ inlineGetURLs <$> F.toList is)
                inlineGetURLs (Link is _ url) = url Seq.<| (mconcat $ inlineGetURLs <$> F.toList is)
                inlineGetURLs (Image is _ _) = mconcat $ inlineGetURLs <$> F.toList is
                inlineGetURLs _ = mempty

                -- Search from the most recent message backwards until we find
                -- one with one or more URLs
                recentIdx = Seq.findIndexR (not . Seq.null . msgURLs) msgs

            case recentIdx of
                Nothing -> return ()
                Just i -> do
                    -- For each URL in the message, invoke the configured "url
                    -- open command" if we have one
                    --
                    -- If no open command is available, show an error message.
                    let Just msg = msgs Seq.!? i
                    F.forM_ (msgURLs msg) $ \url -> do
                        liftIO $ void $ system $ (T.unpack urlOpenCommand) <> " " <> show url

            return st
