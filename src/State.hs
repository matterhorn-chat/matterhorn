module State where

import           Brick (EventM, str, vBox)
import           Brick.Widgets.Edit (editor)
import qualified Control.Concurrent.Chan as Chan
import           Control.Monad.IO.Class (liftIO)
import           Data.HashMap.Strict ((!))
import           Brick.Main (viewportScroll, vScrollToEnd)
import           Brick.Widgets.Edit (applyEdit)
import           Control.Exception (catch)
import           Control.Monad (join, forM, when)
import           Data.Text.Zipper (clearZipper)
import qualified Data.HashMap.Strict as HM
import           Data.List (sort)
import           Data.Maybe (listToMaybe, maybeToList)
import           Data.Monoid ((<>))
import           Data.Time.Clock ( UTCTime, getCurrentTime )
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
import           Zipper (Zipper)
import qualified Zipper as Z

fromPosts :: Posts -> ChannelContents
fromPosts p = ChannelContents
  { _cdOrder   = map MMId (p ^. postsOrderL)
  , _cdPosts   = fmap toClientPost (p ^. postsPostsL)
  , _cdCMsgs   = HM.empty
  }

newState :: Token
         -> ConnectionData
         -> Zipper ChannelId
         -> User
         -> Team
         -> TimeZone
         -> Maybe String
         -> InputHistory
         -> RequestChan
         -> ChatState
newState t c i u m tz fmt hist rq = ChatState
  { _csTok    = t
  , _csConn   = c
  , _csFocus  = i
  , _csMe     = u
  , _csMyTeam = m
  , _csNames  = MMNames [] [] HM.empty [] HM.empty
  , _msgMap   = HM.empty
  , _usrMap   = HM.empty
  , _cmdLine  = editor MessageInput (vBox . map str) (Just 1) ""
  , _timeZone = tz
  , _csRequestQueue = rq
  , _timeFormat = fmt
  , _csInputHistory = hist
  , _csInputHistoryPosition = mempty
  , _csCurrentCompletion = Nothing
  }

runAsync :: ChatState -> IO (ChatState -> ChatState) -> IO ()
runAsync st thunk =
  Chan.writeChan (st^.csRequestQueue) thunk

doAsync :: ChatState -> IO () -> IO ()
doAsync st thunk =
  Chan.writeChan (st^.csRequestQueue) (thunk >> return id)

hasUnread :: ChatState -> ChannelId -> Bool
hasUnread st cId = maybe False id $ do
  chan <- st^.msgMap.at(cId)
  let u = chan^.ccInfo.cdViewed
      v = chan^.ccInfo.cdUpdated
  return (v > u)
  where

updateViewed :: ChatState -> EventM a ChatState
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
    return (msgMap . ix cId . ccInfo . cdViewed .~ now)
  return st

resetHistoryPosition :: ChatState -> EventM a ChatState
resetHistoryPosition st =
    let cId = currentChannelId st
    in return $ st & csInputHistoryPosition.at cId .~ Just Nothing

clearEditor :: ChatState -> EventM a ChatState
clearEditor st = return $ st & cmdLine %~ applyEdit clearZipper

changeChannelCommon :: ChatState -> EventM a ChatState
changeChannelCommon st =
    clearEditor =<<
    resetHistoryPosition st

nextChannel :: ChatState -> EventM a ChatState
nextChannel st = changeChannelCommon =<<
                 updateViewed (st & csFocus %~ Z.right)

prevChannel :: ChatState -> EventM a ChatState
prevChannel st = changeChannelCommon =<<
                 updateViewed (st & csFocus %~ Z.left)

updateChannelScrollState :: ChatState -> EventM Name ChatState
updateChannelScrollState st = do
  let cId = currentChannelId st
  vScrollToEnd $ viewportScroll (ChannelMessages cId)
  return st

currentChannelId :: ChatState -> ChannelId
currentChannelId st = Z.focus (st ^. csFocus)

channelExists :: ChatState -> String -> Bool
channelExists st n = n `elem` st ^. csNames . cnChans

userExists :: ChatState -> String -> Bool
userExists st n = n `elem` st ^. csNames . cnUsers

setFocus :: String -> ChatState -> EventM a ChatState
setFocus n st = updateViewed (st & csFocus %~ Z.findRight (==n'))
  where
  Just n' = st ^. csNames . cnToChanId . at n

setDMFocus :: String -> ChatState -> EventM a ChatState
setDMFocus n st =
    case n' of
        Nothing -> return st
        Just dmName -> updateViewed (st & csFocus %~ Z.findRight (==dmName))
  where
  n' = st ^. csNames . cnToChanId . at n

editMessage :: Post -> ChatState -> EventM a ChatState
editMessage new st = do
  now <- liftIO getCurrentTime
  let chan = msgMap . ix (postChannelId new)
      rs = st & chan . ccContents . cdPosts . ix (getId new) . cpText .~ postMessage new
              & chan . ccInfo . cdUpdated .~ now
  return rs

addMessage :: Post -> ChatState -> EventM a ChatState
addMessage new st = do
  now <- liftIO getCurrentTime
  let cp = toClientPost new
  let chan = msgMap . ix (postChannelId new)
      rs = st & chan . ccContents . cdPosts . at (getId new) .~ Just cp
              & chan . ccContents . cdOrder %~ (MMId (getId new) :)
              & chan . ccInfo . cdUpdated .~ now
  if postChannelId new == currentChannelId st
    then updateViewed rs
    else return rs

-- XXX: Right now, our new ID is based on the size of the map containing all
-- the ClientMessages, which only makes sense if we never delete ClientMessages.
-- We should probably figure out a better way of choosing IDs.
addClientMessage :: ClientMessage -> ChatState -> ChatState
addClientMessage msg st =
  let n = HM.size (st ^. msgMap . ix cid . ccContents . cdCMsgs) + 1
      cid = currentChannelId st
  in st & msgMap . ix cid . ccContents . cdCMsgs . at n .~ Just msg
        & msgMap . ix cid . ccContents . cdOrder %~ (CLId n :)

mmMessageDigest :: ChannelId -> PostId -> ChatState -> (UTCTime, String, String, Bool)
mmMessageDigest cId ref st =
  ( p^.cpDate, userProfileUsername (us ! (p^.cpUser)), p^.cpText, p^.cpIsEmote )
  where p = ((ms ! cId) ^. ccContents . cdPosts) ! ref
        ms = st ^. msgMap
        us = st ^. usrMap

newClientMessage :: String -> EventM a ClientMessage
newClientMessage msg = do
  now <- liftIO getCurrentTime
  return (ClientMessage msg now)

clientMessageDigest :: ChannelId -> Int -> ChatState -> (UTCTime, String, String, Bool)
clientMessageDigest cId ref st =
  ( m ^. cmDate, "*matterhorn", m ^. cmText, False )
  where m = ((ms ! cId) ^. ccContents . cdCMsgs) ! ref
        ms = st ^. msgMap

getMessageListing :: ChannelId -> ChatState -> [(UTCTime, String, String, Bool)]
getMessageListing cId st =
  let is    = st ^. msgMap . ix cId . ccContents . cdOrder
  in reverse
    [ msg
    | i <- is
    , let msg = case i of
                  CLId c -> clientMessageDigest cId c st
                  MMId pId -> mmMessageDigest cId pId st
    ]

getChannelName :: ChannelId -> ChatState -> String
getChannelName cId st =
  st ^. msgMap . ix cId . ccInfo . cdName

getDMChannelName :: UserId -> UserId -> String
getDMChannelName me you = cname
  where
  [loUser, hiUser] = sort [ you, me ]
  cname = idString loUser ++ "__" ++ idString hiUser

getChannel :: ChannelId -> ChatState -> Maybe ClientChannel
getChannel cId st = st ^. msgMap . at cId

execMMCommand :: String -> ChatState -> EventM a ChatState
execMMCommand cmd st = liftIO (runCmd `catch` handler)
  where
  mc = MinCommand
        { minComChannelId = currentChannelId st
        , minComCommand   = "/" ++ cmd
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
    let msg = ClientMessage ("Error running command: " ++ err) now
    return (addClientMessage msg st)

setupState :: Config -> RequestChan -> IO ChatState
setupState config requestChan = do
  putStrLn "Authenticating..."

  ctx <- initConnectionContext
  let cd = mkConnectionData (T.unpack (configHost config))
                            (fromIntegral (configPort config))
                            ctx
      PasswordString pass = configPass config
      login = Login { username = configUser config
                    , password = pass
                    }

  (token, myUser) <- join (hoistE <$> mmLogin cd login)

  initialLoad <- mmGetInitialLoad cd token
  when (null $ initialLoadTeams initialLoad) $ do
      putStrLn "Error: your account is not a member of any teams"
      exitFailure

  myTeam <- case configTeam config of
      Nothing -> do
          interactiveTeamSelection (initialLoadTeams initialLoad)
      Just tName -> do
          let matchingTeam = listToMaybe $ filter matches $ initialLoadTeams initialLoad
              matches t = teamName t == (T.unpack tName)
          case matchingTeam of
              Nothing -> interactiveTeamSelection (initialLoadTeams initialLoad)
              Just t -> return t

  let myTeamId = getId myTeam

  putStrLn $ "Loading channels for team " <> show (teamName myTeam) <> "..."

  Channels chans cm <- mmGetChannels cd token myTeamId

  let lookupChan n = [ c ^. channelIdL
                     | c <- chans
                     , c ^. channelNameL == n ]

  msgs <- fmap HM.fromList $ forM chans $ \c -> do
    posts <- mmGetPosts cd token myTeamId (getId c) 0 30
    let chanData = cm ! getId c
        viewed   = chanData ^. channelDataLastViewedAtL
        updated  = c ^. channelLastPostAtL
        cInfo    = ChannelInfo
                     { _cdViewed  = viewed
                     , _cdUpdated = updated
                     , _cdName    = c^.channelNameL
                     , _cdPurpose = c^.channelPurposeL
                     , _cdType    = c^.channelTypeL
                     }
        cChannel = ClientChannel
                     { _ccContents = fromPosts posts
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

  let chanNames = MMNames
        { _cnChans = sort
                     [ channelName c
                     | c <- chans
                     , channelType c == "O"
                     ]
        , _cnDMs = sort
                   [ channelName c
                   | c <- chans
                   , channelType c == "D"
                   ]
        , _cnToChanId = HM.fromList $
                          [ (channelName c, channelId c)
                          | c <- chans
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
      st = newState token cd chanZip myUser myTeam tz (configTimeFormat config) hist requestChan
             & usrMap .~ users
             & msgMap .~ msgs
             & csNames .~ chanNames

  updateViewedIO st


debugPrintTimes :: ChatState -> String -> EventM a ChatState
debugPrintTimes st cn = do
  let Just cId = st^.csNames.cnToChanId.at(cn)
      Just ch = st^.msgMap.at(cId)
      viewed = ch^.ccInfo.cdViewed
      updated = ch^.ccInfo.cdUpdated
  m1 <- newClientMessage ("Viewed: " ++ show viewed)
  m2 <- newClientMessage ("Updated: " ++ show updated)
  return (st & addClientMessage m1 & addClientMessage m2)
