{-# LANGUAGE TemplateHaskell #-}

module State where

import           Brick (EventM, str, vBox)
import           Brick.Main (viewportScroll, vScrollToEnd)
import           Brick.Widgets.Edit (Editor, editor, applyEdit)
import           Control.Monad (join, forM, when)
import           Control.Monad.IO.Class (liftIO)
import           Data.Text.Zipper (clearZipper)
import           Data.HashMap.Strict (HashMap, (!))
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
import           Network.Mattermost.Lenses

import           Config
import           TeamSelect
import           InputHistory
import           Zipper (Zipper)
import qualified Zipper as Z

data MMNames = MMNames
  { _cnChans    :: [String]
  , _cnDMs      :: [String]
  , _cnToChanId :: HashMap String ChannelId
  , _cnUsers    :: [String]
  , _cnToUserId :: HashMap String UserId
  }

makeLenses ''MMNames

data Name = ChannelMessages ChannelId
          | MessageInput
          | NormalChannelList
          | DMChannelList
          deriving (Eq, Show, Ord)

-- We want to continue referring to posts by their IDs, but we don't want to
-- have to synthesize new valid IDs for messages from the client itself. To
-- that end, a PostRef can be either a PostId or a newly-generated client ID
data PostRef
  = MMId PostId
  | CLId Int
    deriving (Eq, Show)

-- A ClientMessage is a message given to us by our client, like help text
-- or an error message.
data ClientMessage = ClientMessage
  { _cmText :: String
  , _cmDate :: UTCTime
  }

makeLenses ''ClientMessage

-- Our ChannelContents is roughly equivalent to the Post structure we get from
-- the MM API, but we also map integers to ClientMessage values, which are
-- bits out debug output from the client itself.
data ChannelContents = ChannelContents
  { _cdOrder   :: [PostRef]
  , _cdPosts   :: HashMap PostId Post
  , _cdCMsgs   :: HashMap Int ClientMessage
  , _cdViewed  :: UTCTime
  , _cdUpdated :: UTCTime
  }

fromPosts :: UTCTime -> UTCTime -> Posts -> ChannelContents
fromPosts viewed updated p = ChannelContents
  { _cdOrder   = map MMId (p ^. postsOrderL)
  , _cdPosts   = (p ^. postsPostsL)
  , _cdCMsgs   = HM.empty
  , _cdViewed  = viewed
  , _cdUpdated = updated
  }

makeLenses ''ChannelContents

data ChatState = ChatState
  { _csTok      :: Token
  , _csConn     :: ConnectionData
  , _csFocus    :: Zipper ChannelId
  , _csNames    :: MMNames
  , _csMe       :: User
  , _csMyTeam   :: Team
  , _chnMap     :: HashMap ChannelId Channel
  , _msgMap     :: HashMap ChannelId ChannelContents
  , _usrMap     :: HashMap UserId UserProfile
  , _cmdLine    :: Editor Name
  , _timeZone   :: TimeZone
  , _timeFormat :: Maybe String
  , _csInputHistory :: InputHistory
  , _csInputHistoryPosition :: HM.HashMap ChannelId (Maybe Int)
  , _csCurrentCompletion :: Maybe String
  }

newState :: Token
         -> ConnectionData
         -> Zipper ChannelId
         -> User
         -> Team
         -> TimeZone
         -> Maybe String
         -> InputHistory
         -> ChatState
newState t c i u m tz fmt hist = ChatState
  { _csTok    = t
  , _csConn   = c
  , _csFocus  = i
  , _csMe     = u
  , _csMyTeam = m
  , _csNames  = MMNames [] [] HM.empty [] HM.empty
  , _chnMap   = HM.empty
  , _msgMap   = HM.empty
  , _usrMap   = HM.empty
  , _cmdLine  = editor MessageInput (vBox . map str) (Just 1) ""
  , _timeZone = tz
  , _timeFormat = fmt
  , _csInputHistory = hist
  , _csInputHistoryPosition = mempty
  , _csCurrentCompletion = Nothing
  }

makeLenses ''ChatState

hasUnread :: ChatState -> ChannelId -> Bool
hasUnread st cId = maybe False id $ do
  chan <- st^.msgMap.at(cId)
  let u = chan^.cdViewed
      v = chan^.cdUpdated
  return (v > u)
  where

updateViewed :: ChatState -> EventM a ChatState
updateViewed st = do
  now <- liftIO getCurrentTime
  let cId = currentChannelId st
  liftIO $ mmUpdateLastViewedAt
    (st^.csConn)
    (st^.csTok)
    (getId (st^.csMyTeam))
    cId
  return (st & msgMap . ix cId . cdViewed .~ now)

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
      rs = st & chan . cdPosts . ix (getId new) .~ new
              & chan . cdUpdated .~ now
  return rs

addMessage :: Post -> ChatState -> EventM a ChatState
addMessage new st = do
  now <- liftIO getCurrentTime
  let chan = msgMap . ix (postChannelId new)
      rs = st & chan . cdPosts . at (getId new) .~ Just new
              & chan . cdOrder %~ (MMId (getId new) :)
              & chan . cdUpdated .~ now
--  return rs
  if postChannelId new == currentChannelId st
    then updateViewed rs
    else return rs

-- XXX: Right now, our new ID is based on the size of the map containing all
-- the ClientMessages, which only makes sense if we never delete ClientMessages.
-- We should probably figure out a better way of choosing IDs.
addClientMessage :: ClientMessage -> ChatState -> ChatState
addClientMessage msg st =
  let n = HM.size (st ^. msgMap . ix cid . cdCMsgs) + 1
      cid = currentChannelId st
  in st & msgMap . ix cid . cdCMsgs . at n .~ Just msg
        & msgMap . ix cid . cdOrder %~ (CLId n :)

mmMessageDigest :: ChannelId -> PostId -> ChatState -> (UTCTime, String, String)
mmMessageDigest cId ref st =
  ( postCreateAt p, userProfileUsername (us ! postUserId p), postMessage p )
  where p = ((ms ! cId) ^. cdPosts) ! ref
        ms = st ^. msgMap
        us = st ^. usrMap

newClientMessage :: String -> EventM a ClientMessage
newClientMessage msg = do
  now <- liftIO getCurrentTime
  return (ClientMessage msg now)

clientMessageDigest :: ChannelId -> Int -> ChatState -> (UTCTime, String, String)
clientMessageDigest cId ref st =
  ( m ^. cmDate, "*matterhorn", m ^. cmText )
  where m = ((ms ! cId) ^. cdCMsgs) ! ref
        ms = st ^. msgMap

getMessageListing :: ChannelId -> ChatState -> [(UTCTime, String, String)]
getMessageListing cId st =
  let is = st ^. msgMap . ix cId . cdOrder
  in reverse
    [ msg
    | i <- is
    , let msg = case i of
                  CLId c -> clientMessageDigest cId c st
                  MMId p -> mmMessageDigest cId p st
    ]

getChannelName :: ChannelId -> ChatState -> String
getChannelName cId st =
  (st ^. chnMap . ix cId . channelNameL)

getDMChannelName :: UserId -> UserId -> String
getDMChannelName me you = cname
  where
  [loUser, hiUser] = sort [ you, me ]
  cname = idString loUser ++ "__" ++ idString hiUser

getChannel :: ChannelId -> ChatState -> Maybe Channel
getChannel cId st =
  (st ^. chnMap . at cId)

execMMCommand :: String -> ChatState -> EventM a ()
execMMCommand cmd st = do
  let mc = MinCommand
        { minComChannelId = currentChannelId st
        , minComCommand   = "/" ++ cmd
        , minComSuggest   = False
        }
  _ <- liftIO $ mmExecute
         (st^.csConn)
         (st^.csTok)
         (st^.csMyTeam.teamIdL)
         mc
  return ()

setupState :: Config -> IO ChatState
setupState config = do
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
    return (getId c, fromPosts viewed updated posts)

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
      st = newState token cd chanZip myUser myTeam tz (configTimeFormat config) hist
             & chnMap .~ HM.fromList [ (getId c, c)
                                     | c <- chans
                                     ]
             & usrMap .~ users
             & msgMap .~ msgs
             & csNames .~ chanNames

  return st


debugPrintTimes :: ChatState -> String -> EventM a ChatState
debugPrintTimes st cn = do
  let Just cId = st^.csNames.cnToChanId.at(cn)
      Just ch = st^.msgMap.at(cId)
      viewed = ch^.cdViewed
      updated = ch^.cdUpdated
  m1 <- newClientMessage ("Viewed: " ++ show viewed)
  m2 <- newClientMessage ("Updated: " ++ show updated)
  return (st & addClientMessage m1 & addClientMessage m2)
