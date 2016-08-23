{-# LANGUAGE TemplateHaskell #-}

module State where

import           Brick (str, vBox)
import           Brick.Widgets.Edit (Editor, editor)
import           Control.Monad (join, forM)
import           Data.HashMap.Strict (HashMap, (!))
import qualified Data.HashMap.Strict as HM
import           Data.List (sort)
import qualified Data.Text as T
import           Lens.Micro.Platform

import           Network.Connection
import           Network.Mattermost
import           Network.Mattermost.Lenses

import           Config
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

data ChatState = ChatState
  { _csTok    :: Token
  , _csConn   :: ConnectionData
  , _csFocus  :: Zipper String
  , _csNames  :: MMNames
  , _csMe     :: User
  , _csMyTeam :: Team
  , _chnMap   :: HashMap ChannelId Channel
  , _msgMap   :: HashMap ChannelId Posts
  , _usrMap   :: HashMap UserId UserProfile
  , _cmdLine  :: Editor Int
  }

newState :: Token -> ConnectionData -> Zipper String -> User -> Team -> ChatState
newState t c i u m = ChatState
  { _csTok   = t
  , _csConn  = c
  , _csFocus = i
  , _csMe    = u
  , _csMyTeam = m
  , _csNames = MMNames [] [] HM.empty [] HM.empty
  , _chnMap  = HM.empty
  , _msgMap  = HM.empty
  , _usrMap  = HM.empty
  , _cmdLine = editor 1 (vBox . map str) (Just 1) ""
  }

makeLenses ''ChatState

nextChannel :: ChatState -> ChatState
nextChannel st = st & csFocus %~ Z.right

prevChannel :: ChatState -> ChatState
prevChannel st = st & csFocus %~ Z.left

currChannel :: ChatState -> ChannelId
currChannel st = (st ^. csNames . cnToChanId) ! Z.focus (st ^. csFocus)

channelExists :: ChatState -> String -> Bool
channelExists st n = n `elem` st ^. csNames . cnChans

setFocus :: String -> ChatState -> ChatState
setFocus n st = st & csFocus %~ Z.findRight (==n)

editMessage :: Post -> ChatState -> ChatState
editMessage new st =
  st & msgMap . ix (postChannelId new) . postsPostsL . ix (getId new) .~ new

addMessage :: Post -> ChatState -> ChatState
addMessage new st =
  st & msgMap . ix (postChannelId new) . postsPostsL . at (getId new) .~ Just new
     & msgMap . ix (postChannelId new) . postsOrderL %~ (getId new :)

getMessageListing :: ChannelId -> ChatState -> [(String, String)]
getMessageListing cId st =
  let us = st ^. usrMap
      ps = st ^. msgMap . ix cId . postsPostsL
      is = st ^. msgMap . ix cId . postsOrderL
  in reverse
    [ ( userProfileUsername (us ! postUserId p), postMessage p)
    | i <- is
    , let p = ps ! i
    ]

getChannelName :: ChannelId -> ChatState -> String
getChannelName cId st =
  (st ^. chnMap . ix cId . channelNameL)

setupState :: Config -> IO ChatState
setupState config = do
  ctx <- initConnectionContext
  let cd = mkConnectionData (T.unpack (configHost config))
                            (fromIntegral (configPort config))
                            ctx
      Right pass = configPass config
      login = Login { username = configUser config
                    , password = pass
                    , teamname = configTeam config
                    }

  (token, myUser) <- join (hoistE <$> mmLogin cd login)

  teamMap <- mmGetTeams cd token
  let [myTeam] = [ t | t <- HM.elems teamMap
                     , teamName t == T.unpack (configTeam config)
                     ]

  Channels chans _ <- mmGetChannels cd token (getId myTeam)

  msgs <- fmap HM.fromList $ forM chans $ \c -> do
    posts <- mmGetPosts cd token (getId myTeam) (getId c) 0 30
    return (getId c, posts)

  users <- mmGetProfiles cd token (getId myTeam)

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
        , _cnToChanId = HM.fromList
                          [ (channelName c, channelId c)
                          | c <- chans
                          ]
        , _cnUsers = sort (map userProfileUsername (HM.elems users))
        , _cnToUserId = HM.fromList
                          [ (userProfileUsername u, getId u)
                          | u <- HM.elems users
                          ]
        }
      chanZip = Z.findRight (== "town-square") (Z.fromList (chanNames ^. cnChans))
      st = newState token cd chanZip myUser myTeam
             & chnMap .~ HM.fromList [ (getId c, c)
                                     | c <- chans
                                     ]
             & usrMap .~ users
             & msgMap .~ msgs
             & csNames .~ chanNames

  return st
