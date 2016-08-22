{-# LANGUAGE TemplateHaskell #-}

module State where

import           Control.Monad (join, forM)
import           Data.HashMap.Strict (HashMap, (!))
import qualified Data.HashMap.Strict as HM
import           Data.List (elemIndex)
import qualified Data.Text as T
import           Lens.Micro.Platform

import           Network.Connection
import           Network.Mattermost
import           Network.Mattermost.Lenses

import           Config

data ChatState = ChatState
  { _csTok    :: Token
  , _csConn   :: ConnectionData
  , _csFocus  :: Maybe ChannelId
  , _chnMap   :: HashMap ChannelId Channel
  , _msgMap   :: HashMap ChannelId Posts
  , _usrMap   :: HashMap UserId UserProfile
  }

newState :: Token -> ConnectionData -> ChatState
newState t c = ChatState t c Nothing HM.empty HM.empty HM.empty

makeLenses ''ChatState

nextChannel :: (Int -> Int) -> ChatState -> ChatState
nextChannel nxt st =
  let keyList = HM.keys (st ^. chnMap) in
  case st ^. csFocus of
  Nothing -> st & csFocus .~ Just (head keyList)
  Just i  ->
    let Just idx = elemIndex i keyList
        nextKey  = keyList !! (nxt idx `mod` length keyList)
    in st & csFocus .~ Just nextKey

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

  let st = newState token cd
             & chnMap .~ HM.fromList [ (getId c, c)
                                     | c <- chans
                                     ]
             & usrMap .~ users
             & msgMap .~ msgs
  return st
