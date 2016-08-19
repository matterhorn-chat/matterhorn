{-# LANGUAGE RecordWildCards #-}

module Main where

import           Control.Monad (join, forM, forM_)
import           Data.HashMap.Strict ((!))
import qualified Data.HashMap.Strict as HM
import           Data.IORef
import qualified Data.Text as T
import           Lens.Micro.Platform

import           Network.Connection
import           Network.Mattermost
import           Network.Mattermost.Lenses
import           Network.Mattermost.WebSocket
import           Network.Mattermost.WebSocket.Types

import           Config
import           State

editMessage :: Post -> StateRef -> IO ()
editMessage new stRef = modifyIORef stRef $ \ st ->
  st & msgMap . ix (postChannelId new) . postsPostsL . ix (getId new) .~ new

addMessage :: Post -> StateRef -> IO ()
addMessage new stRef = modifyIORef stRef $ \ st ->
  st & msgMap . ix (postChannelId new) . postsPostsL . at (getId new) .~ Just new
     & msgMap . ix (postChannelId new) . postsOrderL %~ (getId new :)

getMessageListing :: ChannelId -> StateRef -> IO [(String, String)]
getMessageListing cId stRef = do
  st <- readIORef stRef
  let us = st ^. usrMap
  let ps = st ^. msgMap . ix cId . postsPostsL
  let is = st ^. msgMap . ix cId . postsOrderL
  return $ reverse
    [ ( userProfileUsername (us ! postUserId p), postMessage p)
    | i <- is
    , let p = ps ! i
    ]

main :: IO ()
main = do
  config <- getConfig
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

  st <- newIORef $ newState & chnMap .~ HM.fromList [ (getId c, c)
                                                    | c <- chans
                                                    ]
                            & usrMap .~ users
                            & msgMap .~ msgs

  putStrLn "Ready."
  mmWithWebSocket cd token (onEvent st)
                           (handleInput st)

onEvent :: StateRef -> WebsocketEvent -> IO ()
onEvent st we = do
  case weAction we of
    WMPosted -> case wepPost (weProps we) of
      Just p  -> addMessage p st
      Nothing -> return ()
    WMPostEdited -> case wepPost (weProps we) of
      Just p  -> editMessage p st
      Nothing -> return ()
    WMPostDeleted -> case wepPost (weProps we) of
      Just p  -> editMessage p { postMessage = "[deleted]" } st
      Nothing -> return ()
    _ -> return ()


handleInput :: StateRef -> MMWebSocket -> IO ()
handleInput st ws = do
  ln <- getLine
  case words ln of
    ["show", chan] -> do
      ChatState { _chnMap = cs } <- readIORef st
      case [ c | c <- HM.elems cs, channelName c == chan ] of
        c:_ -> do
          ms <- getMessageListing (channelId c) st
          forM_ ms $ \ (u, m) -> do
            putStrLn ("@" ++ u ++ ":  " ++ m)
          handleInput st ws
        _ -> do
          putStrLn ("cannot find " ++ chan)
          handleInput st ws
    ["quit"] -> do
      mmCloseWebSocket ws
    cmd -> do
      putStrLn ("I don't know how to " ++ unwords cmd)
      handleInput st ws
