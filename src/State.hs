{-# LANGUAGE TemplateHaskell #-}

module State where

import Data.HashMap.Strict (HashMap, empty)
import Data.IORef (IORef)
import Lens.Micro.Platform
import Network.Mattermost

data ChatState = ChatState
  { _chnMap :: HashMap ChannelId Channel
  , _msgMap :: HashMap ChannelId Posts
  , _usrMap :: HashMap UserId UserProfile
  } deriving (Eq, Show)

newState :: ChatState
newState = ChatState empty empty empty

type StateRef = IORef ChatState

makeLenses ''ChatState
