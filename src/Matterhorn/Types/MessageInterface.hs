{-# LANGUAGE TemplateHaskell #-}
module Matterhorn.Types.MessageInterface
  ( MessageInterface(..)
  , miMessages
  , miEditor
  , miMode
  , miMessageSelect
  , miRootPostId
  , miChannelId
  , miTarget

  , MessageInterfaceMode(..)
  , MessageInterfaceTarget(..)
  )
where

import           Prelude ()
import           Matterhorn.Prelude

import           Lens.Micro.Platform ( makeLenses )
import           Network.Mattermost.Types ( ChannelId, TeamId )

import           Matterhorn.Types.Core ( MessageSelectState )
import           Matterhorn.Types.EditState
import           Matterhorn.Types.Messages


-- | A UI region in which a specific message listing is viewed, where
-- the user can send messages in that channel or thread.
data MessageInterface n i =
    MessageInterface { _miMessages :: Messages
                     -- ^ The messages.
                     , _miEditor :: EditState n
                     -- ^ The editor and associated state for composing
                     -- messages in this channel or thread.
                     , _miMessageSelect :: MessageSelectState
                     -- ^ Message selection state for the interface.
                     , _miRootPostId :: i
                     -- ^ The root post ID if these messages belong to a
                     -- thread.
                     , _miChannelId :: ChannelId
                     -- ^ The channel that these messages belong to.
                     , _miMode :: MessageInterfaceMode
                     -- ^ The mode of the interface.
                     , _miTarget :: MessageInterfaceTarget
                     -- ^ The target value for this message interface
                     }

data MessageInterfaceMode =
    Compose
    -- ^ Composing messages and interacting with the editor
    | MessageSelect
    -- ^ Selecting from messages in the listing
    deriving (Eq, Show)

data MessageInterfaceTarget =
    MITeamThread TeamId
    | MIChannel ChannelId
    deriving (Eq, Show)

makeLenses ''MessageInterface
