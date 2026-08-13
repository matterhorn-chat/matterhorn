module Matterhorn.State.Teams
  ( makeClientChannel
  )
where

import           Prelude ()
import           Matterhorn.Prelude

import qualified Brick.BChan as BCH
import           Text.Aspell ( Aspell )

import           Network.Mattermost.Types ( Channel, ChannelMember, UserId, TeamId, Bookmark )

import           Matterhorn.Types ( MHEvent )
import           Matterhorn.Types.Channels ( ClientChannel )

makeClientChannel :: (MonadIO m)
                  => BCH.BChan MHEvent
                  -> Maybe Aspell
                  -> UserId
                  -> Maybe TeamId
                  -> Seq Bookmark
                  -> Channel
                  -> ChannelMember
                  -> m ClientChannel
