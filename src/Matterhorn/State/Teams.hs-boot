module Matterhorn.State.Teams where

import           Prelude ()
import           Matterhorn.Prelude

import qualified Brick.BChan as BCH
import           Text.Aspell ( Aspell )

import           Network.Mattermost.Types ( Channel, UserId, TeamId )

import           Matterhorn.Types ( MHEvent )
import           Matterhorn.Types.Channels ( ClientChannel )

makeClientChannel :: (MonadIO m) => BCH.BChan MHEvent -> Maybe Aspell -> UserId -> Maybe TeamId -> Channel -> m ClientChannel
