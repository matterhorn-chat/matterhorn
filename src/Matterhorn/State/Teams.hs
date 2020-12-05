module Matterhorn.State.Teams
  ( nextTeam
  , prevTeam
  )
where

import           Prelude ()
import           Matterhorn.Prelude

import           Lens.Micro.Platform ( (%=) )

import           Matterhorn.Types
import           Matterhorn.State.Channels
import           Matterhorn.State.Messages
import qualified Matterhorn.Zipper as Z


nextTeam :: MH ()
nextTeam = do
    csTeamZipper %= Z.right
    postChangeTeamCommon

prevTeam :: MH ()
prevTeam = do
    csTeamZipper %= Z.left

postChangeTeamCommon :: MH ()
postChangeTeamCommon = do
    updateViewed False
    fetchVisibleIfNeeded
