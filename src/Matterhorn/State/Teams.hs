module Matterhorn.State.Teams
  ( nextTeam
  , prevTeam
  )
where

import           Prelude ()
import           Matterhorn.Prelude

import           Lens.Micro.Platform ( (%=) )

import           Matterhorn.Types
import qualified Matterhorn.Zipper as Z


nextTeam :: MH ()
nextTeam = do
    csTeamZipper %= Z.right

prevTeam :: MH ()
prevTeam = do
    csTeamZipper %= Z.left
