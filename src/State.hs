module State
  ( showHelpScreen
  )
where

import           Prelude ()
import           Prelude.MH

import           Brick.Main ( viewportScroll, vScrollToBeginning )
import           Lens.Micro.Platform

import qualified Network.Mattermost.Endpoints as MM

import           Types

import           State.Common


showHelpScreen :: HelpTopic -> MH ()
showHelpScreen topic = do
    mh $ vScrollToBeginning (viewportScroll HelpViewport)
    setMode $ ShowHelp topic
