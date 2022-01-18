module Matterhorn.State.Help
  ( showHelpScreen
  )
where

import           Prelude ()
import           Matterhorn.Prelude

import           Brick.Main ( viewportScroll, vScrollToBeginning )

import           Network.Mattermost.Types ( TeamId )

import           Matterhorn.Types


showHelpScreen :: TeamId -> HelpTopic -> MH ()
showHelpScreen tId topic = do
    curMode <- use (csTeam(tId).tsMode)
    case curMode of
        ShowHelp {} -> return ()
        _ -> do
            mh $ vScrollToBeginning (viewportScroll HelpViewport)
            setMode tId $ ShowHelp topic curMode
