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
    curMode <- getTeamMode tId
    case curMode of
        ShowHelp {} -> return ()
        _ -> do
            mh $ vScrollToBeginning (viewportScroll HelpViewport)
            pushMode tId $ ShowHelp topic
