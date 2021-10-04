module Matterhorn.State.Help
  ( showHelpScreen
  )
where

import           Prelude ()
import           Matterhorn.Prelude

import           Brick.Main ( viewportScroll, vScrollToBeginning )

import           Matterhorn.Types


showHelpScreen :: HelpTopic -> MH ()
showHelpScreen topic = do
    tId <- use csCurrentTeamId
    curMode <- use (csTeam(tId).tsMode)
    case curMode of
        ShowHelp {} -> return ()
        _ -> do
            mh $ vScrollToBeginning (viewportScroll HelpViewport)
            setMode tId $ ShowHelp topic curMode
