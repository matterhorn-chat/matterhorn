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
    curMode <- use (csCurrentTeam.tsMode)
    case curMode of
        ShowHelp {} -> return ()
        _ -> do
            mh $ vScrollToBeginning (viewportScroll HelpViewport)
            setMode $ ShowHelp topic curMode
