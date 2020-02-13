module State.Help
  ( showHelpScreen
  )
where

import           Prelude ()
import           Prelude.MH

import           Brick.Main ( viewportScroll, vScrollToBeginning )

import           Types


showHelpScreen :: HelpTopic -> MH ()
showHelpScreen topic = do
    curMode <- gets appMode
    case curMode of
        ShowHelp {} -> return ()
        _ -> do
            mh $ vScrollToBeginning (viewportScroll HelpViewport)
            setMode $ ShowHelp topic curMode
