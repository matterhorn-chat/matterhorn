module App
  ( runMatterhorn
  , closeMatterhorn
  )
where

import           Prelude ()
import           Prelude.MH

import           Brick
import           Control.Monad.Trans.Except ( runExceptT )
import qualified Graphics.Vty as Vty
import           System.IO ( IOMode(WriteMode), openFile, hClose )
import           Text.Aspell ( stopAspell )

import           Network.Mattermost

import           Config
import           Draw
import           Events
import           IOUtil
import           InputHistory
import           LastRunState
import           Options
import           State.Setup
import           Types


app :: App ChatState MHEvent Name
app = App
  { appDraw         = draw
  , appChooseCursor = showFirstCursor
  , appHandleEvent  = onEvent
  , appStartEvent   = return
  , appAttrMap      = (^.csResources.crTheme)
  }

runMatterhorn :: Options -> Config -> IO ChatState
runMatterhorn opts config = do
    logFile <- case optLogLocation opts of
      Just path -> Just `fmap` openFile path WriteMode
      Nothing   -> return Nothing

    st <- setupState logFile config

    let mkVty = do
          vty <- Vty.mkVty Vty.defaultConfig
          let output = Vty.outputIface vty
          Vty.setMode output Vty.BracketedPaste True
          Vty.setMode output Vty.Hyperlink $ configHyperlinkingMode config
          return vty

    finalSt <- customMain mkVty (Just $ st^.csResources.crEventQueue) app st

    case finalSt^.csEditState.cedSpellChecker of
        Nothing -> return ()
        Just (s, _) -> stopAspell s

    case logFile of
      Nothing -> return ()
      Just h -> hClose h

    return finalSt

-- | Cleanup resources and save data for restoring on program restart.
closeMatterhorn :: ChatState -> IO ()
closeMatterhorn finalSt = do
  logIfError (mmCloseSession $ getResourceSession $ finalSt^.csResources) "Error in closing session"
  logIfError (writeHistory (finalSt^.csEditState.cedInputHistory)) "Error in writing history"
  logIfError (writeLastRunState finalSt) "Error in writing last run state"
  where
    logIfError action msg = do
      done <- runExceptT $ convertIOException $ action
      case done of
        Left err -> putStrLn $ msg <> ": " <> err
        Right _  -> return ()
