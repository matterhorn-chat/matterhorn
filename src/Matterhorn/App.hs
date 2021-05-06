module Matterhorn.App
  ( runMatterhorn
  , closeMatterhorn
  )
where

import           Prelude ()
import           Matterhorn.Prelude

import           Brick
import           Control.Monad.Trans.Except ( runExceptT )
import qualified Data.HashMap.Strict as HM
import qualified Graphics.Vty as Vty
import           Text.Aspell ( stopAspell )
import           GHC.Conc (getNumProcessors, setNumCapabilities)
import           System.Posix.IO ( stdInput )

import           Network.Mattermost

import           Matterhorn.Config
import           Matterhorn.Draw
import qualified Matterhorn.Events as Events
import           Matterhorn.IOUtil
import           Matterhorn.InputHistory
import           Matterhorn.LastRunState
import           Matterhorn.Options hiding ( ShowHelp )
import           Matterhorn.State.Setup
import           Matterhorn.State.Setup.Threads.Logging ( shutdownLogManager )
import           Matterhorn.Types


app :: App ChatState MHEvent Name
app = App
  { appDraw         = draw
  , appChooseCursor = \s cs -> case s^.csCurrentTeam.tsMode of
      Main                          -> showFirstCursor s cs
      ChannelSelect                 -> showFirstCursor s cs
      UserListOverlay               -> showFirstCursor s cs
      ReactionEmojiListOverlay      -> showFirstCursor s cs
      ChannelListOverlay            -> showFirstCursor s cs
      ManageAttachmentsBrowseFiles  -> showFirstCursor s cs
      ThemeListOverlay              -> showFirstCursor s cs
      ChannelTopicWindow            -> let tId = s^.csCurrentTeamId
                                       in showCursorNamed (ChannelTopicEditor tId) cs
      SaveAttachmentWindow _        -> let tId = s^.csCurrentTeamId
                                       in showCursorNamed (AttachmentPathEditor tId) cs
      LeaveChannelConfirm           -> Nothing
      DeleteChannelConfirm          -> Nothing
      MessageSelect                 -> Nothing
      MessageSelectDeleteConfirm    -> Nothing
      PostListOverlay _             -> Nothing
      ManageAttachments             -> Nothing
      ViewMessage                   -> Nothing
      ShowHelp _ _                  -> Nothing
      UrlSelect                     -> Nothing
      EditNotifyPrefs               -> Nothing
  , appHandleEvent  = Events.onEvent
  , appStartEvent   = return
  , appAttrMap      = (^.csResources.crTheme)
  }

applicationMaxCPUs :: Int
applicationMaxCPUs = 2

setupCpuUsage :: Config -> IO ()
setupCpuUsage config = do
    actualNumCpus <- getNumProcessors

    let requestedCPUs = case configCpuUsagePolicy config of
            SingleCPU -> 1
            MultipleCPUs -> min applicationMaxCPUs actualNumCpus

    setNumCapabilities requestedCPUs

runMatterhorn :: Options -> Config -> IO ChatState
runMatterhorn opts config = do
    setupCpuUsage config

    let mkVty = do
          mEraseChar <- Vty.getTtyEraseChar stdInput
          let addEraseChar cfg = case mEraseChar of
                  Nothing -> cfg
                  Just ch -> cfg { Vty.inputMap = (Nothing, [ch], Vty.EvKey Vty.KBS []) : Vty.inputMap cfg }

          vty <- Vty.mkVty $ addEraseChar Vty.defaultConfig
          let output = Vty.outputIface vty
          Vty.setMode output Vty.BracketedPaste True
          Vty.setMode output Vty.Hyperlink $ configHyperlinkingMode config
          Vty.setMode output Vty.Mouse $ configMouseMode config
          return vty

    (st, vty) <- setupState mkVty (optLogLocation opts) config
    finalSt <- customMain vty mkVty (Just $ st^.csResources.crEventQueue) app st

    forM_ (HM.elems $ finalSt^.csTeams) $ \ts ->
        case ts^.tsEditState.cedSpellChecker of
            Nothing -> return ()
            Just (s, _) -> stopAspell s

    return finalSt

-- | Cleanup resources and save data for restoring on program restart.
closeMatterhorn :: ChatState -> IO ()
closeMatterhorn finalSt = do
  logIfError (mmCloseSession $ getResourceSession $ finalSt^.csResources)
      "Error in closing session"

  logIfError (writeHistory (finalSt^.csInputHistory))
      "Error in writing history"

  logIfError (writeLastRunStates finalSt)
      "Error in writing last run states"

  shutdownLogManager $ finalSt^.csResources.crLogManager

  where
    logIfError action msg = do
      done <- runExceptT $ convertIOException $ action
      case done of
        Left err -> putStrLn $ msg <> ": " <> err
        Right _  -> return ()
