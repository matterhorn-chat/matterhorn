module Matterhorn.App
  ( runMatterhorn
  , closeMatterhorn
  )
where

import           Prelude ()
import           Matterhorn.Prelude

import           Brick
import           Control.Monad.Trans.Except ( runExceptT )
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
app =
    App { appDraw         = draw
        , appHandleEvent  = Events.onEvent
        , appStartEvent   = return ()
        , appAttrMap      = (^.csResources.crTheme)
        , appChooseCursor = \s cs -> do
            tId <- s^.csCurrentTeamId
            cursorByMode cs s tId (teamMode $ s^.csTeam(tId))
        }

cursorByMode :: [CursorLocation Name] -> ChatState -> TeamId -> Mode -> Maybe (CursorLocation Name)
cursorByMode cs s tId mode =
    case mode of
        Main -> case s^.csTeam(tId).tsMessageInterfaceFocus of
            FocusCurrentChannel -> do
                cId <- s^.csCurrentChannelId(tId)
                mi <- s^?maybeChannelMessageInterface(cId)
                cur <- messageInterfaceCursor mi
                showCursorNamed cur cs
            FocusThread -> do
                ti <- s^.csTeam(tId).tsThreadInterface
                cur <- messageInterfaceCursor ti
                showCursorNamed cur cs
        LeaveChannelConfirm           -> Nothing
        DeleteChannelConfirm          -> Nothing
        MessageSelectDeleteConfirm {} -> Nothing
        (PostListWindow {})           -> Nothing
        ViewMessage                   -> Nothing
        (ShowHelp {})                 -> Nothing
        EditNotifyPrefs               -> Nothing
        ChannelSelect                 -> showFirstCursor s cs
        UserListWindow                -> showFirstCursor s cs
        ReactionEmojiListWindow       -> showFirstCursor s cs
        ChannelListWindow             -> showFirstCursor s cs
        ThemeListWindow               -> showFirstCursor s cs
        ChannelTopicWindow            -> showCursorNamed (ChannelTopicEditor tId) cs

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

    case st^.csResources.crSpellChecker of
        Nothing -> return ()
        Just s -> stopAspell s

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
