module Matterhorn.App
  ( runMatterhorn
  , closeMatterhorn
  )
where

import           Prelude ()
import           Matterhorn.Prelude

import           Brick
import           Control.Monad.Trans.Except ( runExceptT )
import qualified Control.Exception as E
import qualified Graphics.Vty as Vty
import qualified Graphics.Vty.CrossPlatform as Vty
import qualified Graphics.Vty.UnicodeWidthTable.Install as Vty
import           Text.Aspell ( stopAspell )
import           GHC.Conc (getNumProcessors, setNumCapabilities)

import           Network.Mattermost

import           Matterhorn.Config
import           Matterhorn.Draw
import qualified Matterhorn.Events as Events
import           Matterhorn.IOUtil
import           Matterhorn.InputHistory
import           Matterhorn.LastRunState
import           Matterhorn.Options
import           Matterhorn.State.Setup
import           Matterhorn.State.Setup.Threads.Logging ( shutdownLogManager )
import           Matterhorn.Types


app :: App ChatState MHEvent Name
app =
    App { appDraw         = draw
        , appHandleEvent  = Events.onEvent
        , appAttrMap      = (^.csResources.crTheme)
        , appChooseCursor = \s cs -> do
            tId <- s^.csCurrentTeamId
            cursorByMode cs s tId (teamMode $ s^.csTeam(tId))
        , appStartEvent = do
            vty <- getVtyHandle
            (w, h) <- liftIO $ Vty.displayBounds $ Vty.outputIface vty
            runMHEvent $ Events.setWindowSize w h
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

setupCharWidthMap :: Config -> IO ()
setupCharWidthMap config = do
    case configCharacterWidths config of
        Nothing -> return ()
        Just widths -> do
            let wMap = buildWidthMap widths
            Vty.installUnicodeWidthTable wMap `E.catch`
                (\(_::Vty.TableInstallException) -> return ())

runMatterhorn :: Options -> Config -> IO ChatState
runMatterhorn opts config = do
    setupCpuUsage config

    setupCharWidthMap config

    let mkVty = do
          vty <- Vty.mkVty Vty.defaultConfig
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
