module Matterhorn.Scripts
  ( findAndRunScript
  , listScripts
  )
where

import           Prelude ()
import           Matterhorn.Prelude

import           Control.Concurrent ( takeMVar, newEmptyMVar )
import qualified Control.Concurrent.STM as STM
import qualified Data.Text as T
import           System.Exit ( ExitCode(..) )

import           Network.Mattermost.Types ( ChannelId )

import           Matterhorn.FilePaths ( Script(..), getAllScripts, locateScriptPath )
import           Matterhorn.State.Common
import           Matterhorn.State.Messages ( sendMessage )
import           Matterhorn.Types


findAndRunScript :: ChannelId -> Text -> Text -> MH ()
findAndRunScript cId scriptName input = do
    fpMb <- liftIO $ locateScriptPath (T.unpack scriptName)
    outputChan <- use (csResources.crSubprocessLog)
    case fpMb of
      ScriptPath scriptPath -> do
        doAsyncWith Preempt $ runScript cId outputChan scriptPath input
      NonexecScriptPath scriptPath -> do
        let msg = ("The script `" <> T.pack scriptPath <> "` cannot be " <>
             "executed. Try running\n" <>
             "```\n" <>
             "$ chmod u+x " <> T.pack scriptPath <> "\n" <>
             "```\n" <>
             "to correct this error. " <> scriptHelpAddendum)
        mhError $ GenericError msg
      ScriptNotFound -> do
        mhError $ NoSuchScript scriptName

runScript :: ChannelId -> STM.TChan ProgramOutput -> FilePath -> Text -> IO (Maybe (MH ()))
runScript cId outputChan fp text = do
  outputVar <- newEmptyMVar
  runLoggedCommand outputChan fp [] (Just $ T.unpack text) (Just outputVar)
  po <- takeMVar outputVar
  return $ case programExitCode po of
    ExitSuccess -> do
        case null $ programStderr po of
            True -> Just $ do
                mode <- use (csEditState.cedEditMode)
                sendMessage cId mode (T.pack $ programStdout po) []
            False -> Nothing
    ExitFailure _ -> Nothing

listScripts :: MH ()
listScripts = do
  (execs, nonexecs) <- liftIO getAllScripts
  let scripts = ("Available scripts are:\n" <>
                 mconcat [ "  - " <> T.pack cmd <> "\n"
                         | cmd <- execs
                         ])
  postInfoMessage scripts
  case nonexecs of
    [] -> return ()
    _  -> do
      let errMsg = ("Some non-executable script files are also " <>
                    "present. If you want to run these as scripts " <>
                    "in Matterhorn, mark them executable with \n" <>
                    "```\n" <>
                    "$ chmod u+x [script path]\n" <>
                    "```\n" <>
                    "\n" <>
                    mconcat [ "  - " <> T.pack cmd <> "\n"
                            | cmd <- nonexecs
                            ] <> "\n" <> scriptHelpAddendum)
      mhError $ GenericError errMsg

scriptHelpAddendum :: Text
scriptHelpAddendum =
  "For more help with scripts, run the command\n" <>
  "```\n/help scripts\n```\n"
