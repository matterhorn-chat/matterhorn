module Scripts
  ( findAndRunScript
  , listScripts
  )
where

import Control.Monad.IO.Class (liftIO)
import qualified Data.Text as T
import Data.Monoid ((<>))
import System.Process (readProcessWithExitCode)
import System.Exit (ExitCode(..))
import Lens.Micro.Platform (use)

import Types
import State (sendMessage)
import State.Common
import FilePaths (Script(..), getAllScripts, locateScriptPath)

findAndRunScript :: T.Text -> T.Text -> MH ()
findAndRunScript scriptName input = do
    fpMb <- liftIO $ locateScriptPath (T.unpack scriptName)
    case fpMb of
      ScriptPath scriptPath -> do
        doAsyncWith Preempt $ runScript scriptPath input
      NonexecScriptPath scriptPath -> do
        let msg = ("The script `" <> T.pack scriptPath <> "` cannot be " <>
             "executed. Try running\n" <>
             "```\n" <>
             "$ chmod u+x " <> T.pack scriptPath <> "\n" <>
             "```\n" <>
             "to correct this error. " <> scriptHelpAddendum)
        postErrorMessage msg
      ScriptNotFound -> do
        let msg = ("No script named " <> scriptName <> " was found")
        postErrorMessage msg

runScript :: FilePath -> T.Text -> IO (MH ())
runScript fp text = do
  (code, stdout, stderr) <- readProcessWithExitCode fp [] (T.unpack text)
  case code of
    ExitSuccess -> return $ do
      mode <- use (csEditState.cedEditMode)
      sendMessage mode (T.pack stdout)
    ExitFailure _ -> return $ do
      let msgText = "The script `" <> T.pack fp <> "` exited with a " <>
                    "non-zero exit code."
          msgText' = if stderr == ""
                       then msgText
                       else msgText <> " It also produced the " <>
                            "following output on stderr:\n~~~~~\n" <>
                            T.pack stderr <> "~~~~~\n" <> scriptHelpAddendum
      postErrorMessage msgText'

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
      postErrorMessage errMsg

scriptHelpAddendum :: T.Text
scriptHelpAddendum =
  "For more help with scripts, run the command\n" <>
  "```\n/help scripts\n```\n"
