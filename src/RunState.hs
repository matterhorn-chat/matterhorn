{-# LANGUAGE TemplateHaskell #-}
module RunState
  ( RunState
  , rsHost
  , rsPort
  , rsUserId
  , rsChannelId
  , writeRunState
  , readRunState
  , isValidRunState
  ) where

import Prelude ()
import Prelude.Compat

import Control.Monad.Trans.Except
import Lens.Micro.Platform
import System.Directory (createDirectoryIfMissing)
import System.FilePath (dropFileName)
import qualified System.IO.Strict as S
import qualified System.Posix.Files as P
import qualified System.Posix.Types as P

import IOUtil
import FilePaths
import Network.Mattermost (Hostname, Port)
import Network.Mattermost.Types
import Network.Mattermost.Lenses
import Types
import Zipper (focusL)

-- | Run state of the program. This is saved in a file on program exit and
-- | looked up from the file on program startup.
data RunState = RunState
  { _rsHost      :: Hostname  -- ^ Host of the server
  , _rsPort      :: Port      -- ^ Post of the server
  , _rsUserId    :: UserId    -- ^ ID of the logged-in user
  , _rsChannelId :: ChannelId -- ^ ID of the last selected channel
  } deriving (Show, Read)

makeLenses ''RunState

toRunState :: ChatState -> RunState
toRunState cs = RunState
  { _rsHost      = cs^.csResources.crConn.cdHostnameL
  , _rsPort      = cs^.csResources.crConn.cdPortL
  , _rsUserId    = cs^.csMe.userIdL
  , _rsChannelId = cs^.csFocus.focusL
  }

runStateFileMode :: P.FileMode
runStateFileMode = P.unionFileModes P.ownerReadMode P.ownerWriteMode

-- | Writes the run state to a file. The file is specific to the current team.
writeRunState :: ChatState -> IO (Either String ())
writeRunState cs = runExceptT . convertIOException $ do
  let runState = toRunState cs
      tId      = cs^.csMyTeam.teamIdL
  runStateFile <- runStateFilePath . unId . toId $ tId
  createDirectoryIfMissing True $ dropFileName runStateFile
  writeFile runStateFile $ show runState
  P.setFileMode runStateFile runStateFileMode

-- | Reads the run state from a file given the current team ID.
readRunState :: TeamId -> IO (Either String RunState)
readRunState tId = runExceptT $ do
  contents <- convertIOException $
    (runStateFilePath . unId . toId $ tId) >>= S.readFile
  case reads contents of
    [(val, "")] -> return val
    _ -> throwE "Failed to parse runState file"

-- | Checks if the given run state is valid for the current server and user.
isValidRunState :: ChatResources -> User -> RunState -> Bool
isValidRunState cr myUser rs =
     rs^.rsHost   == cr^.crConn.cdHostnameL
  && rs^.rsPort   == cr^.crConn.cdPortL
  && rs^.rsUserId == myUser^.userIdL
