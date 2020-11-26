{-# LANGUAGE TemplateHaskell #-}
module Matterhorn.LastRunState
  ( LastRunState
  , lrsHost
  , lrsPort
  , lrsUserId
  , lrsSelectedChannelId
  , writeLastRunState
  , readLastRunState
  , isValidLastRunState
  )
where

import           Prelude ()
import           Matterhorn.Prelude

import           Control.Monad.Trans.Except
import qualified Data.Aeson as A
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS
import           Lens.Micro.Platform ( makeLenses )
import           System.Directory ( createDirectoryIfMissing )
import           System.FilePath ( dropFileName )
import qualified System.Posix.Files as P
import qualified System.Posix.Types as P

import           Network.Mattermost.Lenses
import           Network.Mattermost.Types

import           Matterhorn.FilePaths
import           Matterhorn.IOUtil
import           Matterhorn.Types


-- | Run state of the program. This is saved in a file on program exit and
-- | looked up from the file on program startup.
data LastRunState = LastRunState
  { _lrsHost              :: Hostname  -- ^ Host of the server
  , _lrsPort              :: Port      -- ^ Post of the server
  , _lrsUserId            :: UserId    -- ^ ID of the logged-in user
  , _lrsSelectedChannelId :: ChannelId -- ^ ID of the last selected channel
  }

instance A.ToJSON LastRunState where
  toJSON lrs = A.object [ "host"           A..= _lrsHost lrs
                        , "port"           A..= _lrsPort lrs
                        , "user_id"        A..= _lrsUserId lrs
                        , "sel_channel_id" A..= _lrsSelectedChannelId lrs
                        ]

instance A.FromJSON LastRunState where
  parseJSON = A.withObject "LastRunState" $ \v ->
    LastRunState
    <$> v A..: "host"
    <*> v A..: "port"
    <*> v A..: "user_id"
    <*> v A..: "sel_channel_id"

makeLenses ''LastRunState

toLastRunState :: ChatState -> LastRunState
toLastRunState cs = LastRunState
  { _lrsHost              = cs^.csResources.crConn.cdHostnameL
  , _lrsPort              = cs^.csResources.crConn.cdPortL
  , _lrsUserId            = myUserId cs
  , _lrsSelectedChannelId = cs^.csCurrentChannelId
  }

lastRunStateFileMode :: P.FileMode
lastRunStateFileMode = P.unionFileModes P.ownerReadMode P.ownerWriteMode

-- | Writes the run state to a file. The file is specific to the current team.
-- | Writes only if the current channel is an ordrinary or a private channel.
writeLastRunState :: ChatState -> IO ()
writeLastRunState cs =
  when (cs^.csCurrentChannel.ccInfo.cdType `elem` [Ordinary, Private]) $ do
    let runState = toLastRunState cs
        tId = cs^.csCurrentTeamId

    lastRunStateFile <- lastRunStateFilePath $ unId $ toId tId
    createDirectoryIfMissing True $ dropFileName lastRunStateFile
    BS.writeFile lastRunStateFile $ LBS.toStrict $ A.encode runState
    P.setFileMode lastRunStateFile lastRunStateFileMode

-- | Reads the last run state from a file given the current team ID.
readLastRunState :: TeamId -> IO (Either String LastRunState)
readLastRunState tId = runExceptT $ do
  contents <- convertIOException $
    lastRunStateFilePath (unId $ toId tId) >>= BS.readFile
  case A.eitherDecodeStrict' contents of
    Right val -> return val
    Left err -> throwE $ "Failed to parse lastRunState file: " ++ err

-- | Checks if the given last run state is valid for the current server and user.
isValidLastRunState :: ChatResources -> User -> LastRunState -> Bool
isValidLastRunState cr me rs =
     rs^.lrsHost   == cr^.crConn.cdHostnameL
  && rs^.lrsPort   == cr^.crConn.cdPortL
  && rs^.lrsUserId == me^.userIdL
