{-# LANGUAGE TemplateHaskell #-}
module Matterhorn.LastRunState
  ( LastRunState
  , lrsHost
  , lrsPort
  , lrsUserId
  , lrsSelectedChannelId
  , lrsOpenThread
  , writeLastRunStates
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
import qualified Data.HashMap.Strict as HM
import           Lens.Micro.Platform ( makeLenses )
import           System.Directory ( createDirectoryIfMissing )
import           System.FilePath ( dropFileName )
import qualified System.PosixCompat.Files as P
import qualified System.PosixCompat.Types as P

import           Network.Mattermost.Lenses
import           Network.Mattermost.Types

import           Matterhorn.FilePaths
import           Matterhorn.IOUtil
import           Matterhorn.Types


-- | Run state of the program. This is saved in a file on program exit and
-- | looked up from the file on program startup.
data LastRunState =
    LastRunState { _lrsHost              :: !Hostname  -- ^ Host of the server
                 , _lrsPort              :: !Port      -- ^ Post of the server
                 , _lrsUserId            :: !UserId    -- ^ ID of the logged-in user
                 , _lrsSelectedChannelId :: !(Maybe ChannelId) -- ^ ID of the last selected channel
                 , _lrsOpenThread        :: !(Maybe (ChannelId, PostId))
                 }

instance A.ToJSON LastRunState where
    toJSON lrs = A.object [ "host"           A..= _lrsHost lrs
                          , "port"           A..= _lrsPort lrs
                          , "user_id"        A..= _lrsUserId lrs
                          , "sel_channel_id" A..= _lrsSelectedChannelId lrs
                          , "open_thread"    A..= _lrsOpenThread lrs
                          ]

instance A.FromJSON LastRunState where
    parseJSON = A.withObject "LastRunState" $ \v ->
        LastRunState
        <$> v A..: "host"
        <*> v A..: "port"
        <*> v A..: "user_id"
        <*> v A..: "sel_channel_id"
        <*> v A..:? "open_thread"

makeLenses ''LastRunState

toLastRunState :: ChatState -> TeamId -> LastRunState
toLastRunState cs tId =
    LastRunState { _lrsHost              = cs^.csResources.crConn.cdHostnameL
                 , _lrsPort              = cs^.csResources.crConn.cdPortL
                 , _lrsUserId            = myUserId cs
                 , _lrsSelectedChannelId = cs^.csCurrentChannelId(tId)
                 , _lrsOpenThread = do
                     ti <- cs^.csTeam(tId).tsThreadInterface
                     return (ti^.miChannelId, ti^.miRootPostId)
                 }

lastRunStateFileMode :: P.FileMode
lastRunStateFileMode = P.unionFileModes P.ownerReadMode P.ownerWriteMode

-- | Writes the run state to a file. The file is specific to the current team.
-- | Writes only if the current channel is an ordrinary or a private channel.
writeLastRunStates :: ChatState -> IO ()
writeLastRunStates cs =
    forM_ (HM.keys $ cs^.csTeams) $ \tId ->
        writeLastRunState cs tId

writeLastRunState :: ChatState -> TeamId -> IO ()
writeLastRunState cs tId =
    case cs^.csCurrentChannelId(tId) of
        Nothing -> return ()
        Just cId -> case cs^?csChannel(cId) of
            Nothing -> return ()
            Just chan ->
                when (chan^.ccInfo.cdType `elem` [Ordinary, Private]) $ do
                    let runState = toLastRunState cs tId

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
    and [ rs^.lrsHost   == cr^.crConn.cdHostnameL
        , rs^.lrsPort   == cr^.crConn.cdPortL
        , rs^.lrsUserId == me^.userIdL
        ]
