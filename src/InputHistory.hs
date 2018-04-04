{-# LANGUAGE TemplateHaskell #-}
module InputHistory
  ( InputHistory
  , newHistory
  , readHistory
  , writeHistory
  , addHistoryEntry
  , getHistoryEntry
  , removeChannelHistory
  ) where

import Prelude ()
import Prelude.MH

import Control.Monad.Trans.Except
import Lens.Micro.Platform
import qualified Data.HashMap.Strict as HM
import System.Directory (createDirectoryIfMissing)
import System.FilePath (dropFileName)
import qualified System.IO.Strict as S
import qualified Data.Vector as V
import           Data.Text ( Text )
import qualified System.Posix.Files as P
import qualified System.Posix.Types as P

import IOUtil
import FilePaths
import Network.Mattermost.Types (ChannelId)

data InputHistory =
    InputHistory { _historyEntries :: HM.HashMap ChannelId (V.Vector Text)
                 }
                 deriving (Show)

makeLenses ''InputHistory

newHistory :: InputHistory
newHistory = InputHistory mempty

removeChannelHistory :: ChannelId -> InputHistory -> InputHistory
removeChannelHistory cId ih = ih & historyEntries.at cId .~ Nothing

historyFileMode :: P.FileMode
historyFileMode = P.unionFileModes P.ownerReadMode P.ownerWriteMode

writeHistory :: InputHistory -> IO ()
writeHistory ih = do
    historyFile <- historyFilePath
    createDirectoryIfMissing True $ dropFileName historyFile
    let entries = (\(cId, z) -> (cId, V.toList z)) <$>
                  (HM.toList $ ih^.historyEntries)
    writeFile historyFile $ show entries
    P.setFileMode historyFile historyFileMode

readHistory :: IO (Either String InputHistory)
readHistory = runExceptT $ do
    contents <- convertIOException (S.readFile =<< historyFilePath)
    case reads contents of
        [(val, "")] -> do
            let entries = (\(cId, es) -> (cId, V.fromList es)) <$> val
            return $ InputHistory $ HM.fromList entries
        _ -> throwE "Failed to parse history file"

addHistoryEntry :: Text -> ChannelId -> InputHistory -> InputHistory
addHistoryEntry e cId ih = ih & historyEntries.at cId %~ insertEntry
    where
    insertEntry Nothing  = Just $ V.singleton e
    insertEntry (Just v) =
      Just $ V.cons e (V.filter (/= e) v)

getHistoryEntry :: ChannelId -> Int -> InputHistory -> Maybe Text
getHistoryEntry cId i ih = do
    es <- ih^.historyEntries.at cId
    es ^? ix i
