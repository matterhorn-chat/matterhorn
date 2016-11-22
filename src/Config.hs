{-# LANGUAGE RecordWildCards #-}

module Config
  ( Config(..)
  , PasswordSource(..)
  , findConfig
  , getCredentials
  ) where

import           Control.Applicative
import           Control.Monad.Trans.Except
import           Data.Ini.Config
import           Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import           Data.Monoid ((<>))
import           System.Process (readProcess)

import           Prelude

import           IOUtil
import           FilePaths

data PasswordSource =
    PasswordString Text
    | PasswordCommand Text
    deriving (Eq, Read, Show)

data Config = Config
  { configUser           :: Maybe Text
  , configHost           :: Text
  , configTeam           :: Maybe Text
  , configPort           :: Int
  , configPass           :: Maybe PasswordSource
  , configTimeFormat     :: Maybe Text
  , configDateFormat     :: Maybe Text
  , configTheme          :: Maybe Text
  , configSmartBacktick  :: Bool
  , configURLOpenCommand :: Maybe Text
  , configActivityBell   :: Bool
  , configShowMessagePreview :: Bool
  } deriving (Eq, Show)

fromIni :: IniParser Config
fromIni = do
  section "mattermost" $ do
    configUser           <- fieldMb "user"
    configHost           <- field   "host"
    configTeam           <- fieldMb "team"
    configPort           <- fieldOf "port" number
    configTimeFormat     <- fieldMb "timeFormat"
    configDateFormat     <- fieldMb "dateFormat"
    configTheme          <- fieldMb "theme"
    configURLOpenCommand <- fieldMb "urlOpenCommand"
    configSmartBacktick      <- fieldFlagDef "smartbacktick" True
    configShowMessagePreview <- fieldFlagDef "showMessagePreview" False
    configActivityBell       <- fieldFlagDef "activityBell" False
    configPass <- (Just . PasswordCommand <$> field "passcmd") <|>
                  (Just . PasswordString  <$> field "pass") <|>
                  pure Nothing
    return Config { .. }

findConfig :: Maybe FilePath -> IO (Either String Config)
findConfig Nothing = do
    let err = "Configuration file " <> show configFileName <> " not found"
    maybe (return $ Left err) getConfig =<< locateConfig configFileName
findConfig (Just path) = getConfig path

getConfig :: FilePath -> IO (Either String Config)
getConfig fp = runExceptT $ do
  t <- (convertIOException $ T.readFile fp) `catchE`
       (\e -> throwE $ "Could not read " <> show fp <> ": " <> e)
  case parseIniFile t fromIni of
    Left err -> do
      throwE $ "Unable to parse " ++ fp ++ ":" ++ err
    Right conf -> do
      actualPass <- case configPass conf of
        Just (PasswordCommand cmdString) -> do
          let (cmd:rest) = T.unpack <$> T.words cmdString
          output <- convertIOException (readProcess cmd rest "") `catchE`
                    (\e -> throwE $ "Could not execute password command: " <> e)
          return $ Just $ T.pack (takeWhile (/= '\n') output)
        Just (PasswordString pass) -> return $ Just pass
        _ -> return Nothing
      return conf { configPass = PasswordString <$> actualPass }

getCredentials :: Config -> Maybe (Text, Text)
getCredentials config = case (,) <$> configUser config <*> configPass config of
  Nothing                    -> Nothing
  Just (u, PasswordString p) -> Just (u, p)
  _ -> error $ "BUG: unexpected password state: " <> show (configPass config)
