{-# LANGUAGE RecordWildCards #-}

module Config
  ( Config(..)
  , PasswordSource(..)
  , findConfig
  ) where

import           Control.Monad.Trans.Except
import qualified Data.HashMap.Strict as HM
import           Data.Ini
import           Data.Text (Text)
import qualified Data.Text as T
import           Data.Monoid ((<>))
import           System.Process (readProcess)

import           IOUtil
import           FilePaths

data PasswordSource =
    PasswordString Text
    | PasswordCommand String
    deriving (Eq, Read, Show)

data Config = Config
  { configUser        :: Maybe Text
  , configHost        :: Text
  , configTeam        :: Maybe Text
  , configPort        :: Int
  , configPass        :: Maybe PasswordSource
  , configTimeFormat  :: Maybe String
  , configTheme       :: Maybe String
  } deriving (Eq, Show)

(??) :: Maybe a -> String -> Either String a
(Just x) ?? _ = Right x
Nothing  ?? s = Left ("Missing field: `" ++ s ++ "`")

readT :: Read a => Text -> a
readT = read . T.unpack

fromIni :: Ini -> Either String Config
fromIni (Ini ini) = do
  cS <- HM.lookup "mattermost" ini ?? "mattermost"
  let configUser = HM.lookup "user" cS
  configHost <- HM.lookup "host" cS ?? "host"
  let configTimeFormat = T.unpack <$> HM.lookup "timeFormat" cS
      configTeam = HM.lookup "team" cS
  configPort <- readT `fmap` (HM.lookup "port" cS ?? "port")
  let passCmd = HM.lookup "passcmd" cS
      configTheme = T.unpack <$> HM.lookup "theme" cS
  let pass    = HM.lookup "pass" cS
  configPass <- case passCmd of
    Nothing -> case pass of
      Nothing -> return Nothing
      Just p -> return $ Just (PasswordString p)
    Just c -> return $ Just (PasswordCommand (T.unpack c))
  return Config { .. }

findConfig :: IO (Either String Config)
findConfig = do
    let err = "Configuration file " <> show configFileName <> " not found"
    maybe (return $ Left err) getConfig =<< locateConfig configFileName

getConfig :: FilePath -> IO (Either String Config)
getConfig fp = runExceptT $ do
  t <- (convertIOException $ readIniFile fp) `catchE`
       (\e -> throwE $ "Could not read " <> show fp <> ": " <> e)
  case t >>= fromIni of
    Left err -> do
      throwE $ "Unable to parse " ++ fp ++ ":" ++ err
    Right conf -> do
      actualPass <- case configPass conf of
        Just (PasswordCommand cmdString) -> do
          let (cmd:rest) = words cmdString
          output <- convertIOException (readProcess cmd rest "") `catchE`
                    (\e -> throwE $ "Could not execute password command: " <> e)
          return $ Just $ T.pack (takeWhile (/= '\n') output)
        Just (PasswordString pass) -> return $ Just pass
        _ -> return Nothing
      return conf { configPass = PasswordString <$> actualPass }
