{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TupleSections #-}

module Config
  ( Config(..)
  , PasswordSource(..)
  , findConfig
  ) where

import           Control.Monad.Trans.Except
import           Control.Monad (forM)
import qualified Data.HashMap.Strict as HM
import           Data.Ini
import           Data.Text (Text)
import qualified Data.Text as T
import           Data.Maybe (listToMaybe)
import           Data.Monoid ((<>))
import           System.Directory (doesFileExist)
import           System.Environment.XDG.BaseDir (getAllConfigFiles)
import           System.Process (readProcess)

import           IOUtil

data PasswordSource =
    PasswordString Text
    | PasswordCommand String
    deriving (Eq, Read, Show)

data Config = Config
  { configUser        :: Text
  , configHost        :: Text
  , configTeam        :: Maybe Text
  , configPort        :: Int
  , configPass        :: PasswordSource
  , configTimeFormat  :: Maybe String
  } deriving (Eq, Show)

(??) :: Maybe a -> String -> Either String a
(Just x) ?? _ = Right x
Nothing  ?? s = Left ("Missing field: `" ++ s ++ "`")

readT :: Read a => Text -> a
readT = read . T.unpack

fromIni :: Ini -> Either String Config
fromIni (Ini ini) = do
  cS <- HM.lookup "mattermost" ini ?? "mattermost"
  configUser <- HM.lookup "user" cS ?? "user"
  configHost <- HM.lookup "host" cS ?? "host"
  let configTimeFormat = T.unpack <$> HM.lookup "timeFormat" cS
      configTeam = HM.lookup "team" cS
  configPort <- readT `fmap` (HM.lookup "port" cS ?? "port")
  let passCmd = HM.lookup "passcmd" cS
  let pass    = HM.lookup "pass" cS
  configPass <- case passCmd of
    Nothing -> case pass of
      Nothing -> fail "Either `pass` or `passcmd` is needed."
      Just p -> return (PasswordString p)
    Just c -> return (PasswordCommand (T.unpack c))
  return Config { .. }

-- | Find a specified configuration file by looking in all of the
-- supported locations.
locateConfig :: FilePath -> IO (Maybe FilePath)
locateConfig filename = do
  xdgLocations <- getAllConfigFiles "matterhorn" filename
  let confLocations = ["./" <> filename] ++
                      xdgLocations ++
                      ["/etc/matterhorn/" <> filename]
  results <- forM confLocations $ \fp -> (fp,) <$> doesFileExist fp
  return $ listToMaybe $ fst <$> filter snd results

mainConfigFilename :: FilePath
mainConfigFilename = "config.ini"

findConfig :: IO (Either String Config)
findConfig = do
    let err = "Configuration file " <> show mainConfigFilename <> " not found"
    maybe (return $ Left err) getConfig =<< locateConfig mainConfigFilename

getConfig :: FilePath -> IO (Either String Config)
getConfig fp = runExceptT $ do
  t <- (convertIOException $ readIniFile fp) `catchE`
       (\e -> throwE $ "Could not read " <> show fp <> ": " <> e)
  case t >>= fromIni of
    Left err -> do
      throwE $ "Unable to parse " ++ fp ++ ":" ++ err
    Right conf -> do
      actualPass <- case configPass conf of
        PasswordCommand cmdString -> do
          let (cmd:rest) = words cmdString
          output <- convertIOException (readProcess cmd rest "") `catchE`
                    (\e -> throwE $ "Could not execute password command: " <> e)
          return $ T.pack (takeWhile (/= '\n') output)
        PasswordString pass -> return pass
      return conf { configPass = PasswordString actualPass }
