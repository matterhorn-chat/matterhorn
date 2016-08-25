{-# LANGUAGE RecordWildCards #-}

module Config
  ( Config(..)
  , PasswordSource(..)
  , getConfig
  ) where

import           Data.Aeson
import qualified Data.ByteString.Lazy as BS
import           Data.Text (Text)
import qualified Data.Text as T
import           System.Exit (exitFailure)
import           System.Process (readProcess)

data PasswordSource =
    PasswordString Text
    | PasswordCommand String
    deriving (Eq, Read, Show)

data Config = Config
  { configUser     :: Text
  , configHost     :: Text
  , configTeam     :: Text
  , configPort     :: Int
  , configPass     :: PasswordSource
  } deriving (Eq, Show)

instance FromJSON Config where
  parseJSON = withObject "config" $ \o -> do
    configUser <- o .:  "user"
    configHost <- o .:  "host"
    configTeam <- o .:  "team"
    configPort <- o .:  "port"
    passCmd    <- o .:? "passcmd"
    pass       <- o .:? "pass"
    configPass <- case passCmd of
      Nothing -> case pass of
        Nothing     -> fail "Configuration needs either `pass` or `passcmd`"
        Just passwd -> return (PasswordString passwd)
      Just cmd -> return (PasswordCommand cmd)
    return Config { .. }

getConfig :: IO Config
getConfig = do
  bs <- BS.readFile "config.json"
  case decode bs of
    Nothing   -> do
      putStrLn "No config.json found"
      exitFailure
    Just conf -> do
      actualPass <- case configPass conf of
        PasswordCommand cmdString -> do
          let (cmd:rest) = words cmdString
          r <- readProcess cmd rest ""
          return (T.pack (takeWhile (/= '\n') r))
        PasswordString pass -> return pass
      return conf { configPass = PasswordString actualPass }
