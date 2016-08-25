{-# LANGUAGE RecordWildCards #-}

module Config
  ( Config(..)
  , PasswordSource(..)
  , findConfig
  ) where

import           Control.Applicative ((<|>))
import           Data.Aeson
import qualified Data.ByteString.Lazy as BS
import           Data.Text (Text)
import qualified Data.Text as T
import           System.Directory (doesFileExist)
import           System.Environment.XDG.BaseDir (getAllConfigFiles)
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
    configUser <- o .: "user"
    configHost <- o .: "host"
    configTeam <- o .: "team"
    configPort <- o .: "port"

    passCmd    <- (PasswordCommand <$>) <$> o .:? "passcmd"
    pass       <- (PasswordString <$>)  <$> o .:? "pass"
    let failPasswordRequired = fail "Configuration needs either `pass` or `passcmd`"
    configPass <- maybe failPasswordRequired return $ passCmd <|> pass

    return Config { .. }

findConfig :: IO Config
findConfig = do
  xdgLocations <- getAllConfigFiles "matterhorn" "config.json"
  let confLocations = ["./config.json"] ++ xdgLocations
                                        ++ ["/etc/matterhorn/config.json"]
  loop confLocations
  where loop [] = do
          putStrLn "No matterhorn configuration found"
          exitFailure
        loop (c:cs) = do
          ex <- doesFileExist c
          if ex
            then getConfig c
            else loop cs

getConfig :: FilePath -> IO Config
getConfig fp = do
  bs <- BS.readFile fp
  case decode bs of
    Nothing   -> do
      putStrLn ("Unable to parse " ++ fp)
      exitFailure
    Just conf -> do
      actualPass <- case configPass conf of
        PasswordCommand cmdString -> do
          let (cmd:rest) = words cmdString
          r <- readProcess cmd rest ""
          return (T.pack (takeWhile (/= '\n') r))
        PasswordString pass -> return pass
      return conf { configPass = PasswordString actualPass }
