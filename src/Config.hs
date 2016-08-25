{-# LANGUAGE RecordWildCards #-}

module Config
  ( Config(..)
  , PasswordSource(..)
  , findConfig
  ) where

import qualified Data.HashMap.Strict as HM
import           Data.Ini
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
  configTeam <- HM.lookup "team" cS ?? "team"
  configPort <- readT `fmap` (HM.lookup "port" cS ?? "port")
  let passCmd = HM.lookup "passcmd" cS
  let pass    = HM.lookup "pass" cS
  configPass <- case passCmd of
    Nothing -> case pass of
      Nothing -> fail "Either `pass` or `passcmd` is needed."
      Just p -> return (PasswordString p)
    Just c -> return (PasswordCommand (T.unpack c))
  return Config { .. }

findConfig :: IO Config
findConfig = do
  xdgLocations <- getAllConfigFiles "matterhorn" "config.ini"
  let confLocations = ["./config.ini"] ++ xdgLocations
                                       ++ ["/etc/matterhorn/config.ini"]
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
  t <- readIniFile fp
  case t >>= fromIni of
    Left err -> do
      putStrLn ("Unable to parse " ++ fp ++ ":")
      putStrLn ("  " ++ err)
      exitFailure
    Right conf -> do
      actualPass <- case configPass conf of
        PasswordCommand cmdString -> do
          let (cmd:rest) = words cmdString
          r <- readProcess cmd rest ""
          return (T.pack (takeWhile (/= '\n') r))
        PasswordString pass -> return pass
      return conf { configPass = PasswordString actualPass }
