{-# LANGUAGE RecordWildCards #-}

module Config
  ( Config(..)
  , PasswordSource(..)
  , findConfig
  , getCredentials
  ) where

import           Control.Monad.Trans.Except
import           Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HM
import           Data.Ini
import           Data.Text (Text)
import qualified Data.Text as T
import           Data.Monoid ((<>))
import           System.Process (readProcess)
import           Text.Read (readMaybe)

import           IOUtil
import           FilePaths

-- These helper functions make our Ini parsing a LOT nicer
type IniParser s a = ExceptT String ((->) (Text, s)) a
type Section = HashMap Text Text

-- Run the parser over an Ini file
runParse :: IniParser Ini a -> Ini -> Either String a
runParse mote ini = runExceptT mote ("", ini)

-- Run parsing within a named section
section :: Text -> IniParser Section a -> IniParser Ini a
section name thunk = ExceptT $ \(_, Ini ini) ->
  case HM.lookup name ini of
    Nothing  -> Left ("No section named" ++ show name)
    Just sec -> runExceptT thunk (name, sec)

-- Retrieve a field, returning Nothing if it doesn't exist
fieldM :: Text -> IniParser Section (Maybe Text)
fieldM name = ExceptT $ \(_,m) ->
  return (HM.lookup name m)

-- Retrieve a field, failing to parse if it doesn't exist
field :: Text -> IniParser Section Text
field name = ExceptT $ \(sec,m) ->
  case HM.lookup name m of
    Nothing -> Left ("Missing field " ++ show name ++
                     " in section " ++ show sec)
    Just x  -> return x

-- Retrieve a field and try to 'Read' it to a value, failing
-- to parse if it doesn't exist or if the 'Read' operation
-- fails.
fieldR :: Read a => Text -> IniParser Section a
fieldR name = do
  str <- field name
  case readMaybe (T.unpack str) of
    Just x  -> return x
    Nothing -> fail ("Unable to read field " ++ show name)

fieldMR :: Read a => Text -> IniParser Section (Maybe a)
fieldMR name = do
  mb <- fieldM name
  return $ case mb of
    Nothing  -> Nothing
    Just str -> readMaybe (T.unpack str)

data PasswordSource =
    PasswordString Text
    | PasswordCommand Text
    deriving (Eq, Read, Show)

data Config = Config
  { configUser          :: Maybe Text
  , configHost          :: Text
  , configTeam          :: Maybe Text
  , configPort          :: Int
  , configPass          :: Maybe PasswordSource
  , configTimeFormat    :: Maybe Text
  , configTheme         :: Maybe Text
  , configSmartBacktick :: Bool
  } deriving (Eq, Show)

fromIni :: Ini -> Either String Config
fromIni = runParse $ do
  section "mattermost" $ do
    configUser       <- fieldM  "user"
    configHost       <- field   "host"
    configTeam       <- fieldM  "team"
    configPort       <- fieldR  "port"
    configTimeFormat <- fieldM  "timeFormat"
    configTheme      <- fieldM  "theme"
    pass             <- fieldM  "pass"
    passCmd          <- fieldM  "passcmd"
    smartBacktick    <- fieldMR "smartbacktick"
    let configPass = case passCmd of
          Nothing -> case pass of
            Nothing -> Nothing
            Just p  -> Just (PasswordString p)
          Just c -> Just (PasswordCommand c)
        configSmartBacktick = case smartBacktick of
          Nothing -> True
          Just b  -> b
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
