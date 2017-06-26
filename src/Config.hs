{-# LANGUAGE RecordWildCards #-}

module Config
  ( Config(..)
  , PasswordSource(..)
  , findConfig
  , getCredentials
  , defaultConfig
  ) where

import           Prelude ()
import           Prelude.Compat

import           Control.Applicative
import           Control.Monad.Trans.Except
import           Data.Ini.Config
import qualified Data.Text as T
import qualified Data.Text.IO as T
import           Data.Monoid ((<>))
import           System.Process (readProcess)

import           IOUtil
import           FilePaths
import           Types

defaultPort :: Int
defaultPort = 443

fromIni :: IniParser Config
fromIni = do
  section "mattermost" $ do
    configUser           <- fieldMb "user"
    configHost           <- fieldMb "host"
    configTeam           <- fieldMb "team"
    configPort           <- fieldDefOf "port" number (configPort defaultConfig)
    configTimeFormat     <- fieldMb "timeFormat"
    configDateFormat     <- fieldMb "dateFormat"
    configTheme          <- fieldMb "theme"
    configAspellDictionary <- fieldMb "aspellDictionary"
    configURLOpenCommand <- fieldMb "urlOpenCommand"
    configSmartBacktick  <- fieldFlagDef "smartbacktick"
      (configSmartBacktick defaultConfig)
    configShowMessagePreview <- fieldFlagDef "showMessagePreview"
      (configShowMessagePreview defaultConfig)
    configEnableAspell <- fieldFlagDef "enableAspell"
      (configEnableAspell defaultConfig)
    configActivityBell <- fieldFlagDef "activityBell"
      (configActivityBell defaultConfig)
    configPass <- (Just . PasswordCommand <$> field "passcmd") <|>
                  (Just . PasswordString  <$> field "pass") <|>
                  pure Nothing
    return Config { .. }

defaultConfig :: Config
defaultConfig =
    Config { configUser               = Nothing
           , configHost               = Nothing
           , configTeam               = Nothing
           , configPort               = defaultPort
           , configPass               = Nothing
           , configTimeFormat         = Nothing
           , configDateFormat         = Nothing
           , configTheme              = Nothing
           , configSmartBacktick      = True
           , configURLOpenCommand     = Nothing
           , configActivityBell       = False
           , configShowMessagePreview = False
           , configEnableAspell       = False
           , configAspellDictionary   = Nothing
           }

findConfig :: Maybe FilePath -> IO (Either String Config)
findConfig Nothing = do
    cfg <- locateConfig configFileName
    case cfg of
        Nothing -> return $ Right defaultConfig
        Just path -> getConfig path
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
        Nothing -> return Nothing

      return conf { configPass = PasswordString <$> actualPass }

-- | Returns the hostname, username, and password from the config. Only
-- returns Just if all three have been provided. The idea is that if
-- this returns Nothing, we're missing at least some of these values.
getCredentials :: Config -> Maybe ConnectionInfo
getCredentials config = do
    pass <- configPass config
    passStr <- case pass of
        PasswordString p -> return p
        PasswordCommand _ ->
            error $ "BUG: unexpected credentials state: " <>
                    show (configPass config)

    ConnectionInfo <$> configHost config
                   <*> (pure $ configPort config)
                   <*> configUser config
                   <*> (pure passStr)
