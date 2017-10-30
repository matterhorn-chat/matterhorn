{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TupleSections #-}
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
    configUser           <- fieldMbOf "user" stringField
    configHost           <- fieldMbOf "host" stringField
    configTeam           <- fieldMbOf "team" stringField
    configPort           <- fieldDefOf "port" number (configPort defaultConfig)
    configChannelListWidth <- fieldDefOf "channelListWidth" number
                              (configChannelListWidth defaultConfig)
    configTimeFormat     <- fieldMbOf "timeFormat" stringField
    configDateFormat     <- fieldMbOf "dateFormat" stringField
    configTheme          <- fieldMbOf "theme" stringField
    configThemeCustomizationFile <- fieldMbOf "themeCustomizationFile" stringField
    configAspellDictionary <- fieldMbOf "aspellDictionary" stringField
    configURLOpenCommand <- fieldMbOf "urlOpenCommand" stringField
    configURLOpenCommandInteractive <- fieldFlagDef "urlOpenCommandIsInteractive" False
    configSmartBacktick  <- fieldFlagDef "smartbacktick"
      (configSmartBacktick defaultConfig)
    configShowOlderEdits <- fieldFlagDef "showOlderEdits"
      (configShowOlderEdits defaultConfig)
    configShowBackground <- fieldDefOf "showBackgroundActivity" backgroundField
      (configShowBackground defaultConfig)
    configShowMessagePreview <- fieldFlagDef "showMessagePreview"
      (configShowMessagePreview defaultConfig)
    configEnableAspell <- fieldFlagDef "enableAspell"
      (configEnableAspell defaultConfig)
    configActivityBell <- fieldFlagDef "activityBell"
      (configActivityBell defaultConfig)
    configPass <- (Just . PasswordCommand <$> field "passcmd") <|>
                  (Just . PasswordString  <$> field "pass") <|>
                  pure Nothing
    configUnsafeUseHTTP <-
      fieldFlagDef "unsafeUseUnauthenticatedConnection" False
    return Config { .. }

backgroundField :: T.Text -> Either String BackgroundInfo
backgroundField t =
  case t of
    "Disabled" -> Right Disabled
    "Active" -> Right Active
    "ActiveCount" -> Right ActiveCount
    _ -> Left ("Invalid value " <> show t
              <> "; must be one of: Disabled, Active, ActiveCount")

stringField :: T.Text -> Either String T.Text
stringField t =
    case isQuoted t of
        True -> Right $ parseQuotedString t
        False -> Right t

parseQuotedString :: T.Text -> T.Text
parseQuotedString t =
    let body = T.drop 1 $ T.init t
        unescapeQuotes s | T.null s = s
                         | "\\\"" `T.isPrefixOf` s = "\"" <> unescapeQuotes (T.drop 2 s)
                         | otherwise = (T.singleton $ T.head s) <> unescapeQuotes (T.drop 1 s)
    in unescapeQuotes body

isQuoted :: T.Text -> Bool
isQuoted t =
    let quote = "\""
    in (quote `T.isPrefixOf` t) &&
       (quote `T.isSuffixOf` t)

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
           , configThemeCustomizationFile = Nothing
           , configSmartBacktick      = True
           , configURLOpenCommand     = Nothing
           , configURLOpenCommandInteractive = False
           , configActivityBell       = False
           , configShowBackground     = Disabled
           , configShowMessagePreview = False
           , configEnableAspell       = False
           , configAspellDictionary   = Nothing
           , configUnsafeUseHTTP    = False
           , configChannelListWidth = 20
           , configShowOlderEdits     = True
           }

findConfig :: Maybe FilePath -> IO (Either String (Maybe FilePath, Config))
findConfig Nothing = do
    cfg <- locateConfig configFileName
    case cfg of
        Nothing -> return $ Right (Nothing, defaultConfig)
        Just path -> (fmap (Just path,)) <$> getConfig path
findConfig (Just path) = (fmap (Just path,)) <$> getConfig path

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
