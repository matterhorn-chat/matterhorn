{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE MultiWayIf #-}
module Config
  ( Config(..)
  , PasswordSource(..)
  , findConfig
  , getCredentials
  , defaultConfig
  )
where

import           Prelude ()
import           Prelude.MH

import           Control.Monad.Trans.Except
import           Control.Monad.Trans.Class ( lift )
import           Control.Monad.IO.Class ( liftIO )
import           Data.Ini.Config
import qualified Data.Map.Strict as M
import qualified Data.Text as T
import qualified Data.Text.IO as T
import           System.Directory ( makeAbsolute, getHomeDirectory )
import           System.Environment ( getExecutablePath )
import           System.FilePath ( (</>), takeDirectory, splitPath, joinPath )
import           System.Process ( readProcess )

import           FilePaths
import           IOUtil
import           Types
import           Types.KeyEvents


defaultPort :: Int
defaultPort = 443

bundledSyntaxPlaceholderName :: String
bundledSyntaxPlaceholderName = "BUNDLED_SYNTAX"

userSyntaxPlaceholderName :: String
userSyntaxPlaceholderName = "USER_SYNTAX"

defaultSkylightingPaths :: IO [FilePath]
defaultSkylightingPaths = do
    xdg <- xdgSyntaxDir
    adjacent <- getBundledSyntaxPath
    return [xdg, adjacent]

getBundledSyntaxPath :: IO FilePath
getBundledSyntaxPath = do
    selfPath <- getExecutablePath
    let distDir = "dist-newstyle/"
        pathBits = splitPath selfPath

    return $ if distDir `elem` pathBits
             then
                 -- We're in development, so use the development
                 -- executable path to locate the XML path in the
                 -- development tree.
                 (joinPath $ takeWhile (/= distDir) pathBits) </> syntaxDirName
             else
                 -- In this case we assume the binary is being run from
                 -- a release, in which case the syntax directory is a
                 -- sibling of the executable path.
                 takeDirectory selfPath </> syntaxDirName

fromIni :: IniParser Config
fromIni = do
  conf <- section "mattermost" $ do
    configUser           <- fieldMbOf "user" stringField
    configHost           <- fieldMbOf "host" stringField
    configTeam           <- fieldMbOf "team" stringField
    configPort           <- fieldDefOf "port" number (configPort defaultConfig)
    configChannelListWidth <- fieldDefOf "channelListWidth" number
                              (configChannelListWidth defaultConfig)
    configCpuUsagePolicy <- fieldDefOf "cpuUsagePolicy" cpuUsagePolicy
                            (configCpuUsagePolicy defaultConfig)
    configLogMaxBufferSize <- fieldDefOf "logMaxBufferSize" number
                              (configLogMaxBufferSize defaultConfig)
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
    configShowChannelList <- fieldFlagDef "showChannelList"
      (configShowChannelList defaultConfig)
    configShowTypingIndicator <- fieldFlagDef "showTypingIndicator"
      (configShowTypingIndicator defaultConfig)
    configEnableAspell <- fieldFlagDef "enableAspell"
      (configEnableAspell defaultConfig)
    configSyntaxDirs <- fieldDefOf "syntaxDirectories" syntaxDirsField []
    configActivityNotifyCommand <- fieldMb "activityNotifyCommand"
    configActivityBell <- fieldFlagDef "activityBell"
      (configActivityBell defaultConfig)
    configHyperlinkingMode <- fieldFlagDef "hyperlinkURLs"
      (configHyperlinkingMode defaultConfig)
    configPass <- (Just . PasswordCommand <$> field "passcmd") <|>
                  (Just . PasswordString  <$> field "pass") <|>
                  pure Nothing
    configUnsafeUseHTTP <-
      fieldFlagDef "unsafeUseUnauthenticatedConnection" False
    configValidateServerCertificate <-
      fieldFlagDef "validateServerCertificate" True
    configDirectChannelExpirationDays <- fieldDefOf "directChannelExpirationDays" number
      (configDirectChannelExpirationDays defaultConfig)
    configDefaultAttachmentPath <- fieldMbOf "defaultAttachmentPath" filePathField

    let configAbsPath = Nothing
        configUserKeys = mempty
    return Config { .. }
  keys <- sectionMb "keybindings" $ do
      fmap (M.fromList . catMaybes) $ forM allEvents $ \ ev -> do
          kb <- fieldMbOf (keyEventName ev) parseBindingList
          case kb of
              Nothing      -> return Nothing
              Just binding -> return (Just (ev, binding))
  return conf { configUserKeys = fromMaybe mempty keys }

syntaxDirsField :: Text -> Either String [FilePath]
syntaxDirsField = listWithSeparator ":" string

expandTilde :: FilePath -> FilePath -> FilePath
expandTilde homeDir p =
    let parts = splitPath p
        f part | part == "~/" = homeDir <> "/"
               | otherwise    = part
    in joinPath $ f <$> parts

backgroundField :: Text -> Either String BackgroundInfo
backgroundField t =
    case t of
        "Disabled" -> Right Disabled
        "Active" -> Right Active
        "ActiveCount" -> Right ActiveCount
        _ -> Left ("Invalid value " <> show t
                  <> "; must be one of: Disabled, Active, ActiveCount")

cpuUsagePolicy :: Text -> Either String CPUUsagePolicy
cpuUsagePolicy t =
    case T.toLower t of
        "single" -> return SingleCPU
        "multiple" -> return MultipleCPUs
        _ -> Left $ "Invalid CPU usage policy value: " <> show t

stringField :: Text -> Either String Text
stringField t =
    case isQuoted t of
        True -> Right $ parseQuotedString t
        False -> Right t

filePathField :: Text -> Either String FilePath
filePathField t = let path = T.unpack t in Right path

parseQuotedString :: Text -> Text
parseQuotedString t =
    let body = T.drop 1 $ T.init t
        unescapeQuotes s | T.null s = s
                         | "\\\"" `T.isPrefixOf` s = "\"" <> unescapeQuotes (T.drop 2 s)
                         | otherwise = (T.singleton $ T.head s) <> unescapeQuotes (T.drop 1 s)
    in unescapeQuotes body

isQuoted :: Text -> Bool
isQuoted t =
    let quote = "\""
    in (quote `T.isPrefixOf` t) &&
       (quote `T.isSuffixOf` t)

defaultConfig :: Config
defaultConfig =
    Config { configAbsPath                     = Nothing
           , configUser                        = Nothing
           , configHost                        = Nothing
           , configTeam                        = Nothing
           , configPort                        = defaultPort
           , configPass                        = Nothing
           , configTimeFormat                  = Nothing
           , configDateFormat                  = Nothing
           , configTheme                       = Nothing
           , configThemeCustomizationFile      = Nothing
           , configSmartBacktick               = True
           , configURLOpenCommand              = Nothing
           , configURLOpenCommandInteractive   = False
           , configActivityNotifyCommand       = Nothing
           , configActivityBell                = False
           , configShowBackground              = Disabled
           , configShowMessagePreview          = False
           , configShowChannelList             = True
           , configEnableAspell                = False
           , configAspellDictionary            = Nothing
           , configUnsafeUseHTTP               = False
           , configValidateServerCertificate   = True
           , configChannelListWidth            = 20
           , configLogMaxBufferSize            = 200
           , configShowOlderEdits              = True
           , configUserKeys                    = mempty
           , configShowTypingIndicator         = False
           , configHyperlinkingMode            = True
           , configSyntaxDirs                  = []
           , configDirectChannelExpirationDays = 7
           , configCpuUsagePolicy              = MultipleCPUs
           , configDefaultAttachmentPath       = Nothing
           }

findConfig :: Maybe FilePath -> IO (Either String Config)
findConfig Nothing = runExceptT $ do
    cfg <- lift $ locateConfig configFileName
    fixupPaths =<< case cfg of
        Nothing -> return defaultConfig
        Just path -> getConfig path
findConfig (Just path) =
    runExceptT $ fixupPaths =<< getConfig path

-- | Fix path references in the configuration:
--
-- * Rewrite the syntax directory path list with 'fixupSyntaxDirs'
-- * Expand "~" encountered in any setting that contains a path value
fixupPaths :: Config -> ExceptT String IO Config
fixupPaths initial = do
    new <- fixupSyntaxDirs initial
    homeDir <- liftIO getHomeDirectory
    let fixP = expandTilde homeDir
        fixPText = T.pack . expandTilde homeDir . T.unpack
    return $ new { configThemeCustomizationFile = fixPText <$> configThemeCustomizationFile new
                 , configSyntaxDirs             = fixP <$> configSyntaxDirs new
                 , configURLOpenCommand         = fixPText <$> configURLOpenCommand new
                 , configActivityNotifyCommand  = fixPText <$> configActivityNotifyCommand new
                 , configDefaultAttachmentPath  = fixP <$> configDefaultAttachmentPath new
                 }

-- | If the configuration has no syntax directories specified (the
-- default if the user did not provide the setting), fill in the
-- list with the defaults. Otherwise replace any bundled directory
-- placeholders in the config's syntax path list.
fixupSyntaxDirs :: Config -> ExceptT String IO Config
fixupSyntaxDirs c =
    if configSyntaxDirs c == []
    then do
        dirs <- liftIO defaultSkylightingPaths
        return $ c { configSyntaxDirs = dirs }
    else do
        newDirs <- forM (configSyntaxDirs c) $ \dir ->
            if | dir == bundledSyntaxPlaceholderName -> liftIO getBundledSyntaxPath
               | dir == userSyntaxPlaceholderName    -> liftIO xdgSyntaxDir
               | otherwise                           -> return dir

        return $ c { configSyntaxDirs = newDirs }

getConfig :: FilePath -> ExceptT String IO Config
getConfig fp = do
    absPath <- convertIOException $ makeAbsolute fp
    t <- (convertIOException $ T.readFile absPath) `catchE`
         (\e -> throwE $ "Could not read " <> show absPath <> ": " <> e)

    -- HACK ALERT FIXME:
    --
    -- The config parser library we use, config-ini (as of 0.2.4.0)
    -- cannot handle configuration files without trailing newlines.
    -- Since that's not a really good reason for this function to raise
    -- an exception (and is fixable on the fly), we have the following
    -- check. This check is admittedly not a great thing to have to do,
    -- and we should definitely get rid of it when config-ini fixes this
    -- issue.
    let t' = if "\n" `T.isSuffixOf` t then t else t <> "\n"

    case parseIniFile t' fromIni of
        Left err -> do
            throwE $ "Unable to parse " ++ absPath ++ ":" ++ err
        Right conf -> do
            actualPass <- case configPass conf of
                Just (PasswordCommand cmdString) -> do
                    let (cmd:rest) = T.unpack <$> T.words cmdString
                    output <- convertIOException (readProcess cmd rest "") `catchE`
                              (\e -> throwE $ "Could not execute password command: " <> e)
                    return $ Just $ T.pack (takeWhile (/= '\n') output)
                Just (PasswordString pass) -> return $ Just pass
                Nothing -> return Nothing

            return conf { configPass = PasswordString <$> actualPass
                        , configAbsPath = Just absPath
                        }

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
