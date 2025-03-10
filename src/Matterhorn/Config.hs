{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE MultiWayIf #-}
module Matterhorn.Config
  ( Config(..)
  , PasswordSource(..)
  , findConfig
  , configConnectionType
  )
where

import           Prelude ()
import           Matterhorn.Prelude

import qualified Paths_matterhorn as Paths

import           Brick.Keybindings
import qualified Control.Exception as E
import           Control.Monad.Trans.Except
import           Control.Monad.Trans.Class ( lift )
import           Data.Char ( isDigit, isAlpha )
import           Data.List ( isPrefixOf )
import           Data.List.Split ( splitOn )
import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified Graphics.Vty as Vty
import           System.Directory ( makeAbsolute, getHomeDirectory )
import           System.Environment ( getExecutablePath )
import           System.FilePath ( (</>), takeDirectory, splitPath, joinPath )
import           System.Process ( readProcess )
import           Network.Mattermost.Types (ConnectionType(..))
import           Network.URI ( isIPv4address, isIPv6address )

import           Matterhorn.Config.Schema
import           Matterhorn.FilePaths
import           Matterhorn.IOUtil
import           Matterhorn.Types


defaultPort :: Int
defaultPort = 443

bundledSyntaxPlaceholderName :: String
bundledSyntaxPlaceholderName = "BUNDLED_SYNTAX"

userSyntaxPlaceholderName :: String
userSyntaxPlaceholderName = "USER_SYNTAX"

defaultSkylightingPaths :: IO [FilePath]
defaultSkylightingPaths = do
    xdg <- xdgSyntaxDir
    dataDirs <- xdgDataDirs
    adjacent <- getBundledSyntaxPath
    cabalDataFiles <- Paths.getDataFileName syntaxDirName
    return $ [xdg, adjacent, cabalDataFiles] <> dataDirs

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
  section "mattermost" $ do
    configUser           <- fieldMbOf "user" stringField
    configHost           <- fieldMbOf "host" hostField
    configTeam           <- fieldMbOf "team" stringField
    configPort           <- fieldDefOf "port" number (configPort defaultConfig)
    configUrlPath        <- fieldMbOf "urlPath" stringField
    configChannelListWidth <- fieldDefOf "channelListWidth" channelListWidthField
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
    configSmartEditing <- fieldFlagDef "smartediting"
      (configSmartEditing defaultConfig)
    configShowOlderEdits <- fieldFlagDef "showOlderEdits"
      (configShowOlderEdits defaultConfig)
    configShowBackground <- fieldDefOf "showBackgroundActivity" backgroundField
      (configShowBackground defaultConfig)
    configShowMessagePreview <- fieldFlagDef "showMessagePreview"
      (configShowMessagePreview defaultConfig)
    configShowChannelList <- fieldFlagDef "showChannelList"
      (configShowChannelList defaultConfig)
    configShowExpandedChannelTopics <- fieldFlagDef "showExpandedChannelTopics"
      (configShowExpandedChannelTopics defaultConfig)
    configShowTypingIndicator <- fieldFlagDef "showTypingIndicator"
      (configShowTypingIndicator defaultConfig)
    configSendTypingNotifications <- fieldFlagDef "sendTypingNotifications"
      (configSendTypingNotifications defaultConfig)
    configEnableAspell <- fieldFlagDef "enableAspell"
      (configEnableAspell defaultConfig)
    configSyntaxDirs <- fieldDefOf "syntaxDirectories" syntaxDirsField []
    configActivityNotifyCommand <- fieldMb "activityNotifyCommand"
    configActivityNotifyVersion <- fieldDefOf "activityNotifyVersion"
      notifyVersion (configActivityNotifyVersion defaultConfig)
    configShowMessageTimestamps <- fieldFlagDef "showMessageTimestamps"
      (configShowMessageTimestamps defaultConfig)
    configActivityBell <- fieldFlagDef "activityBell"
      (configActivityBell defaultConfig)
    configTruncateVerbatimBlocksInt <- fieldDefOf "truncateVerbatimBlockHeight" number
      (maybe 0 id $ configTruncateVerbatimBlocks defaultConfig)
    configChannelListSorting <- fieldDefOf "channelListSorting"
      parseChannelListSorting (configChannelListSorting defaultConfig)
    configTeamListSorting <- fieldDefOf "teamListSorting"
      parseTeamListSorting (configTeamListSorting defaultConfig)
    let configTruncateVerbatimBlocks = case configTruncateVerbatimBlocksInt of
            i | i <= 0 -> Nothing
              | otherwise -> Just i
    configHyperlinkingMode <- fieldFlagDef "hyperlinkURLs"
      (configHyperlinkingMode defaultConfig)
    configShowLastOpenThread <- fieldFlagDef "showLastOpenThread"
      (configShowLastOpenThread defaultConfig)
    configPass <- (Just . PasswordCommand <$> field "passcmd") <!>
                  (Just . PasswordString  <$> field "pass") <!>
                  pure Nothing
    configChannelListOrientation <- fieldDefOf "channelListOrientation"
        channelListOrientationField
        (configChannelListOrientation defaultConfig)
    configThreadOrientation <- fieldDefOf "threadOrientation"
        threadOrientationField
        (configThreadOrientation defaultConfig)
    configToken <- (Just . TokenCommand  <$> field "tokencmd") <!>
                   pure Nothing
    configOTPToken <- (Just . OTPTokenCommand  <$> field "otptokencmd") <!>
                      pure Nothing
    configUnsafeUseHTTP <-
      fieldFlagDef "unsafeUseUnauthenticatedConnection" False
    configValidateServerCertificate <-
      fieldFlagDef "validateServerCertificate" True
    configDirectChannelExpirationDays <- fieldDefOf "directChannelExpirationDays" number
      (configDirectChannelExpirationDays defaultConfig)
    configDefaultAttachmentPath <- fieldMbOf "defaultAttachmentPath" filePathField
    configMouseMode <- fieldFlagDef "enableMouseMode"
      (configMouseMode defaultConfig)
    configChannelSelectCaseInsensitive <- fieldFlagDef "channelSelectCaseInsensitive"
      (configChannelSelectCaseInsensitive defaultConfig)

    let configAbsPath = Nothing
        configUserKeys = newKeyConfig allEvents [] []
        configCharacterWidths = Nothing
    return Config { .. }

defaultBindings :: [(KeyEvent, [Binding])]
defaultBindings =
    [ (VtyRefreshEvent                  , [ ctrl 'l' ])
    , (ShowHelpEvent                    , [ fn 1 ])
    , (EnterSelectModeEvent             , [ ctrl 's' ])
    , (ReplyRecentEvent                 , [ ctrl 'r' ])
    , (ToggleMessagePreviewEvent        , [ meta 'p' ])
    , (InvokeEditorEvent                , [ meta 'k' ])
    , (EnterFastSelectModeEvent         , [ ctrl 'g' ])
    , (QuitEvent                        , [ ctrl 'q' ])
    , (NextChannelEvent                 , [ ctrl 'n' ])
    , (PrevChannelEvent                 , [ ctrl 'p' ])
    , (NextChannelEventAlternate        , [ bind Vty.KDown ])
    , (PrevChannelEventAlternate        , [ bind Vty.KUp ])
    , (NextUnreadChannelEvent           , [ meta 'a' ])
    , (ShowAttachmentListEvent          , [ ctrl 'x' ])
    , (ChangeMessageEditorFocus         , [ meta 'o' ])
    , (NextUnreadUserOrChannelEvent     , [ ])
    , (LastChannelEvent                 , [ meta 's' ])
    , (EnterOpenURLModeEvent            , [ ctrl 'o' ])
    , (ClearUnreadEvent                 , [ meta 'l' ])
    , (ToggleMultiLineEvent             , [ meta 'e' ])
    , (EnterFlaggedPostsEvent           , [ meta '8' ])
    , (ToggleChannelListVisibleEvent    , [ fn 2 ])
    , (ToggleExpandedChannelTopicsEvent , [ fn 3 ])
    , (CycleChannelListSorting          , [ fn 4 ])
    , (SelectNextTabEvent               , [ bind '\t' ])
    , (SelectPreviousTabEvent           , [ bind Vty.KBackTab ])
    , (SaveAttachmentEvent              , [ bind 's' ])
    , (LoadMoreEvent                    , [ ctrl 'b' ])
    , (ScrollUpEvent                    , [ bind Vty.KUp ])
    , (ScrollDownEvent                  , [ bind Vty.KDown ])
    , (ScrollLeftEvent                  , [ bind Vty.KLeft ])
    , (ScrollRightEvent                 , [ bind Vty.KRight ])
    , (ChannelListScrollUpEvent         , [ ctrl Vty.KUp ])
    , (ChannelListScrollDownEvent       , [ ctrl Vty.KDown ])
    , (PageUpEvent                      , [ bind Vty.KPageUp ])
    , (PageDownEvent                    , [ bind Vty.KPageDown ])
    , (PageLeftEvent                    , [ shift Vty.KLeft ])
    , (PageRightEvent                   , [ shift Vty.KRight ])
    , (ScrollTopEvent                   , [ bind Vty.KHome, meta '<' ])
    , (ScrollBottomEvent                , [ bind Vty.KEnd, meta '>' ])
    , (SelectOldestMessageEvent         , [ shift Vty.KHome ])
    , (SelectUpEvent                    , [ bind 'k', bind Vty.KUp ])
    , (SelectDownEvent                  , [ bind 'j', bind Vty.KDown ])
    , (ActivateListItemEvent            , [ bind Vty.KEnter ])
    , (SearchSelectUpEvent              , [ ctrl 'p', bind Vty.KUp ])
    , (SearchSelectDownEvent            , [ ctrl 'n', bind Vty.KDown ])
    , (ViewMessageEvent                 , [ bind 'v' ])
    , (FillGapEvent                     , [ bind Vty.KEnter ])
    , (CopyPostLinkEvent                , [ bind 'l' ])
    , (FlagMessageEvent                 , [ bind 'f' ])
    , (OpenThreadEvent                  , [ bind 't' ])
    , (PinMessageEvent                  , [ bind 'p' ])
    , (OpenMessageInExternalEditorEvent , [ bind 'O' ])
    , (YankMessageEvent                 , [ bind 'y' ])
    , (YankWholeMessageEvent            , [ bind 'Y' ])
    , (DeleteMessageEvent               , [ bind 'd' ])
    , (EditMessageEvent                 , [ bind 'e' ])
    , (ReplyMessageEvent                , [ bind 'r' ])
    , (ReactToMessageEvent              , [ bind 'a' ])
    , (OpenMessageURLEvent              , [ bind 'o' ])
    , (AttachmentListAddEvent           , [ bind 'a' ])
    , (AttachmentListDeleteEvent        , [ bind 'd' ])
    , (AttachmentOpenEvent              , [ bind 'o' ])
    , (CancelEvent                      , [ bind Vty.KEsc, ctrl 'c' ])
    , (EditorBolEvent                   , [ ctrl 'a' ])
    , (EditorEolEvent                   , [ ctrl 'e' ])
    , (EditorTransposeCharsEvent        , [ ctrl 't' ])
    , (EditorDeleteCharacter            , [ ctrl 'd' ])
    , (EditorKillToBolEvent             , [ ctrl 'u' ])
    , (EditorKillToEolEvent             , [ ctrl 'k' ])
    , (EditorPrevCharEvent              , [ ctrl 'b' ])
    , (EditorNextCharEvent              , [ ctrl 'f' ])
    , (EditorPrevWordEvent              , [ meta 'b' ])
    , (EditorNextWordEvent              , [ meta 'f' ])
    , (EditorDeleteNextWordEvent        , [ meta 'd' ])
    , (EditorDeletePrevWordEvent        , [ ctrl 'w', meta Vty.KBS ])
    , (EditorHomeEvent                  , [ bind Vty.KHome ])
    , (EditorEndEvent                   , [ bind Vty.KEnd ])
    , (EditorYankEvent                  , [ ctrl 'y' ])
    , (FileBrowserBeginSearchEvent      , [ bind '/' ])
    , (FileBrowserSelectEnterEvent      , [ bind Vty.KEnter ])
    , (FileBrowserSelectCurrentEvent    , [ bind ' ' ])
    , (FileBrowserListPageUpEvent       , [ ctrl 'b', bind Vty.KPageUp ])
    , (FileBrowserListPageDownEvent     , [ ctrl 'f', bind Vty.KPageDown ])
    , (FileBrowserListHalfPageUpEvent   , [ ctrl 'u' ])
    , (FileBrowserListHalfPageDownEvent , [ ctrl 'd' ])
    , (FileBrowserListTopEvent          , [ bind 'g', bind Vty.KHome, meta '<' ])
    , (FileBrowserListBottomEvent       , [ bind 'G', bind Vty.KEnd, meta '>' ])
    , (FileBrowserListNextEvent         , [ bind 'j', ctrl 'n', bind Vty.KDown ])
    , (FileBrowserListPrevEvent         , [ bind 'k', ctrl 'p', bind Vty.KUp ])
    , (FormSubmitEvent                  , [ bind Vty.KEnter ])
    , (NextTeamEvent                    , [ ctrl Vty.KRight ])
    , (PrevTeamEvent                    , [ ctrl Vty.KLeft ])
    , (MoveCurrentTeamLeftEvent         , [ ])
    , (MoveCurrentTeamRightEvent        , [ ])
    ]

channelListWidthField :: Text -> Either String ChannelListWidth
channelListWidthField t =
    case T.toLower t of
        "auto" -> return ChannelListWidthAuto
        _ -> case readMaybe (T.unpack t) of
            Nothing -> Left "Invalid value for channelListWidth"
            Just w  -> Right $ ChannelListWidthFixed w

channelListOrientationField :: Text -> Either String ChannelListOrientation
channelListOrientationField t =
    case T.toLower t of
        "left" -> return ChannelListLeft
        "right" -> return ChannelListRight
        _ -> Left $ "Invalid value for channelListOrientation: " <> show t

threadOrientationField :: Text -> Either String ThreadOrientation
threadOrientationField t =
    case T.toLower t of
        "left" -> return ThreadLeft
        "right" -> return ThreadRight
        "above" -> return ThreadAbove
        "below" -> return ThreadBelow
        _ -> Left $ "Invalid value for threadOrientation: " <> show t

syntaxDirsField :: Text -> Either String [FilePath]
syntaxDirsField = listWithSeparator ":" string

validHostnameFragmentChar :: Char -> Bool
validHostnameFragmentChar c = isAlpha c || isDigit c || c == '-'

isHostnameFragment :: String -> Bool
isHostnameFragment "" = False
isHostnameFragment s = all validHostnameFragmentChar s

isHostname :: String -> Bool
isHostname "" = False
isHostname s =
    let parts = splitOn "." s
        h = case parts of
            (p:_) -> p
            [] -> error $ "BUG: isHostname: should always get at least one component: " <> show parts
    in all isHostnameFragment parts && not ("-" `isPrefixOf` h)

hostField :: Text -> Either String Text
hostField t =
    let s = T.unpack t
        valid = or [ isIPv4address s
                   , isIPv6address s
                   , isHostname s
                   ]
    in if valid
       then Right t
       else Left "Invalid 'host' value, must be a hostname or IPv4/IPv6 address"

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

notifyVersion :: Text -> Either String NotificationVersion
notifyVersion t =
    case t of
        "1" -> Right NotifyV1
        "2" -> Right NotifyV2
        _ -> Left ("Invalid value " <> show t
                  <> "; must be one of NotifyV1, NotifyV2")

parseChannelListSorting :: Text -> Either String ChannelListSorting
parseChannelListSorting t =
    let validValues = [ ("default", ChannelListSortDefault)
                      , ("unread-first", ChannelListSortUnreadFirst)
                      ]
    in case lookup (T.unpack $ T.toLower t) validValues of
        Just s -> Right s
        Nothing ->
            Left ("Invalid value " <> show t <> "; must be one of " <> intercalate ", " (fst <$> validValues))

parseTeamListSorting :: Text -> Either String TeamListSorting
parseTeamListSorting t =
    let validValues = [ ("default", TeamListSortDefault)
                      , ("unread-first", TeamListSortUnreadFirst)
                      ]
    in case lookup (T.unpack $ T.toLower t) validValues of
        Just s -> Right s
        Nothing ->
            Left ("Invalid value " <> show t <> "; must be one of " <> intercalate ", " (fst <$> validValues))

cpuUsagePolicy :: Text -> Either String CPUUsagePolicy
cpuUsagePolicy t =
    case T.toLower t of
        "single" -> return SingleCPU
        "multiple" -> return MultipleCPUs
        _ -> Left $ "Invalid CPU usage policy value: " <> show t

stringField :: Text -> Either e Text
stringField t =
    case isQuoted t of
        True -> Right $ parseQuotedString t
        False -> Right t

filePathField :: Text -> Either e FilePath
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

addDefaultKeys :: Config -> Config
addDefaultKeys config =
    config { configUserKeys = newKeyConfig allEvents defaultBindings mempty }

defaultConfig :: Config
defaultConfig = addDefaultKeys $
    Config { configAbsPath                     = Nothing
           , configUser                        = Nothing
           , configHost                        = Nothing
           , configTeam                        = Nothing
           , configPort                        = defaultPort
           , configUrlPath                     = Nothing
           , configPass                        = Nothing
           , configToken                       = Nothing
           , configOTPToken                    = Nothing
           , configTimeFormat                  = Nothing
           , configDateFormat                  = Nothing
           , configTheme                       = Nothing
           , configThemeCustomizationFile      = Nothing
           , configSmartBacktick               = True
           , configSmartEditing                = True
           , configURLOpenCommand              = Nothing
           , configURLOpenCommandInteractive   = False
           , configActivityNotifyCommand       = Nothing
           , configActivityNotifyVersion       = NotifyV1
           , configActivityBell                = False
           , configTruncateVerbatimBlocks      = Nothing
           , configShowMessageTimestamps       = True
           , configShowBackground              = Disabled
           , configShowMessagePreview          = False
           , configShowChannelList             = True
           , configShowExpandedChannelTopics   = True
           , configEnableAspell                = False
           , configAspellDictionary            = Nothing
           , configUnsafeUseHTTP               = False
           , configValidateServerCertificate   = True
           , configChannelListWidth            = ChannelListWidthFixed 22
           , configLogMaxBufferSize            = 200
           , configShowOlderEdits              = True
           , configUserKeys                    = newKeyConfig allEvents [] []
           , configShowTypingIndicator         = False
           , configSendTypingNotifications     = False
           , configHyperlinkingMode            = True
           , configShowLastOpenThread          = False
           , configSyntaxDirs                  = []
           , configDirectChannelExpirationDays = 7
           , configCpuUsagePolicy              = MultipleCPUs
           , configDefaultAttachmentPath       = Nothing
           , configChannelListOrientation      = ChannelListLeft
           , configThreadOrientation           = ThreadBelow
           , configMouseMode                   = False
           , configChannelListSorting          = ChannelListSortDefault
           , configTeamListSorting             = TeamListSortDefault
           , configChannelSelectCaseInsensitive = False
           , configCharacterWidths             = Nothing
           }

findConfig :: Maybe FilePath -> IO (Either String ([String], Config))
findConfig mPath = runExceptT $ do
    -- Load the main configuration
    locatedConfig <- lift $ locateConfig configFileName

    (warns, config) <- case mPath <|> locatedConfig of
        Nothing -> return ([], defaultConfig)
        Just path -> loadConfig path

    config' <- fixupPaths config

    -- If there is a char widths file, load that and add it to the
    -- configuration
    widthsResult <- liftIO loadCharWidths

    case widthsResult of
        Nothing ->
            return (warns, config')
        Just (Left e) ->
            return (warns <> [e], config')
        Just (Right widths) ->
            return (warns, config' { configCharacterWidths = Just widths })

loadCharWidths :: IO (Maybe (Either String CharWidths))
loadCharWidths = do
    locatedPath <- locateConfig charWidthsFileName

    case locatedPath of
        Nothing -> return Nothing
        Just p -> Just <$> parseCharWidthsFile p

parseCharWidthsFile :: FilePath -> IO (Either String CharWidths)
parseCharWidthsFile path = runExceptT $ do
    contents <- ExceptT $ (Right <$> T.readFile path) `E.catch`
                          (\(e::E.SomeException) -> return $ Left $ show e)

    let pairs = catMaybes (parseCharWidth <$> T.lines contents)

    case pairs of
        [] -> throwE $ path <> ": could not read any valid character width entries"
        _ -> return $ newCharWidths pairs

parseCharWidth :: T.Text -> Maybe (Char, Int)
parseCharWidth s =
    case T.words s of
        [ch, widthS] | T.length ch == 1 -> (T.head ch,) <$> readMaybe (T.unpack widthS)
        _ -> Nothing

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

keybindingsSectionName :: Text
keybindingsSectionName = "keybindings"

-- | Given a file path, load a Matterhorn configuration from the
-- specified path, loading any ancillary information such as passwords
-- or tokens from external sources as specified in the configuration.
--
-- Fails with a string error message; otherwise returns a list of
-- warnings generated during the loading process as well as the loaded
-- configuration itself.
loadConfig :: FilePath -> ExceptT String IO ([String], Config)
loadConfig fp = do
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
            throwE $ "Unable to parse " ++ absPath ++ ":" ++ fatalString err
        Right (warns, confNoKeys) -> do
            let mKeys = either (const Nothing) id $ keybindingsFromIni allEvents keybindingsSectionName t'
                kc = newKeyConfig allEvents defaultBindings (fromMaybe mempty mKeys)
                conf = confNoKeys { configUserKeys = kc }

            actualPass <- case configPass conf of
                Just (PasswordCommand cmdString) -> do
                    let (cmd, rest) = case T.unpack <$> T.words cmdString of
                            (a:as) -> (a, as)
                            [] -> error $ "BUG: loadConfig: got empty command string"
                    output <- convertIOException (readProcess cmd rest "") `catchE`
                              (\e -> throwE $ "Could not execute password command: " <> e)
                    return $ Just $ T.pack (takeWhile (/= '\n') output)
                Just (PasswordString pass) -> return $ Just pass
                Nothing -> return Nothing

            actualToken <- case configToken conf of
                Just (TokenCommand cmdString) -> do
                    let (cmd, rest) = case T.unpack <$> T.words cmdString of
                            (a:as) -> (a, as)
                            [] -> error $ "BUG: loadConfig: got empty command string"
                    output <- convertIOException (readProcess cmd rest "") `catchE`
                              (\e -> throwE $ "Could not execute token command: " <> e)
                    return $ Just $ T.pack (takeWhile (/= '\n') output)
                Just (TokenString _) -> error $ "BUG: loadConfig: token in the Config was already a TokenString"
                Nothing -> return Nothing

            actualOTPToken <- case configOTPToken conf of
                Just (OTPTokenCommand cmdString) -> do
                    let (cmd, rest) = case T.unpack <$> T.words cmdString of
                            (a:as) -> (a, as)
                            [] -> error $ "BUG: loadConfig: got empty command string"
                    output <- convertIOException (readProcess cmd rest "") `catchE`
                              (\e -> throwE $ "Could not execute OTP token command: " <> e)
                    return $ Just $ T.pack (takeWhile (/= '\n') output)
                Just (OTPTokenString _) -> error $ "BUG: loadConfig: otptoken in the Config was already a OTPTokenString"
                Nothing -> return Nothing

            let conf' = conf
                  { configPass = PasswordString <$> actualPass
                  , configToken = TokenString <$> actualToken
                  , configOTPToken = OTPTokenString <$> actualOTPToken
                  , configAbsPath = Just absPath
                  }
            return (map warningString warns, conf')

configConnectionType :: Config -> ConnectionType
configConnectionType config
  | configUnsafeUseHTTP config = ConnectHTTP
  | otherwise = ConnectHTTPS (configValidateServerCertificate config)
