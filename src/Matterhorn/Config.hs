{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE MultiWayIf #-}
module Matterhorn.Config
  ( Config(..)
  , PasswordSource(..)
  , findConfig
  , defaultConfig
  , configConnectionType
  )
where

import           Prelude ()
import           Matterhorn.Prelude

import qualified Paths_matterhorn as Paths

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
import           Matterhorn.Types.KeyEvents


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
    cabalDataFiles <- Paths.getDataFileName syntaxDirName
    return [xdg, adjacent, cabalDataFiles]

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
    configHost           <- fieldMbOf "host" hostField
    configTeam           <- fieldMbOf "team" stringField
    configPort           <- fieldDefOf "port" number (configPort defaultConfig)
    configUrlPath        <- fieldMbOf "urlPath" stringField
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
    configUnsafeUseHTTP <-
      fieldFlagDef "unsafeUseUnauthenticatedConnection" False
    configValidateServerCertificate <-
      fieldFlagDef "validateServerCertificate" True
    configDirectChannelExpirationDays <- fieldDefOf "directChannelExpirationDays" number
      (configDirectChannelExpirationDays defaultConfig)
    configDefaultAttachmentPath <- fieldMbOf "defaultAttachmentPath" filePathField
    configMouseMode <- fieldFlagDef "enableMouseMode"
      (configMouseMode defaultConfig)

    let configAbsPath = Nothing
        configUserKeys = newKeyConfig allEvents [] []
    return Config { .. }
  keys <- sectionMb "keybindings" $ do
      fmap catMaybes $ forM (keyEventsList allEvents) $ \(evName, ev) -> do
          kb <- fieldMbOf evName parseBindingList
          case kb of
              Nothing      -> return Nothing
              Just binding -> return (Just (ev, binding))
  return conf { configUserKeys = newKeyConfig allEvents (fromMaybe mempty keys) defaultBindings }

defaultBindings :: [(KeyEvent, [Binding])]
defaultBindings =
  let meta binding = binding { kbMods = Vty.MMeta : kbMods binding }
      ctrl binding = binding { kbMods = Vty.MCtrl : kbMods binding }
      shift binding = binding { kbMods = Vty.MShift : kbMods binding }
      kb k = Binding { kbMods = [], kbKey = k }
      key c = Binding { kbMods = [], kbKey = Vty.KChar c }
      fn n = Binding { kbMods = [], kbKey = Vty.KFun n }
  in [ (VtyRefreshEvent                  , [ ctrl (key 'l') ])
     , (ShowHelpEvent                    , [ fn 1 ])
     , (EnterSelectModeEvent             , [ ctrl (key 's') ])
     , (ReplyRecentEvent                 , [ ctrl (key 'r') ])
     , (ToggleMessagePreviewEvent        , [ meta (key 'p') ])
     , (InvokeEditorEvent                , [ meta (key 'k') ])
     , (EnterFastSelectModeEvent         , [ ctrl (key 'g') ])
     , (QuitEvent                        , [ ctrl (key 'q') ])
     , (NextChannelEvent                 , [ ctrl (key 'n') ])
     , (PrevChannelEvent                 , [ ctrl (key 'p') ])
     , (NextChannelEventAlternate        , [ kb Vty.KDown ])
     , (PrevChannelEventAlternate        , [ kb Vty.KUp ])
     , (NextUnreadChannelEvent           , [ meta (key 'a') ])
     , (ShowAttachmentListEvent          , [ ctrl (key 'x') ])
     , (ChangeMessageEditorFocus         , [ meta (key 'o') ])
     , (NextUnreadUserOrChannelEvent     , [ ])
     , (LastChannelEvent                 , [ meta (key 's') ])
     , (EnterOpenURLModeEvent            , [ ctrl (key 'o') ])
     , (ClearUnreadEvent                 , [ meta (key 'l') ])
     , (ToggleMultiLineEvent             , [ meta (key 'e') ])
     , (EnterFlaggedPostsEvent           , [ meta (key '8') ])
     , (ToggleChannelListVisibleEvent    , [ fn 2 ])
     , (ToggleExpandedChannelTopicsEvent , [ fn 3 ])
     , (CycleChannelListSorting          , [ fn 4 ])
     , (SelectNextTabEvent               , [ key '\t' ])
     , (SelectPreviousTabEvent           , [ kb Vty.KBackTab ])
     , (SaveAttachmentEvent              , [ key 's' ])
     , (LoadMoreEvent                    , [ ctrl (key 'b') ])
     , (ScrollUpEvent                    , [ kb Vty.KUp ])
     , (ScrollDownEvent                  , [ kb Vty.KDown ])
     , (ScrollLeftEvent                  , [ kb Vty.KLeft ])
     , (ScrollRightEvent                 , [ kb Vty.KRight ])
     , (ChannelListScrollUpEvent         , [ ctrl (kb Vty.KUp) ])
     , (ChannelListScrollDownEvent       , [ ctrl (kb Vty.KDown) ])
     , (PageUpEvent                      , [ kb Vty.KPageUp ])
     , (PageDownEvent                    , [ kb Vty.KPageDown ])
     , (PageLeftEvent                    , [ shift (kb Vty.KLeft) ])
     , (PageRightEvent                   , [ shift (kb Vty.KRight) ])
     , (ScrollTopEvent                   , [ kb Vty.KHome, meta $ key '<' ])
     , (ScrollBottomEvent                , [ kb Vty.KEnd, meta $ key '>' ])
     , (SelectOldestMessageEvent         , [ shift (kb Vty.KHome) ])
     , (SelectUpEvent                    , [ key 'k', kb Vty.KUp ])
     , (SelectDownEvent                  , [ key 'j', kb Vty.KDown ])
     , (ActivateListItemEvent            , [ kb Vty.KEnter ])
     , (SearchSelectUpEvent              , [ ctrl (key 'p'), kb Vty.KUp ])
     , (SearchSelectDownEvent            , [ ctrl (key 'n'), kb Vty.KDown ])
     , (ViewMessageEvent                 , [ key 'v' ])
     , (FillGapEvent                     , [ kb Vty.KEnter ])
     , (CopyPostLinkEvent                , [ key 'l' ])
     , (FlagMessageEvent                 , [ key 'f' ])
     , (OpenThreadEvent                  , [ key 't' ])
     , (PinMessageEvent                  , [ key 'p' ])
     , (YankMessageEvent                 , [ key 'y' ])
     , (YankWholeMessageEvent            , [ key 'Y' ])
     , (DeleteMessageEvent               , [ key 'd' ])
     , (EditMessageEvent                 , [ key 'e' ])
     , (ReplyMessageEvent                , [ key 'r' ])
     , (ReactToMessageEvent              , [ key 'a' ])
     , (OpenMessageURLEvent              , [ key 'o' ])
     , (AttachmentListAddEvent           , [ key 'a' ])
     , (AttachmentListDeleteEvent        , [ key 'd' ])
     , (AttachmentOpenEvent              , [ key 'o' ])
     , (CancelEvent                      , [ kb Vty.KEsc, ctrl (key 'c') ])
     , (EditorBolEvent                   , [ ctrl (key 'a') ])
     , (EditorEolEvent                   , [ ctrl (key 'e') ])
     , (EditorTransposeCharsEvent        , [ ctrl (key 't') ])
     , (EditorDeleteCharacter            , [ ctrl (key 'd') ])
     , (EditorKillToBolEvent             , [ ctrl (key 'u') ])
     , (EditorKillToEolEvent             , [ ctrl (key 'k') ])
     , (EditorPrevCharEvent              , [ ctrl (key 'b') ])
     , (EditorNextCharEvent              , [ ctrl (key 'f') ])
     , (EditorPrevWordEvent              , [ meta (key 'b') ])
     , (EditorNextWordEvent              , [ meta (key 'f') ])
     , (EditorDeleteNextWordEvent        , [ meta (key 'd') ])
     , (EditorDeletePrevWordEvent        , [ ctrl (key 'w'), meta (kb Vty.KBS) ])
     , (EditorHomeEvent                  , [ kb Vty.KHome ])
     , (EditorEndEvent                   , [ kb Vty.KEnd ])
     , (EditorYankEvent                  , [ ctrl (key 'y') ])
     , (FileBrowserBeginSearchEvent      , [ key '/' ])
     , (FileBrowserSelectEnterEvent      , [ kb Vty.KEnter ])
     , (FileBrowserSelectCurrentEvent    , [ kb (Vty.KChar ' ') ])
     , (FileBrowserListPageUpEvent       , [ ctrl (key 'b'), kb Vty.KPageUp ])
     , (FileBrowserListPageDownEvent     , [ ctrl (key 'f'), kb Vty.KPageDown ])
     , (FileBrowserListHalfPageUpEvent   , [ ctrl (key 'u') ])
     , (FileBrowserListHalfPageDownEvent , [ ctrl (key 'd') ])
     , (FileBrowserListTopEvent          , [ key 'g', kb Vty.KHome, meta $ key '<' ])
     , (FileBrowserListBottomEvent       , [ key 'G', kb Vty.KEnd, meta $ key '>' ])
     , (FileBrowserListNextEvent         , [ key 'j', ctrl (key 'n'), kb Vty.KDown ])
     , (FileBrowserListPrevEvent         , [ key 'k', ctrl (key 'p'), kb Vty.KUp ])
     , (FormSubmitEvent                  , [ kb Vty.KEnter ])
     , (NextTeamEvent                    , [ ctrl (kb Vty.KRight) ])
     , (PrevTeamEvent                    , [ ctrl (kb Vty.KLeft) ])
     , (MoveCurrentTeamLeftEvent         , [ ])
     , (MoveCurrentTeamRightEvent        , [ ])
     ]

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

defaultConfig :: Config
defaultConfig =
    Config { configAbsPath                     = Nothing
           , configUser                        = Nothing
           , configHost                        = Nothing
           , configTeam                        = Nothing
           , configPort                        = defaultPort
           , configUrlPath                     = Nothing
           , configPass                        = Nothing
           , configToken                       = Nothing
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
           , configChannelListWidth            = 22
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
           }

findConfig :: Maybe FilePath -> IO (Either String ([String], Config))
findConfig Nothing = runExceptT $ do
    cfg <- lift $ locateConfig configFileName
    (warns, config) <-
      case cfg of
        Nothing -> return ([], defaultConfig)
        Just path -> getConfig path
    config' <- fixupPaths config
    return (warns, config')
findConfig (Just path) =
    runExceptT $ do (warns, config) <- getConfig path
                    config' <- fixupPaths config
                    return (warns, config')

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

getConfig :: FilePath -> ExceptT String IO ([String], Config)
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
            throwE $ "Unable to parse " ++ absPath ++ ":" ++ fatalString err
        Right (warns, conf) -> do
            actualPass <- case configPass conf of
                Just (PasswordCommand cmdString) -> do
                    let (cmd, rest) = case T.unpack <$> T.words cmdString of
                            (a:as) -> (a, as)
                            [] -> error $ "BUG: getConfig: got empty command string"
                    output <- convertIOException (readProcess cmd rest "") `catchE`
                              (\e -> throwE $ "Could not execute password command: " <> e)
                    return $ Just $ T.pack (takeWhile (/= '\n') output)
                Just (PasswordString pass) -> return $ Just pass
                Nothing -> return Nothing

            actualToken <- case configToken conf of
                Just (TokenCommand cmdString) -> do
                    let (cmd, rest) = case T.unpack <$> T.words cmdString of
                            (a:as) -> (a, as)
                            [] -> error $ "BUG: getConfig: got empty command string"
                    output <- convertIOException (readProcess cmd rest "") `catchE`
                              (\e -> throwE $ "Could not execute token command: " <> e)
                    return $ Just $ T.pack (takeWhile (/= '\n') output)
                Just (TokenString _) -> error $ "BUG: getConfig: token in the Config was already a TokenString"
                Nothing -> return Nothing

            let conf' = conf
                  { configPass = PasswordString <$> actualPass
                  , configToken = TokenString <$> actualToken
                  , configAbsPath = Just absPath
                  }
            return (map warningString warns, conf')

configConnectionType :: Config -> ConnectionType
configConnectionType config
  | configUnsafeUseHTTP config = ConnectHTTP
  | otherwise = ConnectHTTPS (configValidateServerCertificate config)
