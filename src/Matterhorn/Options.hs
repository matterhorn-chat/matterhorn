{-# LANGUAGE TemplateHaskell #-}

module Matterhorn.Options where

import Prelude ()
import Matterhorn.Prelude

import Data.Char ( toLower )
import Data.Foldable (traverse_)
import Data.Tuple ( swap )
import Data.Version ( showVersion )
import Development.GitRev
import Network.Mattermost.Version ( mmApiVersion )
import Paths_matterhorn ( version )
import System.Console.GetOpt
import System.Environment ( getArgs )
import System.Exit ( exitFailure, exitSuccess )
import System.IO ( hPutStrLn, stderr )

import Matterhorn.Config


data Behaviour
  = Normal
  | ShowVersion
  | ShowHelp
  | CheckConfig
    deriving (Eq, Show)

data PrintFormat =
    Markdown | Plain deriving (Eq, Show)

data Options = Options
  { optConfLocation     :: Maybe FilePath
  , optLogLocation      :: Maybe FilePath
  , optBehaviour        :: Behaviour
  , optIgnoreConfig     :: Bool
  , optPrintKeybindings :: Bool
  , optPrintCommands    :: Bool
  , optPrintFormat      :: PrintFormat
  } deriving (Eq, Show)

defaultOptions :: Options
defaultOptions = Options
  { optConfLocation     = Nothing
  , optLogLocation      = Nothing
  , optBehaviour        = Normal
  , optIgnoreConfig     = False
  , optPrintKeybindings = False
  , optPrintCommands    = False
  , optPrintFormat      = Plain
  }

optDescrs :: [OptDescr (Options -> Options)]
optDescrs =
  [ Option ['c'] ["config"]
    (ReqArg (\ path c -> c { optConfLocation = Just path }) "PATH")
    "Path to the configuration file"
  , Option ['l'] ["logs"]
    (ReqArg (\ path c -> c { optLogLocation = Just path }) "FILE")
    "Path to debug log output file"
  , Option ['v'] ["version"]
    (NoArg (\ c -> c { optBehaviour = ShowVersion }))
    "Print version information and exit"
  , Option ['h'] ["help"]
    (NoArg (\ c -> c { optBehaviour = ShowHelp }))
    "Print help for command-line flags and exit"
  , Option ['i'] ["ignore-config"]
    (NoArg (\ c -> c { optIgnoreConfig = True }))
    "Start with no configuration"
  , Option ['k'] ["keybindings"]
    (NoArg (\ c -> c { optPrintKeybindings = True }))
    "Print keybindings effective for the current configuration"
  , Option ['m'] ["commands"]
    (NoArg (\ c -> c { optPrintCommands = True }))
    "Print available commands"
  , Option ['f'] ["format"]
    (ReqArg handleFormat "FORMAT")
    ("Print keybinding or command output in the specified format " <>
     "(options: " <> formatChoicesStr <> ", default: " <>
     formatStringFor (optPrintFormat defaultOptions) <> ")")
  , Option [] ["check-config"]
    (NoArg (\ c -> c { optBehaviour = CheckConfig }))
    "Validate configuration file"
  ]

formatChoices :: [(String, PrintFormat)]
formatChoices =
    [ ("plain", Plain)
    , ("markdown", Markdown)
    ]

formatStringFor :: PrintFormat -> String
formatStringFor fmt =
    case lookup fmt (swap <$> formatChoices) of
        Nothing -> error $ "BUG: no format string for " <> show fmt
        Just s -> s

formatChoicesStr :: String
formatChoicesStr = intercalate ", " $ fst <$> formatChoices

handleFormat :: String -> Options -> Options
handleFormat fmtStr c =
    let fmt = case lookup (toLower <$> fmtStr) formatChoices of
            Just f -> f
            Nothing ->
                error $ "Invalid format: " <> show fmtStr <> ", choices: " <>
                        formatChoicesStr
    in c { optPrintFormat = fmt }

mhVersion :: String
mhVersion
  | $(gitHash) == ("UNKNOWN" :: String) = "matterhorn " ++ showVersion version
  | otherwise = "matterhorn " ++ showVersion version ++ " (" ++
                $(gitBranch) ++ "@" ++ take 7 $(gitHash) ++ ")"

fullVersionString :: String
fullVersionString = mhVersion ++ "\n using " ++ mmApiVersion

usage :: IO ()
usage = putStr (usageInfo "matterhorn" optDescrs)

grabOptions :: IO Options
grabOptions = do
  args <- getArgs
  case getOpt Permute optDescrs args of
    (aps, [], []) -> do
      let rs = foldr (.) id aps defaultOptions
      case optBehaviour rs of
        Normal -> return rs
        ShowHelp -> usage >> exitSuccess
        ShowVersion -> putStrLn fullVersionString >> exitSuccess
        CheckConfig -> checkConfiguration (optConfLocation rs)
    (_, _, errs) -> do
      mapM_ putStr errs
      usage
      exitFailure

checkConfiguration :: Maybe FilePath -> IO a
checkConfiguration mb =
  do res <- findConfig mb
     let writeLn = hPutStrLn stderr
         printLocation Nothing = "No configuration file"
         printLocation (Just fp) = "Location: " ++ fp
     case res of
       Left e ->
         do writeLn e
            exitFailure
       Right ([], config) ->
         do writeLn "Configuration file valid"
            writeLn (printLocation (configAbsPath config))
            exitSuccess
       Right (ws, config) ->
         do writeLn "Configuration file generated warnings"
            writeLn (printLocation (configAbsPath config))
            traverse_ writeLn ws
            exitFailure
