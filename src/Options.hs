{-# LANGUAGE TemplateHaskell #-}

module Options where

import Prelude ()
import Prelude.Compat

import Data.Version (showVersion)
import Development.GitRev
import Network.Mattermost.Version (mmApiVersion)
import Paths_matterhorn (version)
import System.Console.GetOpt
import System.Environment (getArgs)
import System.Exit (exitFailure, exitSuccess)

data Behaviour
  = Normal
  | ShowVersion
  | ShowHelp
    deriving (Eq, Show)

data Options = Options
  { optConfLocation :: Maybe FilePath
  , optLogLocation  :: Maybe FilePath
  , optBehaviour    :: Behaviour
  } deriving (Eq, Show)

defaultOptions :: Options
defaultOptions = Options
  { optConfLocation = Nothing
  , optLogLocation  = Nothing
  , optBehaviour    = Normal
  }

optDescrs :: [OptDescr (Options -> Options)]
optDescrs =
  [ Option ['c'] ["config"]
    (ReqArg (\ path c -> c { optConfLocation = Just path }) "PATH")
    "Path to the configuration file"
  , Option ['l'] ["logs"]
    (ReqArg (\ path c -> c { optLogLocation = Just path }) "PATH")
    "Path to the desired debug logs"
  , Option ['v'] ["version"]
    (NoArg (\ c -> c { optBehaviour = ShowVersion }))
    "Print version information and exit"
  , Option ['h'] ["help"]
    (NoArg (\ c -> c { optBehaviour = ShowHelp }))
    "Print help for command-line flags and exit"
  ]

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
    (_, _, []) -> do
      usage
      exitFailure
    (_, _, errs) -> do
      mapM_ putStr errs
      usage
      exitFailure
