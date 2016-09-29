module Options where

import System.Console.GetOpt
import System.Environment (getArgs)
import System.Exit (exitFailure)

data Options = Options
  { optConfLocation :: Maybe FilePath
  , optLogLocation  :: Maybe FilePath
  } deriving (Eq, Show)

defaultOptions :: Options
defaultOptions = Options
  { optConfLocation = Nothing
  , optLogLocation  = Nothing
  }

optDescrs :: [OptDescr (Options -> Options)]
optDescrs =
  [ Option ['c'] ["config"]
    (ReqArg (\ path c -> c { optConfLocation = Just path }) "PATH")
    "Path to the configuration file"
  , Option ['l'] ["logs"]
    (ReqArg (\ path c -> c { optLogLocation = Just path }) "PATH")
    "Path to the desired debug logs"
  ]

usage :: IO ()
usage = putStr (usageInfo "matterhorn" optDescrs)

grabOptions :: IO Options
grabOptions = do
  args <- getArgs
  case getOpt Permute optDescrs args of
    (aps, [], []) ->
      return (foldr (.) id aps defaultOptions)
    (_, _, []) -> do
      usage
      exitFailure
    (_, _, errs) -> do
      mapM_ putStr errs
      usage
      exitFailure
