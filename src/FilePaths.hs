{-# LANGUAGE TupleSections #-}
module FilePaths
  ( historyFilePath
  , historyFileName

  , configFileName

  , xdgName
  , locateConfig
  ) where

import Control.Monad (forM)
import Data.Monoid ((<>))
import Data.Maybe (listToMaybe)
import System.Directory (doesFileExist)
import System.Environment.XDG.BaseDir (getUserConfigFile, getAllConfigFiles)

xdgName :: String
xdgName = "matterhorn"

historyFileName :: FilePath
historyFileName = "history.txt"

configFileName :: FilePath
configFileName = "config.ini"

historyFilePath :: IO FilePath
historyFilePath = getUserConfigFile xdgName historyFileName

-- | Find a specified configuration file by looking in all of the
-- supported locations.
locateConfig :: FilePath -> IO (Maybe FilePath)
locateConfig filename = do
  xdgLocations <- getAllConfigFiles "matterhorn" filename
  let confLocations = ["./" <> filename] ++
                      xdgLocations ++
                      ["/etc/matterhorn/" <> filename]
  results <- forM confLocations $ \fp -> (fp,) <$> doesFileExist fp
  return $ listToMaybe $ fst <$> filter snd results
