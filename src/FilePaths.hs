{-# LANGUAGE TupleSections #-}
module FilePaths
  ( historyFilePath
  , historyFileName

  , configFileName

  , xdgName
  , locateConfig

  , locateScriptPath
  , getAllScripts
  ) where

import Control.Applicative
import Control.Monad (forM)
import Data.Monoid ((<>))
import Data.Maybe (listToMaybe)
import System.Directory (doesFileExist, doesDirectoryExist, getDirectoryContents)
import System.Environment.XDG.BaseDir (getUserConfigFile, getAllConfigFiles)

import Prelude

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

scriptDirName :: FilePath
scriptDirName = "scripts"

locateScriptPath :: FilePath -> IO (Maybe FilePath)
locateScriptPath name
  | head name == '.' = return Nothing
  | otherwise = do
    xdgLocations <- getAllConfigFiles "matterhorn" scriptDirName
    let cmdLocations = [ xdgLoc ++ "/" ++ name
                       | xdgLoc <- xdgLocations
                       ] ++ [ "/etc/matterhorn/scripts/" <> name ]
    results <- forM cmdLocations $ \fp -> (fp,) <$> doesFileExist fp
    return $ listToMaybe $ fst <$> filter snd results

getAllScripts :: IO [FilePath]
getAllScripts = do
  xdgLocations <- getAllConfigFiles "matterhorn" scriptDirName
  let cmdLocations = xdgLocations ++ ["/etc/matterhorn/scripts"]
  let getCommands dir = do
        exists <- doesDirectoryExist dir
        if exists
          then getDirectoryContents dir
          else return []
  fmap (filter (\s -> head s /= '.') . concat) $ mapM getCommands cmdLocations
