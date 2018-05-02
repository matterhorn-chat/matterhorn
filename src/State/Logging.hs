module State.Logging
  ( startLogging
  , stopLogging
  , logSnapshot
  , getLogDestination
  )
where

import           Prelude ()
import           Prelude.MH

import           Types


startLogging :: FilePath -> MH ()
startLogging path = do
    mgr <- use (csResources.crLogManager)
    liftIO $ startLoggingToFile mgr path

stopLogging :: MH ()
stopLogging = do
    mgr <- use (csResources.crLogManager)
    liftIO $ stopLoggingToFile mgr

logSnapshot :: FilePath -> MH ()
logSnapshot path = do
    mgr <- use (csResources.crLogManager)
    liftIO $ requestLogSnapshot mgr path

getLogDestination :: MH ()
getLogDestination = do
    mgr <- use (csResources.crLogManager)
    liftIO $ requestLogDestination mgr
