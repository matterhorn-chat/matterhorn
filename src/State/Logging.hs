module State.Logging
  ( startLogging
  , stopLogging
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
