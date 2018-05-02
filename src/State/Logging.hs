module State.Logging
  ( startLogging
  , stopLogging
  )
where

import           Prelude ()
import           Prelude.MH

import           Types
import           State.Setup.Threads ( startLoggingToFile, stopLoggingToFile )


startLogging :: FilePath -> MH ()
startLogging path = do
    chan <- use (csResources.crLoggingChannel)
    liftIO $ startLoggingToFile chan path

stopLogging :: MH ()
stopLogging = do
    chan <- use (csResources.crLoggingChannel)
    liftIO $ stopLoggingToFile chan
