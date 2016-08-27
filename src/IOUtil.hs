module IOUtil
  ( convertIOException
  ) where

import Control.Exception
import Control.Monad.IO.Class
import Control.Monad.Trans.Except
import System.IO.Error (ioeGetErrorString)

convertIOException :: IO a -> ExceptT String IO a
convertIOException act = do
    result <- liftIO $ (Right <$> act) `catch`
                       (\(e::IOError) -> return $ Left $ ioeGetErrorString e)
    case result of
        Left e -> throwE e
        Right v -> return v
