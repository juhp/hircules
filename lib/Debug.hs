module Debug (debug, setDebug)
where

import IOExts (unsafePerformIO)
import MVar (newMVar, swapMVar, readMVar, MVar)
import Monad (when)
import Control.Monad.Trans (liftIO, MonadIO)

debugging :: MVar Bool
debugging = unsafePerformIO $ newMVar False

setDebug :: Bool -> IO Bool
setDebug st =
    do swapMVar debugging st

debug :: (Show a, MonadIO m) => a -> m ()
debug v =
    do
    dbg <- liftIO $ readMVar debugging
    when dbg $
      liftIO $ putStrLn $ show v
