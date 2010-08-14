module Debug.State (debug, debugDo, setDebug)
where

import Foreign (unsafePerformIO)
import Control.Concurrent.MVar (newMVar, swapMVar, readMVar, MVar)
import Control.Monad (when)
import Control.Monad.Trans (liftIO, MonadIO)

debugging :: MVar Bool
debugging = unsafePerformIO $ newMVar False

setDebug :: Bool -> IO Bool
setDebug st =
    do swapMVar debugging st

debug :: (Show a, MonadIO m) => String -> a -> m ()
debug txt v =
    do
    dbg <- liftIO $ readMVar debugging
    when dbg $
      liftIO $ do
          putStrLn $ txt ++ ": " ++ show v

debugDo :: MonadIO m => m () -> m ()
debugDo act = do
    dbg <- liftIO $ readMVar debugging
    when dbg act
