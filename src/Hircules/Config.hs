module Hircules.Config (configDir, logDir)
where

import System.Environment (getEnv)
import System.FilePath ((</>))
import System.IO.Unsafe (unsafePerformIO)

homeDir :: FilePath
homeDir = unsafePerformIO $ getEnv "HOME"

configDir :: FilePath
configDir = homeDir </> configDirName

configDirName :: FilePath
configDirName = ".hircules"

logDir :: FilePath
logDir = configDir </> logDirName

logDirName :: FilePath
logDirName = "logs"
