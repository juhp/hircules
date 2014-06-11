module Hircules.Directories (
--                             dirname, basename,
                             makeDirectory)
where

import Control.Monad (unless)
import System.Directory (createDirectory, doesDirectoryExist)
--import System.FilePath

-- dirname :: FilePath -> FilePath
-- dirname = joinSegments . init . pathSegments

-- basename :: FilePath -> FilePath
-- basename = last . pathSegments

-- pathSegments :: FilePath -> [String]
-- pathSegments fs | last fs == '/' = pathSegments $ init fs
-- pathSegments fs =
--     let (l, s') = break (== '/') fs in
--       (l) : case s' of
-- 	      []      -> []
-- 	      (_:s'') -> pathSegments s''

-- joinSegments :: [String] -> FilePath
-- joinSegments = concatMap (++ "/")

makeDirectory :: FilePath -> IO ()
makeDirectory dir = do
    exists <- doesDirectoryExist dir
    unless exists $
      putStrLn ("creating " ++ dir) >> createDirectory dir
