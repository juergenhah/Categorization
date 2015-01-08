module Paths_Categorization (
    version,
    getBinDir, getLibDir, getDataDir, getLibexecDir,
    getDataFileName, getSysconfDir
  ) where

import qualified Control.Exception as Exception
import Data.Version (Version(..))
import System.Environment (getEnv)
import Prelude

catchIO :: IO a -> (Exception.IOException -> IO a) -> IO a
catchIO = Exception.catch


version :: Version
version = Version {versionBranch = [0,1,0,0], versionTags = []}
bindir, libdir, datadir, libexecdir, sysconfdir :: FilePath

bindir     = "/home/juergen/.cabal/bin"
libdir     = "/home/juergen/.cabal/lib/x86_64-linux-ghc-7.8.3/Categorization-0.1.0.0"
datadir    = "/home/juergen/.cabal/share/x86_64-linux-ghc-7.8.3/Categorization-0.1.0.0"
libexecdir = "/home/juergen/.cabal/libexec"
sysconfdir = "/home/juergen/.cabal/etc"

getBinDir, getLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "Categorization_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "Categorization_libdir") (\_ -> return libdir)
getDataDir = catchIO (getEnv "Categorization_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "Categorization_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "Categorization_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
