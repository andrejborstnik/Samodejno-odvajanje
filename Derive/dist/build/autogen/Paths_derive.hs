module Paths_derive (
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

bindir     = "\\\\Spin\\borstnika$\\_System\\ApplicationData\\cabal\\bin"
libdir     = "\\\\Spin\\borstnika$\\_System\\ApplicationData\\cabal\\i386-windows-ghc-7.6.3\\derive-0.1.0.0"
datadir    = "\\\\Spin\\borstnika$\\_System\\ApplicationData\\cabal\\i386-windows-ghc-7.6.3\\derive-0.1.0.0"
libexecdir = "\\\\Spin\\borstnika$\\_System\\ApplicationData\\cabal\\derive-0.1.0.0"
sysconfdir = "\\\\Spin\\borstnika$\\_System\\ApplicationData\\cabal\\etc"

getBinDir, getLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "derive_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "derive_libdir") (\_ -> return libdir)
getDataDir = catchIO (getEnv "derive_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "derive_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "derive_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "\\" ++ name)
