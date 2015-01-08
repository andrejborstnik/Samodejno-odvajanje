module Paths_Lipschitz (
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
libdir     = "\\\\Spin\\borstnika$\\_System\\ApplicationData\\cabal\\i386-windows-ghc-7.6.3\\Lipschitz-0.1.0.0"
datadir    = "\\\\Spin\\borstnika$\\_System\\ApplicationData\\cabal\\i386-windows-ghc-7.6.3\\Lipschitz-0.1.0.0"
libexecdir = "\\\\Spin\\borstnika$\\_System\\ApplicationData\\cabal\\Lipschitz-0.1.0.0"
sysconfdir = "\\\\Spin\\borstnika$\\_System\\ApplicationData\\cabal\\etc"

getBinDir, getLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "Lipschitz_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "Lipschitz_libdir") (\_ -> return libdir)
getDataDir = catchIO (getEnv "Lipschitz_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "Lipschitz_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "Lipschitz_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "\\" ++ name)
