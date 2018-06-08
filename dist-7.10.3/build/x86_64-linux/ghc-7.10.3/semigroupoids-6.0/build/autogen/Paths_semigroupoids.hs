module Paths_semigroupoids (
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
version = Version [6,0] []
bindir, libdir, datadir, libexecdir, sysconfdir :: FilePath

bindir     = "/home/dcartwright/.cabal/bin"
libdir     = "/home/dcartwright/.cabal/lib/x86_64-linux-ghc-7.10.3/semigroupoids-6.0-inplace"
datadir    = "/home/dcartwright/.cabal/share/x86_64-linux-ghc-7.10.3/semigroupoids-6.0"
libexecdir = "/home/dcartwright/.cabal/libexec/x86_64-linux-ghc-7.10.3/semigroupoids-6.0/"
sysconfdir = "/home/dcartwright/.cabal/etc"

getBinDir, getLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "semigroupoids_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "semigroupoids_libdir") (\_ -> return libdir)
getDataDir = catchIO (getEnv "semigroupoids_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "semigroupoids_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "semigroupoids_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
