{-# LANGUAGE CPP #-}
{-# LANGUAGE NoRebindableSyntax #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
module Paths_Reversi (
    version,
    getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir,
    getDataFileName, getSysconfDir
  ) where

import qualified Control.Exception as Exception
import Data.Version (Version(..))
import System.Environment (getEnv)
import Prelude

#if defined(VERSION_base)

#if MIN_VERSION_base(4,0,0)
catchIO :: IO a -> (Exception.IOException -> IO a) -> IO a
#else
catchIO :: IO a -> (Exception.Exception -> IO a) -> IO a
#endif

#else
catchIO :: IO a -> (Exception.IOException -> IO a) -> IO a
#endif
catchIO = Exception.catch

version :: Version
version = Version [0,1,0,0] []
bindir, libdir, dynlibdir, datadir, libexecdir, sysconfdir :: FilePath

bindir     = "C:\\Users\\rasha\\Desktop\\Reversi\\.stack-work\\install\\0f3b3bca\\bin"
libdir     = "C:\\Users\\rasha\\Desktop\\Reversi\\.stack-work\\install\\0f3b3bca\\lib\\x86_64-windows-ghc-8.4.3\\Reversi-0.1.0.0-5l4Mms6Y35KKrYYj2KouXA-Reversi"
dynlibdir  = "C:\\Users\\rasha\\Desktop\\Reversi\\.stack-work\\install\\0f3b3bca\\lib\\x86_64-windows-ghc-8.4.3"
datadir    = "C:\\Users\\rasha\\Desktop\\Reversi\\.stack-work\\install\\0f3b3bca\\share\\x86_64-windows-ghc-8.4.3\\Reversi-0.1.0.0"
libexecdir = "C:\\Users\\rasha\\Desktop\\Reversi\\.stack-work\\install\\0f3b3bca\\libexec\\x86_64-windows-ghc-8.4.3\\Reversi-0.1.0.0"
sysconfdir = "C:\\Users\\rasha\\Desktop\\Reversi\\.stack-work\\install\\0f3b3bca\\etc"

getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "Reversi_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "Reversi_libdir") (\_ -> return libdir)
getDynLibDir = catchIO (getEnv "Reversi_dynlibdir") (\_ -> return dynlibdir)
getDataDir = catchIO (getEnv "Reversi_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "Reversi_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "Reversi_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "\\" ++ name)
