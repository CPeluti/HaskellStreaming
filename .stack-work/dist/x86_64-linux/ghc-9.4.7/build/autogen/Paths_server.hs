{-# LANGUAGE CPP #-}
{-# LANGUAGE NoRebindableSyntax #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
{-# OPTIONS_GHC -w #-}
module Paths_server (
    version,
    getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir,
    getDataFileName, getSysconfDir
  ) where


import qualified Control.Exception as Exception
import qualified Data.List as List
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

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir `joinFileName` name)

getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath



bindir, libdir, dynlibdir, datadir, libexecdir, sysconfdir :: FilePath
bindir     = "/home/caio/projetos/unb/haskell/projeto-final-grupo-2/.stack-work/install/x86_64-linux/af8c17b7f0619cdbe04e36b5ec8573991b472707823510a87b6f575f6c656b86/9.4.7/bin"
libdir     = "/home/caio/projetos/unb/haskell/projeto-final-grupo-2/.stack-work/install/x86_64-linux/af8c17b7f0619cdbe04e36b5ec8573991b472707823510a87b6f575f6c656b86/9.4.7/lib/x86_64-linux-ghc-9.4.7/server-0.1.0.0-DqEh3HgmdAQJJiIkoRgIls"
dynlibdir  = "/home/caio/projetos/unb/haskell/projeto-final-grupo-2/.stack-work/install/x86_64-linux/af8c17b7f0619cdbe04e36b5ec8573991b472707823510a87b6f575f6c656b86/9.4.7/lib/x86_64-linux-ghc-9.4.7"
datadir    = "/home/caio/projetos/unb/haskell/projeto-final-grupo-2/.stack-work/install/x86_64-linux/af8c17b7f0619cdbe04e36b5ec8573991b472707823510a87b6f575f6c656b86/9.4.7/share/x86_64-linux-ghc-9.4.7/server-0.1.0.0"
libexecdir = "/home/caio/projetos/unb/haskell/projeto-final-grupo-2/.stack-work/install/x86_64-linux/af8c17b7f0619cdbe04e36b5ec8573991b472707823510a87b6f575f6c656b86/9.4.7/libexec/x86_64-linux-ghc-9.4.7/server-0.1.0.0"
sysconfdir = "/home/caio/projetos/unb/haskell/projeto-final-grupo-2/.stack-work/install/x86_64-linux/af8c17b7f0619cdbe04e36b5ec8573991b472707823510a87b6f575f6c656b86/9.4.7/etc"

getBinDir     = catchIO (getEnv "server_bindir")     (\_ -> return bindir)
getLibDir     = catchIO (getEnv "server_libdir")     (\_ -> return libdir)
getDynLibDir  = catchIO (getEnv "server_dynlibdir")  (\_ -> return dynlibdir)
getDataDir    = catchIO (getEnv "server_datadir")    (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "server_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "server_sysconfdir") (\_ -> return sysconfdir)




joinFileName :: String -> String -> FilePath
joinFileName ""  fname = fname
joinFileName "." fname = fname
joinFileName dir ""    = dir
joinFileName dir fname
  | isPathSeparator (List.last dir) = dir ++ fname
  | otherwise                       = dir ++ pathSeparator : fname

pathSeparator :: Char
pathSeparator = '/'

isPathSeparator :: Char -> Bool
isPathSeparator c = c == '/'
