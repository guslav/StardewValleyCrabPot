{-# LANGUAGE CPP #-}
{-# LANGUAGE NoRebindableSyntax #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
{-# OPTIONS_GHC -w #-}
module Paths_StardewValleyCrabPot (
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
bindir     = "/home/gerrit/Nextcloud/haskell/StardewValleyCrabPot/.stack-work/install/x86_64-linux-tinfo6/65d6a6e2c1d83ca03b4e1570308a8a76f6e5969e522a8a6115532a12317665df/9.2.5/bin"
libdir     = "/home/gerrit/Nextcloud/haskell/StardewValleyCrabPot/.stack-work/install/x86_64-linux-tinfo6/65d6a6e2c1d83ca03b4e1570308a8a76f6e5969e522a8a6115532a12317665df/9.2.5/lib/x86_64-linux-ghc-9.2.5/StardewValleyCrabPot-0.1.0.0-5YuJ5eV21St1b0zL7uevYV-StardewValleyCrabPot-exe"
dynlibdir  = "/home/gerrit/Nextcloud/haskell/StardewValleyCrabPot/.stack-work/install/x86_64-linux-tinfo6/65d6a6e2c1d83ca03b4e1570308a8a76f6e5969e522a8a6115532a12317665df/9.2.5/lib/x86_64-linux-ghc-9.2.5"
datadir    = "/home/gerrit/Nextcloud/haskell/StardewValleyCrabPot/.stack-work/install/x86_64-linux-tinfo6/65d6a6e2c1d83ca03b4e1570308a8a76f6e5969e522a8a6115532a12317665df/9.2.5/share/x86_64-linux-ghc-9.2.5/StardewValleyCrabPot-0.1.0.0"
libexecdir = "/home/gerrit/Nextcloud/haskell/StardewValleyCrabPot/.stack-work/install/x86_64-linux-tinfo6/65d6a6e2c1d83ca03b4e1570308a8a76f6e5969e522a8a6115532a12317665df/9.2.5/libexec/x86_64-linux-ghc-9.2.5/StardewValleyCrabPot-0.1.0.0"
sysconfdir = "/home/gerrit/Nextcloud/haskell/StardewValleyCrabPot/.stack-work/install/x86_64-linux-tinfo6/65d6a6e2c1d83ca03b4e1570308a8a76f6e5969e522a8a6115532a12317665df/9.2.5/etc"

getBinDir     = catchIO (getEnv "StardewValleyCrabPot_bindir")     (\_ -> return bindir)
getLibDir     = catchIO (getEnv "StardewValleyCrabPot_libdir")     (\_ -> return libdir)
getDynLibDir  = catchIO (getEnv "StardewValleyCrabPot_dynlibdir")  (\_ -> return dynlibdir)
getDataDir    = catchIO (getEnv "StardewValleyCrabPot_datadir")    (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "StardewValleyCrabPot_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "StardewValleyCrabPot_sysconfdir") (\_ -> return sysconfdir)




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
