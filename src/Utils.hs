-- Author: Viacheslav Lotsmanov
-- License: GPLv3 https://raw.githubusercontent.com/unclechu/xlib-keys-hack/master/LICENSE

{-# LANGUAGE PackageImports #-}

module Utils
  ( errPutStrLn
  , errPutStr
  , dieWith
  ) where

import "base" System.IO (stderr, hPutStrLn, hPutStr, hPutStr)


errPutStrLn :: String -> IO ()
errPutStrLn = hPutStrLn stderr

errPutStr :: String -> IO ()
errPutStr = hPutStr stderr

dieWith :: String -> IO a
dieWith = ioError . userError
