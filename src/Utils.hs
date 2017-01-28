-- Author: Viacheslav Lotsmanov
-- License: GPLv3 https://raw.githubusercontent.com/unclechu/xlib-keys-hack/master/LICENSE

{-# LANGUAGE PackageImports #-}

module Utils
  ( errPutStrLn
  , errPutStr
  , dieWith

  , writeToFd
  ) where

import "base" GHC.IO.Handle (hFlushAll)
import "base" System.IO
  (Handle, stderr, hPutStrLn, hPutStr, hIsWritable, hPutStr)

-- local imports

import Utils.Sugar ((|?|))


errPutStrLn :: String -> IO ()
errPutStrLn = hPutStrLn stderr

errPutStr :: String -> IO ()
errPutStr = hPutStr stderr

dieWith :: String -> IO a
dieWith = ioError . userError


writeToFd :: Handle -> String -> IO ()
writeToFd fd chunk = hIsWritable fd >>= write |?| tryAgain
  where write = hPutStr fd chunk >> hFlushAll fd
        tryAgain = writeToFd fd chunk
