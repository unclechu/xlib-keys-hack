-- Author: Viacheslav Lotsmanov
-- License: GPLv3 https://raw.githubusercontent.com/unclechu/xlib-keys-hack/master/LICENSE

{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE DeriveGeneric #-}

module Bindings.Poll
  ( open
  , close
  , getCErr
  , PollFd(..)
  , poll
  , getInEventFds
  , waitForEvent
  ) where

import Foreign
import Foreign.Storable as Storable
import qualified Foreign.CStorable as CStorable
import qualified Foreign.C.Types as CTypes
import qualified Foreign.C.String as CString
import qualified Foreign.C.Error as CError
import GHC.Generics (Generic)

import Control.Monad (when, unless)
import System.Exit (exitFailure)

import Utils (errPutStrLn, (&), (.>))

#include <poll.h>


type Nfds = CTypes.CULong

foreign import ccall safe "fcntl.h open"
  open :: CString.CString -> CTypes.CInt -> IO CTypes.CInt

foreign import ccall safe "unistd.h close"
  close :: CTypes.CInt -> IO CTypes.CInt

foreign import ccall safe "string.h strerror"
  strerror :: CTypes.CInt -> IO CString.CString

foreign import ccall unsafe "poll.h poll"
  poll :: Ptr PollFd -> Nfds -> CTypes.CInt -> IO CTypes.CInt


getCErr :: IO (CTypes.CInt, String)
getCErr = do
  (CError.Errno errno) <- CError.getErrno
  errCStrPtr <- strerror errno
  str <- CString.peekCString errCStrPtr
  -- it looks like that pointer is pointing on global variable
  -- free errCStrPtr
  return (errno, str)


data PollFd =
  PollFd { fd      :: CTypes.CInt
         , events  :: CTypes.CShort
         , revents :: CTypes.CShort
         } deriving (Show, Generic)

instance CStorable.CStorable PollFd
instance Storable.Storable PollFd where
  alignment = CStorable.cSizeOf
  sizeOf = CStorable.cSizeOf
  peek = CStorable.cPeek
  poke = CStorable.cPoke


pollinEventBit :: CTypes.CShort
pollinEventBit = (#const POLLIN)


-- WARNING! returned pointer must be freed!
getInEventFds :: CTypes.CInt -> IO (Ptr PollFd)
getInEventFds fd =
  new PollFd { fd = fd
             , events = pollinEventBit
             , revents = 0
             }


waitForEvent :: Ptr PollFd -> IO Bool
waitForEvent fdsPtr = do
  x <- poll fdsPtr 1 (-1)
  putStrLn $ "<<<< Poll " ++ show x ++ " >>>>"
  return $ x > (-1)
-- waitForEvent fdsPtr = fmap (> (-1)) $ poll fdsPtr 1 (-1)
