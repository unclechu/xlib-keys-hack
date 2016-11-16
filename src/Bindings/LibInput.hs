-- Author: Viacheslav Lotsmanov
-- License: GPLv3 https://raw.githubusercontent.com/unclechu/xlib-keys-hack/master/LICENSE

{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE DeriveGeneric #-}

module Bindings.LibInput
  ( doStuff
  ) where


import Foreign
import Foreign.Storable as Storable
import qualified Foreign.CStorable as CStorable
import qualified Foreign.C.Types as CTypes
import qualified Foreign.C.String as CString

import GHC.Generics (Generic)

import System.Exit (exitFailure)
import System.IO.Unsafe (unsafePerformIO, unsafeInterleaveIO)

import Control.Monad (when, unless)

import Utils (errPutStrLn, (&), (.>))
import qualified Bindings.Poll as Poll


type OpenRestrictedFun
   = CString.CString
  -> CTypes.CInt
  -> Ptr ()
  -> IO CTypes.CInt

type CloseRestrictedFun
   = CTypes.CInt
  -> Ptr ()
  -> IO ()

data LibInput = LibInput deriving (Show)
data Device   = Device   deriving (Show)
data Event    = Event    deriving (Show)

data Interface =
  Interface { open_restricted  :: FunPtr OpenRestrictedFun
            , close_restricted :: FunPtr CloseRestrictedFun
            }
  deriving (Show, Generic)

instance CStorable.CStorable Interface
instance Storable.Storable Interface where
  alignment = CStorable.cSizeOf
  sizeOf = CStorable.cSizeOf
  peek = CStorable.cPeek
  poke = CStorable.cPoke



foreign import ccall safe "libinput.h libinput_path_create_context"
  pathCreateContext :: Ptr Interface -> Ptr () -> IO (Ptr LibInput)

foreign import ccall safe "libinput.h libinput_path_add_device"
  pathAddDevice :: Ptr LibInput -> CString.CString -> IO (Ptr Device)

foreign import ccall safe "libinput.h libinput_unref"
  unref :: Ptr LibInput -> IO (Ptr LibInput)

foreign import ccall safe "libinput.h libinput_get_fd"
  getFd :: Ptr LibInput -> IO CTypes.CInt

foreign import ccall safe "libinput.h libinput_dispatch"
  dispatch :: Ptr LibInput -> IO CTypes.CInt


foreign import ccall "wrapper"
  allocOpenRestrictedFun :: OpenRestrictedFun -> IO (FunPtr OpenRestrictedFun)

foreign import ccall "wrapper"
  allocCloseRestrictedFun :: CloseRestrictedFun
                          -> IO (FunPtr CloseRestrictedFun)


sureUnref :: Ptr LibInput -> IO ()
sureUnref libInputPtr =
  unref libInputPtr
    >>= return . (== nullPtr)
    >>= flip unless (errPutStrLn "LibInput: unref fail" >> exitFailure)

sureCreateContext :: IO () -> Ptr Interface -> IO (Ptr LibInput)
sureCreateContext unref interfacePtr = do
  libInputPtr <- pathCreateContext interfacePtr nullPtr
  when (libInputPtr == nullPtr) $ do
    errPutStrLn "LibInput: create context fail"
    unref >> exitFailure
  return libInputPtr

sureAddDevice :: IO () -> Ptr LibInput -> String -> IO (Ptr Device)
sureAddDevice unref libInputPtr path = do
  devicePtr <- CString.withCString path $ pathAddDevice libInputPtr
  when (devicePtr == nullPtr) $ do
    errPutStrLn "LibInput: add device by path fail"
    unref >> exitFailure
  return devicePtr

sureGetFd :: IO () -> Ptr LibInput -> IO CTypes.CInt
sureGetFd unref libInputPtr = do
  fd <- getFd libInputPtr
  when (fd < 0) $ do
    errPutStrLn "LibInput: get file descriptor fail"
    unref >> exitFailure
  return fd

sureDispatch :: IO () -> Ptr LibInput -> IO ()
sureDispatch unref libInputPtr = do
  dispatch libInputPtr
    >>= return . (< 0)
    >>= flip when
          (do
            (errno, errMsg) <- Poll.getCErr
            errPutStrLn $ "LibInput: dispatch fail (" ++ errMsg ++ ")"
            unref >> exitFailure
          )
  return ()

doStuff :: IO ()
doStuff = do

  ptrOpenRestrictedFun  <- allocOpenRestrictedFun  openRestricted
  ptrCloseRestrictedFun <- allocCloseRestrictedFun closeRestricted

  let interface = Interface { open_restricted  = ptrOpenRestrictedFun
                            , close_restricted = ptrCloseRestrictedFun
                            }

  interfacePtr <- new interface
  libInputPtr <- sureCreateContext (free interfacePtr) interfacePtr
  let unref = sureUnref libInputPtr >> free interfacePtr

  devicePtr <-
    sureAddDevice unref
                  libInputPtr
                  "/dev/input/by-id/usb-1d57_2.4G_Receiver-event-kbd"

  fd <- sureGetFd unref libInputPtr
  fdsPtr <- Poll.getInEventFds fd

  -- event loop
  evParse unref libInputPtr fdsPtr

  unref >> free fdsPtr

  where openRestricted :: OpenRestrictedFun
        openRestricted pathPtr flags _ = do
          putStrLn "... opening ..."
          path <- CString.peekCString pathPtr
          fd <- Poll.open pathPtr flags
          (errno, errMsg) <- Poll.getCErr
          when (fd < 0) $ errPutStrLn $
            "LibInput: failed to open " ++ path ++ " (" ++ errMsg ++ ")"
          return $ if fd < 0 then -errno else fd

        closeRestricted :: CloseRestrictedFun
        closeRestricted fd _ = do
          putStrLn "... closing ..."
          Poll.close fd
            >>= return . (< 0)
            >>= flip when (do errPutStrLn "LibInput: close file descriptor fail"
                              exitFailure)

        evParse unref libInputPtr fdsPtr = do
          x <- Poll.waitForEvent fdsPtr
          putStrLn $ "=== is event okay: " ++ show x ++ " ==="
          when x $ sureDispatch unref libInputPtr
          evParse unref libInputPtr fdsPtr
