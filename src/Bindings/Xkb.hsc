-- Author: Viacheslav Lotsmanov
-- License: GPLv3 https://raw.githubusercontent.com/unclechu/xlib-keys-hack/master/LICENSE

{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Bindings.Xkb
  ( xkbGetDescPtr
  , xkbFetchControls
  , xkbGetGroupsCount
  , XkbGetDisplayError(..)
  , xkbGetDisplay
  ) where

import Foreign
import Foreign.Ptr (nullPtr)
import qualified Foreign.Marshal.Alloc as MAlloc
import qualified Foreign.C.Types as CTypes
import qualified Data.Either as Either

import Graphics.X11.Xlib.Types (Display(Display))
import qualified Graphics.X11.Types as XTypes

import qualified Bindings.Xkb.Types as XkbTypes


#include <X11/XKBlib.h>

#define  __INT_SIZE  sizeof(int)


foreign import ccall unsafe "X11/XKBlib.h XkbGetKeyboard"
  xkbGetKeyboard :: Display
                 -> CTypes.CUInt
                 -> CTypes.CUInt
                 -> IO (Ptr XkbTypes.XkbDescRec)

data XkbGetDescPtrError = DescPtrIsNull deriving (Show)

xkbGetDescPtr :: Display
              -> IO (Either.Either XkbGetDescPtrError
                                   (Ptr XkbTypes.XkbDescRec))
xkbGetDescPtr dpy = do
  ptr <- xkbGetKeyboard dpy (#const XkbAllComponentsMask) (#const XkbUseCoreKbd)
  return $ if ptr == nullPtr
              then Either.Left DescPtrIsNull
              else Either.Right ptr


foreign import ccall unsafe "X11/XKBlib.h XkbGetControls"
  xkbGetControls :: Display
                 -> CTypes.CULong
                 -> Ptr XkbTypes.XkbDescRec
                 -> IO CTypes.CInt

xkbFetchControls :: Display -> Ptr XkbTypes.XkbDescRec -> IO Bool
xkbFetchControls dpy descPtr = do
  status <- xkbGetControls dpy (#const XkbAllControlsMask) descPtr
  return $ status == 0


xkbGetGroupsCount :: Ptr XkbTypes.XkbDescRec -> IO Int
xkbGetGroupsCount descPtr = do
  descRec  <- peek descPtr
  ctrlsRec <- peek $ XkbTypes.ctrls descRec
  return $ fromIntegral $ XkbTypes.num_groups ctrlsRec


foreign import ccall unsafe "X11/XKBlib.h XkbOpenDisplay"
  xkbOpenDisplay :: Ptr CTypes.CChar -- display_name
                 -> Ptr CTypes.CInt -- event_rtrn
                 -> Ptr CTypes.CInt -- error_rtrn
                 -> Ptr CTypes.CInt -- major_in_out
                 -> Ptr CTypes.CInt -- minor_in_out
                 -> Ptr CTypes.CInt -- reason_rtrn
                 -> IO (Ptr Display)

data XkbGetDisplayError = BadLibraryVersion
                        | ConnectionRefused
                        | NonXkbServer
                        | BadServerVersion
                        | XkbDisplayIsNull
                        deriving (Show)

xkbGetDisplay :: IO (Either.Either XkbGetDisplayError Display)
xkbGetDisplay = do

  (returnPtr :: Ptr CTypes.CInt) <- MAlloc.mallocBytes (#const __INT_SIZE)
  (xkbDpy :: Ptr Display) <- xkbOpenDisplay
                               nullPtr nullPtr nullPtr
                               nullPtr nullPtr returnPtr
  (returnVal :: CTypes.CInt) <- peek returnPtr
  MAlloc.free returnPtr

  retval <- return $ case returnVal of
    (#const XkbOD_Success)           -> Either.Right $ Display xkbDpy
    (#const XkbOD_BadLibraryVersion) -> Either.Left BadLibraryVersion
    (#const XkbOD_ConnectionRefused) -> Either.Left ConnectionRefused
    (#const XkbOD_NonXkbServer)      -> Either.Left NonXkbServer
    (#const XkbOD_BadServerVersion)  -> Either.Left BadServerVersion
    _ -> error "xkbOpenDisplay: unknown open display error"

  if Either.isRight retval && xkbDpy == nullPtr
     then return $ Either.Left XkbDisplayIsNull
     else return retval
