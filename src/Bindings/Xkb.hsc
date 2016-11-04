-- Author: Viacheslav Lotsmanov
-- License: GPLv3 https://raw.githubusercontent.com/unclechu/xlib-keys-hack/master/LICENSE

{-# LANGUAGE ForeignFunctionInterface #-}

module Bindings.Xkb
  ( xkbGetDescPtr
  , xkbFetchControls
  , xkbIsDescPtrNotNull
  , xkbGetGroupsCount
  ) where

import Foreign
import Foreign.Ptr (nullPtr)
import qualified Foreign.C.Types as CTypes
import System.IO.Unsafe (unsafePerformIO)

import Graphics.X11.Xlib.Types (Display(Display))
import qualified Graphics.X11.Types as XTypes

import qualified Bindings.Xkb.Types as XkbTypes


#include <X11/XKBlib.h>


foreign import ccall unsafe "X11/XKBlib.h XkbGetKeyboard"
  xkbGetKeyboard :: Display
                 -> CTypes.CUInt
                 -> CTypes.CUInt
                 -> IO (Ptr XkbTypes.XkbDescRec)

xkbGetDescPtr :: Display -> IO (Ptr XkbTypes.XkbDescRec)
xkbGetDescPtr dpy = xkbGetKeyboard dpy
                                   (#const XkbAllComponentsMask)
                                   (#const XkbUseCoreKbd)

xkbIsDescPtrNotNull :: Ptr XkbTypes.XkbDescRec -> Bool
xkbIsDescPtrNotNull = (/= nullPtr)


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
