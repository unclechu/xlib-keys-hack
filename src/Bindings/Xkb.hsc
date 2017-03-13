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
  , xkbSetGroup
  , xkbGetCurrentLayout
  , xkbListenForKeyboardStateEvents
  ) where

import "base" Foreign
import qualified "base" Foreign.Marshal.Alloc as MAlloc
import qualified "base" Foreign.C.Types as CTypes

import "base" Control.Monad (unless)

import qualified "base" Data.Either as Either

import "X11" Graphics.X11.Xlib (Display(Display), sync)

-- local imports

import qualified Bindings.Xkb.Types as XkbTypes


#include <X11/XKBlib.h>

#define  __INT_SIZE  sizeof(int)

type Status = CTypes.CInt


-- native
foreign import ccall unsafe "X11/XKBlib.h XkbGetKeyboard"
  xkbGetKeyboard :: Display
                 -> CTypes.CUInt
                 -> CTypes.CUInt
                 -> IO (Ptr XkbTypes.XkbDescRec)

data XkbGetDescPtrError = DescPtrIsNull deriving (Show)

-- abstract
xkbGetDescPtr :: Display
              -> IO (Either.Either XkbGetDescPtrError
                                   (Ptr XkbTypes.XkbDescRec))
xkbGetDescPtr dpy = do
  ptr <- xkbGetKeyboard dpy (#const XkbAllComponentsMask) (#const XkbUseCoreKbd)
  return $ if ptr == nullPtr
              then Either.Left DescPtrIsNull
              else Either.Right ptr


-- native
foreign import ccall unsafe "X11/XKBlib.h XkbGetControls"
  xkbGetControls :: Display
                 -> CTypes.CULong
                 -> Ptr XkbTypes.XkbDescRec
                 -> IO Status

-- abstract
xkbFetchControls :: Display -> Ptr XkbTypes.XkbDescRec -> IO Bool
xkbFetchControls dpy descPtr = do
  status <- xkbGetControls dpy (#const XkbAllControlsMask) descPtr
  return $ status == 0


-- abstract
xkbGetGroupsCount :: Ptr XkbTypes.XkbDescRec -> IO Int
xkbGetGroupsCount descPtr = do
  descRec  <- peek descPtr
  ctrlsRec <- peek $ XkbTypes.ctrls descRec
  return $ fromIntegral $ XkbTypes.num_groups ctrlsRec


-- native
foreign import ccall unsafe "X11/XKBlib.h XkbOpenDisplay"
  xkbOpenDisplay :: Ptr CTypes.CChar -- display_name
                 -> Ptr CTypes.CInt  -- event_rtrn
                 -> Ptr CTypes.CInt  -- error_rtrn
                 -> Ptr CTypes.CInt  -- major_in_out
                 -> Ptr CTypes.CInt  -- minor_in_out
                 -> Ptr CTypes.CInt  -- reason_rtrn
                 -> IO (Ptr Display)

data XkbGetDisplayError = BadLibraryVersion
                        | ConnectionRefused
                        | NonXkbServer
                        | BadServerVersion
                        | XkbDisplayIsNull
                        deriving (Show)

-- abstract
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


-- native
foreign import ccall unsafe "X11/XKBlib.h XkbLockGroup"
  xkbLockGroup :: Display
               -> CTypes.CUInt -- deviceSpec
               -> CTypes.CUInt -- group
               -> IO Bool

-- abstract
xkbSetGroup :: Display -> CTypes.CUInt -> IO Bool
xkbSetGroup dpy groupNum = do
  result <- xkbLockGroup dpy (#const XkbUseCoreKbd) groupNum
  sync dpy False
  return result



-- native
foreign import ccall unsafe "X11/XKBlib.h XkbGetState"
  xkbGetState :: Display -> CTypes.CUInt -> Ptr XkbTypes.XkbStateRec
              -> IO Status

-- abstract
xkbGetCurrentLayout :: Display -> IO Int
xkbGetCurrentLayout dpy = alloca $ \stRecPtr -> do

  -- Status is ignored here.
  -- For some reason it returns 0 (zero) that means error but it works okay.
  _ <- xkbGetState dpy (#const XkbUseCoreKbd) stRecPtr

  fromIntegral . XkbTypes.group <$> peek stRecPtr



-- native
foreign import ccall unsafe "X11/XKBlib.h XkbSelectEventDetails"
  xkbSelectEventDetails :: Display
                        -> CTypes.CUInt  -- device_spec
                        -> CTypes.CUInt  -- event_type
                        -> CTypes.CULong -- bits_to_change
                        -> CTypes.CULong -- values_for_bits
                        -> IO Bool

-- native
foreign import ccall unsafe "X11/XKBlib.h XkbSelectEvents"
  xkbSelectEvents :: Display
                  -> CTypes.CUInt  -- device_spec
                  -> CTypes.CULong -- bits_to_change
                  -> CTypes.CULong -- values_for_bits
                  -> IO Bool

-- abstract
xkbListenForKeyboardStateEvents :: Display -> IO ()
xkbListenForKeyboardStateEvents dpy = do
  -- Listen for group changes
  flip unless (error "XkbSelectEventDetails error")
    =<< xkbSelectEventDetails dpy (#const XkbUseCoreKbd)
                                  (#const XkbStateNotify)
                                  (#const XkbAllStateComponentsMask)
                                  (#const XkbGroupStateMask)
  -- Layout/geometry changes
  flip unless (error "XkbSelectEvents error")
    =<< xkbSelectEvents dpy (#const XkbUseCoreKbd)
                            (#const XkbNewKeyboardNotifyMask)
                            (#const XkbNewKeyboardNotifyMask)
