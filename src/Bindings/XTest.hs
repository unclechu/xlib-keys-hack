{-# LANGUAGE ForeignFunctionInterface #-}

module Bindings.XTest (fakeKeyEvent) where

import Control.Monad (when)
import Foreign.C.Types (CULong(CULong), CInt(CInt))
import Graphics.X11.Xlib ( Display(Display)
                         , KeyCode
                         , Status
                         , KeySym
                         , sync )
import Graphics.X11.Xlib.Misc (keysymToKeycode)

foreign import ccall unsafe "X11/extensions/XTest.h XTestFakeKeyEvent"
  xFakeKeyEvent :: Display -> KeyCode -> Bool -> CULong -> IO Status


fakeKeyEvent :: Display -> KeySym -> Bool -> IO ()
fakeKeyEvent dpy keysym state = do
  keycode <- keysymToKeycode dpy keysym
  when (keycode == 0) $ error "keycode not found by keysym"
  xFakeKeyEvent dpy keycode state 0
  sync dpy False
