-- Author: Viacheslav Lotsmanov
-- License: GPLv3 https://raw.githubusercontent.com/unclechu/xlib-keys-hack/master/LICENSE

{-# LANGUAGE ForeignFunctionInterface #-}

module Bindings.XTest
  ( fakeKeyEvent
  , fakeKeyCodeEvent
  ) where

import Control.Monad (when)
import Foreign.C.Types ( CULong(CULong)
                       , CInt(CInt)
                       )
import Graphics.X11.Xlib ( Display(Display)
                         , KeyCode
                         , Status
                         , KeySym
                         , sync
                         )
import Graphics.X11.Xlib.Misc (keysymToKeycode)

foreign import ccall unsafe "X11/extensions/XTest.h XTestFakeKeyEvent"
  xFakeKeyEvent :: Display -> KeyCode -> Bool -> CULong -> IO Status


fakeKeyEvent :: Display -> KeySym -> Bool -> IO ()
fakeKeyEvent dpy keySym state = do
  keyCode <- keysymToKeycode dpy keySym
  when (keyCode == 0) $ error "KeyCode not found by KeySym"
  fakeKeyCodeEvent dpy keyCode state


fakeKeyCodeEvent :: Display -> KeyCode -> Bool -> IO ()
fakeKeyCodeEvent dpy keyCode state = do
  xFakeKeyEvent dpy keyCode state 0
  sync dpy False
