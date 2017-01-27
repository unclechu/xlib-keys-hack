-- Author: Viacheslav Lotsmanov
-- License: GPLv3 https://raw.githubusercontent.com/unclechu/xlib-keys-hack/master/LICENSE

{-# LANGUAGE PackageImports #-}

module Utils.X
  ( nextEvent'
  ) where

import "base" System.Posix.Types (Fd(Fd))

import "base" Control.Concurrent (threadWaitRead)

import "base" Data.Bool (bool)

import "X11" Graphics.X11.Xlib (pending)
import "X11" Graphics.X11.Xlib.Types (Display)
import qualified "X11" Graphics.X11.Xlib.Event as XEvent (XEventPtr, nextEvent)
import "X11" Graphics.X11.Xlib.Display (connectionNumber)

-- local imports

import Utils.Sugar ((|?|), (<&>))


-- https://wiki.haskell.org/X_window_programming_in_Haskell
-- A version of nextEvent that does not block in foreign calls.
nextEvent' :: Display -> XEvent.XEventPtr -> IO ()
nextEvent' dpy evPtr =
  pending dpy <&> (/= 0) >>= XEvent.nextEvent dpy evPtr |?| tryAgain
  where fd = connectionNumber dpy
        tryAgain = threadWaitRead (Fd fd) >> nextEvent' dpy evPtr
