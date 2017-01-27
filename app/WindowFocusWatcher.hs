-- Author: Viacheslav Lotsmanov
-- License: GPLv3 https://raw.githubusercontent.com/unclechu/xlib-keys-hack/master/LICENSE

-- BEWARE! This app could fall down with segmentation fault error.
-- It happens sometimes when you subscribe to different events of
-- different windows I don't know why. That is the main reason why
-- I separated this code from main process, so I just can respawn
-- it and prevent main application from falling down.

{-# LANGUAGE PackageImports #-}

module Main (main) where

import "base" Control.Monad (when, forever)
import "base" Control.Concurrent (threadDelay)

import "X11" Graphics.X11.Xlib (Display, openDisplay, closeDisplay)
import "X11" Graphics.X11.Xlib.Event (XEventPtr, allocaXEvent, selectInput, flush)
import "X11" Graphics.X11.Xlib.Extras (getEvent, xSetErrorHandler)
import "X11" Graphics.X11.Xlib.Misc (getInputFocus)
import "X11" Graphics.X11.Types (noEventMask, focusChangeMask)

import "xlib-keys-hack" Utils.X (nextEvent')


main :: IO ()
main = do
  dpy <- openDisplay ""
  xSetErrorHandler -- Prevent errors with closed windows
  allocaXEvent $ \evPtr -> forever $ waitForEvent dpy evPtr

waitForEvent :: Display -> XEventPtr -> IO ()
waitForEvent dpy evPtr = do
  (wnd, _) <- getInputFocus dpy
  selectInput dpy wnd focusChangeMask
  nextEvent' dpy evPtr
  selectInput dpy wnd noEventMask -- Do not react on new window focus events
                                  -- until next iteration.
  flush dpy -- Ignore remaining events in queue
  putStrLn "" -- Just to notify main process about new window focus event
  threadDelay $ 200 * 1000 -- Prevent it from notifying too often
