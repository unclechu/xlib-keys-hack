-- Author: Viacheslav Lotsmanov
-- License: GPLv3 https://raw.githubusercontent.com/unclechu/xlib-keys-hack/master/LICENSE

-- BEWARE! This app could fall down with segmentation fault error.
-- It happens sometimes when you subscribe to different events of
-- different windows I don't know why. That is the main reason why
-- I separated this code from main process, so I just can respawn
-- it and prevent main application from falling down.

{-# LANGUAGE PackageImports #-}

module Main (main) where

import "base" System.IO (stdout, hFlush)
import "base" System.Exit (exitSuccess)
import "unix" System.Posix.Signals ( installHandler
                                   , Handler(Catch)
                                   , sigINT, sigTERM
                                   )

import "base" Control.Monad (forever)
import "base" Control.Concurrent (threadDelay, forkIO, throwTo)
import "base" Control.Concurrent.MVar (newEmptyMVar, putMVar, takeMVar)
import "base" Control.Exception (Exception, catch)

import "base" Data.Typeable (Typeable)

import "X11" Graphics.X11.Xlib (Display, openDisplay, closeDisplay)
import "X11" Graphics.X11.Xlib.Event ( XEventPtr
                                     , allocaXEvent
                                     , selectInput
                                     , flush
                                     )
import "X11" Graphics.X11.Xlib.Extras (xSetErrorHandler)
import "X11" Graphics.X11.Xlib.Misc (getInputFocus)
import "X11" Graphics.X11.Types (noEventMask, focusChangeMask)

import "xlib-keys-hack" Utils.X (nextEvent')


main :: IO ()
main = do

  dpy <- openDisplay ""
  xSetErrorHandler -- Prevent errors with closed windows

  mVar <- newEmptyMVar
  let terminate = closeDisplay dpy >> putMVar mVar ()

  threadId <- forkIO $
    (allocaXEvent $ \evPtr -> forever $ waitForEvent dpy evPtr)
      `catch` \MortifyThreadException -> terminate

  let catch sig = installHandler sig (Catch termHook) Nothing
      timeout   = threadDelay $ 5000 * 1000
      termHook  = do throwTo threadId MortifyThreadException
                     timeout >> terminate
   in mapM_ catch [sigINT, sigTERM]

  takeMVar mVar
  exitSuccess


waitForEvent :: Display -> XEventPtr -> IO ()
waitForEvent dpy evPtr = do

  (wnd, _) <- getInputFocus dpy
  selectInput dpy wnd focusChangeMask
  nextEvent' dpy evPtr
  selectInput dpy wnd noEventMask -- Do not react on new window focus events
                                  -- until next iteration.
  flush dpy -- Ignore remaining events in queue
  putChar '\n' -- Just to notify main process about new window focus event
  hFlush stdout
  threadDelay $ 200 * 1000 -- Prevent it from notifying too often


data MyThreadException = MortifyThreadException deriving (Show, Typeable)
instance Exception MyThreadException
