-- Author: Viacheslav Lotsmanov
-- License: GPLv3 https://raw.githubusercontent.com/unclechu/xlib-keys-hack/master/LICENSE

module Utils
  ( (&)
  , nextEvent'
  , errPutStrLn
  ) where

import Graphics.X11.Xlib (pending)
import Graphics.X11.Xlib.Types (Display)
import qualified Graphics.X11.Xlib.Event as XEvent
import Graphics.X11.Xlib.Display (connectionNumber)

import Control.Concurrent (threadWaitRead)

import System.Posix.Types (Fd(Fd))
import System.IO (hPutStrLn, stderr)


-- pipe operator
(&) :: a -> (a -> b) -> b
(&) = flip ($)
infixl 0 &


-- https://wiki.haskell.org/X_window_programming_in_Haskell
-- A version of nextEvent that does not block in foreign calls.
nextEvent' :: Display -> XEvent.XEventPtr -> IO ()
nextEvent' dpy evPtr = do
  pend <- pending dpy
  if pend /= 0
     then XEvent.nextEvent dpy evPtr
     else do
       threadWaitRead (Fd fd)
       nextEvent' dpy evPtr
  where fd = connectionNumber dpy


errPutStrLn = hPutStrLn stderr
