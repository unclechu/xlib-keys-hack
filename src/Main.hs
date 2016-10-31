-- Author: Viacheslav Lotsmanov
-- License: GPLv3 https://raw.githubusercontent.com/unclechu/xlib-keys-hack/master/LICENSE

module Main (main) where

import System.IO (hPutStrLn, stderr)
import System.Environment (getArgs)
import System.Console.GetOpt (getOpt)
import System.Posix.Types (Fd(Fd))

import Control.Concurrent (threadWaitRead)

import Data.Bits ((.|.))

import Graphics.X11.Types ( xK_Escape
                          , xK_Caps_Lock
                          )
import Graphics.X11.ExtraTypes ( xK_ISO_Level3_Shift
                               )
import Graphics.X11.Xlib ( Display
                         , Window
                         , keyPressMask
                         , keyReleaseMask
                         , focusChangeMask
                         , pending
                         )
import Graphics.X11.Xlib.Display ( openDisplay
                                 , defaultRootWindow
                                 , connectionNumber
                                 )
import Graphics.X11.Xlib.Misc ( keysymToKeycode
                              , getInputFocus
                              )
import Graphics.X11.Xlib.Event ( XEvent(XEvent)
                               , XEventPtr
                               , nextEvent
                               , selectInput
                               , allocaXEvent
                               , sync
                               )
import Graphics.X11.Xlib.Extras ( getEvent
                                , eventName
                                )

import Bindings.XTest ( fakeKeyEvent
                      )


-- constants

appName        = "xlib-keys-hack"
xmobarPipeFile = ".xmonad/xmobar.fifo"

capsKey        = 66
enterKey       = 36
lCtrlKey       = 37
rCtrlKey       = 105

lAltKey        = 64
rAltKey        = 108

lShiftKey      = 50
rShiftKey      = 62


(&) = flip ($)
(?) = flip (.)
errPutStrLn = hPutStrLn stderr

-- https://wiki.haskell.org/X_window_programming_in_Haskell
-- A version of nextEvent that does not block in foreign calls.
nextEvent' :: Display -> XEventPtr -> IO ()
nextEvent' dpy evPtr = do
  pend <- pending dpy
  if pend /= 0
     then nextEvent dpy evPtr
     else do
       threadWaitRead (Fd fd)
       nextEvent' dpy evPtr
  where fd = connectionNumber dpy

processEvent :: Display -> Window -> XEventPtr -> IO ()
processEvent dpy rootWnd evPtr = do

  putStrLn "----- Iteration -----"

  (wnd, _) <- getInputFocus dpy
  if wnd == rootWnd
  then putStrLn "Root window!" >> again
  else do

    putStrLn "selectInput..."
    selectInput dpy wnd (  keyPressMask
                       .|. keyReleaseMask
                       .|. focusChangeMask
                        )

    putStrLn "sync ..."
    sync dpy False
    putStrLn "nextEvent ..."
    nextEvent dpy evPtr
    putStrLn "getEvent ..."
    ev <- getEvent evPtr
    putStrLn $ "Event: " ++ eventName ev
    again

  where again = processEvent dpy rootWnd evPtr
        nextEvent = nextEvent'


main :: IO ()
main = do

  args <- getArgs

  dpy <- openDisplay ""
  let rootWnd = defaultRootWindow dpy

  escapeKeycode      <- keysymToKeycode dpy xK_Escape
  capsLockKeycode    <- keysymToKeycode dpy xK_Caps_Lock
  level3ShiftKeycode <- keysymToKeycode dpy xK_ISO_Level3_Shift

  putStrLn $ "Escape keycode: "       ++ show escapeKeycode
  putStrLn $ "Caps Lock keycode: "    ++ show capsLockKeycode
  putStrLn $ "Level3 Shift keycode: " ++ show level3ShiftKeycode

  evPtr <- allocaXEvent return
  let eventLoop = processEvent dpy rootWnd evPtr
  eventLoop

  -- fakeKeyEvent dpy xK_ISO_Level3_Shift True
  -- fakeKeyEvent dpy xK_ISO_Level3_Shift False

  putStrLn "it's okay"
