-- Author: Viacheslav Lotsmanov
-- License: GPLv3 https://raw.githubusercontent.com/unclechu/xlib-keys-hack/master/LICENSE

{-# LANGUAGE DoAndIfThenElse #-}

module Main (main) where

import System.IO (hPutStrLn, stderr)
-- import System.Console.GetOpt (getOpt) -- TODO (verbose arg)
import System.Posix.Types (Fd(Fd))
import System.Exit (exitFailure)

import Control.Monad (when, unless)
import Control.Concurrent (threadWaitRead)

import qualified Data.Maybe as Maybe
import Data.Bits ((.|.))

import qualified Graphics.X11.Types      as XTypes
import qualified Graphics.X11.ExtraTypes as XTypes
import Graphics.X11.Xlib.Types (Display)
import Graphics.X11.Types (Window)

import qualified Graphics.X11.Xlib.Event  as XEvent
import qualified Graphics.X11.Xlib.Extras as XExtras -- ev_keycode
import Graphics.X11.Xlib (pending)
import Graphics.X11.Xlib.Display ( openDisplay
                                 , defaultRootWindow
                                 , connectionNumber
                                 )
import Graphics.X11.Xlib.Misc ( keysymToKeycode
                              , getInputFocus
                              , grabKey
                              , ungrabKey
                              )

import Bindings.XTest (fakeKeyEvent)
import Bindings.Xkb ( xkbGetDescPtr
                    , xkbFetchControls
                    , xkbIsDescPtrNotNull
                    , xkbGetGroupsCount
                    )


xmobarPipeFile = ".xmonad/xmobar.fifo"

-- caps lock key that remapped to escape key
capsEscKeycode = 66

errPutStrLn = hPutStrLn stderr


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


processEvent :: Display -> Window -> IO ()
processEvent dpy rootWnd = do

  (wnd, _) <- getInputFocus dpy
  if wnd == rootWnd
  then again
  else do

    XEvent.sync dpy False

    XEvent.selectInput dpy wnd (  XTypes.keyPressMask
                              .|. XTypes.keyReleaseMask
                              .|. XTypes.focusChangeMask
                               )

    grabKey dpy
            capsEscKeycode
            XTypes.anyModifier
            wnd
            False
            XTypes.grabModeAsync
            XTypes.grabModeAsync
    evPtr <- XEvent.allocaXEvent return
    nextEvent dpy evPtr
    ungrabKey dpy capsEscKeycode XTypes.anyModifier wnd

    ev <- XExtras.getEvent evPtr
    let m = dealMap (XExtras.eventName ev) evPtr
    when (Maybe.isJust m) $ Maybe.fromJust m

    again

  where again = processEvent dpy rootWnd
        nextEvent = nextEvent'

        dealMap :: String -> XEvent.XEventPtr -> Maybe (IO ())
        dealMap evName evPtr =
          case evName of
               "FocusOut"   -> Maybe.Just $ dealWithFocus evName evPtr
               "KeyPress"   -> Maybe.Just $ dealWithKey   evName evPtr
               "KeyRelease" -> Maybe.Just $ dealWithKey   evName evPtr
               _            -> Maybe.Nothing

        dealWithKey :: String -> XEvent.XEventPtr -> IO ()
        dealWithKey evName evPtr = do
          -- XEvent.get_KeyEvent
          putStrLn $ "key event: " ++ evName
          return ()

        dealWithFocus :: String -> XEvent.XEventPtr -> IO ()
        dealWithFocus evName evPtr = do
          putStrLn $ "focus event: " ++ evName
          return ()


main :: IO ()
main = do

  dpy <- openDisplay ""
  let rootWnd = defaultRootWindow dpy

  -- prevent errors with closed windows
  XExtras.xSetErrorHandler

  escapeKeycode      <- keysymToKeycode dpy XTypes.xK_Escape
  capsLockKeycode    <- keysymToKeycode dpy XTypes.xK_Caps_Lock
  level3ShiftKeycode <- keysymToKeycode dpy XTypes.xK_ISO_Level3_Shift

  putStrLn $ "Escape keycode: "       ++ show escapeKeycode
  putStrLn $ "Caps Lock keycode: "    ++ show capsLockKeycode
  putStrLn $ "Level3 Shift keycode: " ++ show level3ShiftKeycode


  xkbDescPtr <- xkbGetDescPtr dpy
  let isOkay = xkbIsDescPtrNotNull xkbDescPtr
  unless isOkay $ errPutStrLn "Xkb init error: xkbDescPtr is null"
               >> exitFailure

  isOkay <- xkbFetchControls dpy xkbDescPtr
  unless isOkay $ errPutStrLn "Xkb init error: status is not 0"
               >> exitFailure

  count <- xkbGetGroupsCount xkbDescPtr
  unless (count > 0) $ errPutStrLn "Xkb init error: groups count is 0"
                    >> exitFailure


  let eventLoop = processEvent dpy rootWnd
  eventLoop

  -- fakeKeyEvent dpy xK_ISO_Level3_Shift True
  -- fakeKeyEvent dpy xK_ISO_Level3_Shift False

  putStrLn "it's okay"
