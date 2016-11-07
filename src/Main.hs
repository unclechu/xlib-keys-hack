-- Author: Viacheslav Lotsmanov
-- License: GPLv3 https://raw.githubusercontent.com/unclechu/xlib-keys-hack/master/LICENSE

module Main (main) where

-- import System.Console.GetOpt (getOpt) -- TODO (verbose arg)
import System.Exit (exitFailure)

import Control.Monad (when, unless)

import qualified Data.Either as Either

import qualified Graphics.X11.Types      as XTypes
import qualified Graphics.X11.ExtraTypes as XTypes
import Graphics.X11.Xlib.Types (Display)
import Graphics.X11.Types (Window)

import qualified Graphics.X11.Xlib.Extras as XExtras -- ev_keycode
import Graphics.X11.Xlib.Display (defaultRootWindow)
import Graphics.X11.Xlib.Misc (keysymToKeycode)

import Utils (errPutStrLn)
import Bindings.XTest (fakeKeyEvent)
import Bindings.Xkb ( xkbGetDescPtr
                    , xkbFetchControls
                    , xkbGetGroupsCount
                    , xkbGetDisplay
                    )
import Process (processEvents)


xmobarPipeFile = ".xmonad/xmobar.fifo"


xkbInit :: IO Display
xkbInit = do

  ret <- xkbGetDisplay
  let (Either.Left  err) = ret
      (Either.Right dpy) = ret
  when (Either.isLeft ret) $
    errPutStrLn ("Xkb open display error: " ++ show err)
    >> exitFailure

  ret <- xkbGetDescPtr dpy
  let (Either.Left  err)        = ret
      (Either.Right xkbDescPtr) = ret
  when (Either.isLeft ret) $
    errPutStrLn ("Xkb error: get keyboard data error" ++ show err)

  isOkay <- xkbFetchControls dpy xkbDescPtr
  unless isOkay $ errPutStrLn "Xkb error: fetch controls error"
               >> exitFailure

  count <- xkbGetGroupsCount xkbDescPtr
  unless (count > 0) $ errPutStrLn "Xkb error: groups count is 0"
                    >> exitFailure

  return dpy


main :: IO ()
main = do

  dpy <- xkbInit
  let rootWnd = defaultRootWindow dpy

  -- prevent errors with closed windows
  XExtras.xSetErrorHandler

  escapeKeycode      <- keysymToKeycode dpy XTypes.xK_Escape
  capsLockKeycode    <- keysymToKeycode dpy XTypes.xK_Caps_Lock
  level3ShiftKeycode <- keysymToKeycode dpy XTypes.xK_ISO_Level3_Shift

  putStrLn $ "Escape keycode: "       ++ show escapeKeycode
  putStrLn $ "Caps Lock keycode: "    ++ show capsLockKeycode
  putStrLn $ "Level3 Shift keycode: " ++ show level3ShiftKeycode

  -- event loop
  processEvents dpy rootWnd

  -- fakeKeyEvent dpy xK_ISO_Level3_Shift True
  -- fakeKeyEvent dpy xK_ISO_Level3_Shift False

  putStrLn "it's okay"
