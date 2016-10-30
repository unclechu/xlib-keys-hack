-- Author: Viacheslav Lotsmanov
-- License: GPLv3 https://raw.githubusercontent.com/unclechu/xlib-keys-hack/master/LICENSE

module Main (main) where

import System.IO (hPutStrLn, stderr)
import System.Environment (getArgs)
import System.Console.GetOpt (getOpt)

import Graphics.X11.Types (xK_Escape, xK_Caps_Lock)
import Graphics.X11.ExtraTypes (xK_ISO_Level3_Shift)
import Graphics.X11.Xlib.Display (openDisplay, defaultRootWindow)
import Graphics.X11.Xlib.Misc (keysymToKeycode)

import Bindings.XTest (fakeKeyEvent)


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


errPutStrLn = hPutStrLn stderr

main :: IO ()
main = do

  args <- getArgs

  dpy <- openDisplay ""
  let wnd = defaultRootWindow dpy

  escapeKeycode      <- keysymToKeycode dpy xK_Escape
  capsLockKeycode    <- keysymToKeycode dpy xK_Caps_Lock
  level3ShiftKeycode <- keysymToKeycode dpy xK_ISO_Level3_Shift

  putStrLn $ "Escape keycode: "       ++ show escapeKeycode
  putStrLn $ "Caps Lock keycode: "    ++ show capsLockKeycode
  putStrLn $ "Level3 Shift keycode: " ++ show level3ShiftKeycode

  -- fakeKeyEvent dpy xK_ISO_Level3_Shift True
  -- fakeKeyEvent dpy xK_ISO_Level3_Shift False

  putStrLn "it's okay"
