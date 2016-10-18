-- Author: Viacheslav Lotsmanov
-- License: GPLv3 https://raw.githubusercontent.com/unclechu/xlib-keys-hack/master/LICENSE

module Main (main) where

import System.IO (hPutStrLn, stderr)
import System.Environment (getArgs)
import System.Console.GetOpt (getOpt)

import qualified Graphics.X11.Xlib as Xlib
import qualified Graphics.X11.Xlib.Event as XEvent
import qualified Graphics.X11.Xlib.Display as XDisplay


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

  dpy <- XDisplay.openDisplay ""
  let wnd = XDisplay.defaultRootWindow dpy

  -- key_event <- XEvent.get_KeyEvent TODO

  putStrLn "it's okay"
