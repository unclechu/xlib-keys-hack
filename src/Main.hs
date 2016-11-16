-- Author: Viacheslav Lotsmanov
-- License: GPLv3 https://raw.githubusercontent.com/unclechu/xlib-keys-hack/master/LICENSE

{-# LANGUAGE ScopedTypeVariables #-}

module Main (main) where

-- import System.Console.GetOpt (getOpt) -- TODO (verbose arg)
import System.Exit (exitFailure)

import Control.Monad (when, unless)

import qualified Data.Either as Either

import qualified Graphics.X11.Types      as XTypes
import qualified Graphics.X11.ExtraTypes as XTypes
import Graphics.X11.Xlib.Types (Display)
import Graphics.X11.Types (Window)

import qualified Graphics.X11.Xlib.Extras as XExtras
import Graphics.X11.Xlib.Display (defaultRootWindow)
import Graphics.X11.Xlib.Misc (keysymToKeycode)

import Utils (errPutStrLn, (&), (.>))
import Bindings.Xkb ( xkbGetDescPtr
                    , xkbFetchControls
                    , xkbGetGroupsCount
                    , xkbGetDisplay
                    )
import Process (initReset, processEvents)
import qualified State
import qualified Keys


import qualified Bindings.LibInput as LI


xmobarPipeFile = ".xmonad/xmobar.fifo"


xkbInit :: IO Display
xkbInit = do

  (dpy :: Display) <- xkbGetDisplay >>= flip Either.either return
    ( \err -> do
      errPutStrLn $ "Xkb open display error: " ++ show err
      exitFailure
    )

  xkbDescPtr <- xkbGetDescPtr dpy >>= flip Either.either return
    ( \err -> do
      errPutStrLn $ "Xkb error: get keyboard data error" ++ show err
      exitFailure
    )

  xkbFetchControls dpy xkbDescPtr
    >>= flip unless
          (errPutStrLn "Xkb error: fetch controls error" >> exitFailure)

  xkbGetGroupsCount xkbDescPtr
    >>= return . (> 0)
    >>= flip unless (errPutStrLn "Xkb error: groups count is 0" >> exitFailure)

  return dpy


main :: IO ()
main = do
  putStrLn "~~~ begin ~~~"
  LI.doStuff
  putStrLn "~~~ end ~~~"


mainTmp :: IO ()
mainTmp = do

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

  let state = State.initState { State.lastWindow = rootWnd
                              }

  initReset Keys.getRealKeyCodes dpy rootWnd

  let keyCodes = Keys.getKeyCodes
        Keys.VirtualKeyCodes { Keys.capsLockKeyCode = capsLockKeycode
                             , Keys.level3KeyCode   = level3ShiftKeycode
                             }

  -- event loop
  processEvents keyCodes state dpy rootWnd
