-- Author: Viacheslav Lotsmanov
-- License: GPLv3 https://raw.githubusercontent.com/unclechu/xlib-keys-hack/master/LICENSE

{-# LANGUAGE ScopedTypeVariables #-}

module Main (main) where

-- import System.Console.GetOpt (getOpt) -- TODO (verbose arg)
import System.Exit (exitFailure)

import Control.Monad (when, unless)

import qualified Data.Either as Either
import qualified Data.Maybe as Maybe

import qualified Graphics.X11.Types      as XTypes
import qualified Graphics.X11.ExtraTypes as XTypes
import Graphics.X11.Xlib.Types (Display)
import Graphics.X11.Types (Window)

import qualified Graphics.X11.Xlib.Extras as XExtras
import Graphics.X11.Xlib.Display (defaultRootWindow)
import Graphics.X11.Xlib.Misc (keysymToKeycode)

import qualified System.IO as SysIO
import qualified GHC.IO.Handle as IOHandle
import qualified GHC.IO.Handle.FD as IOHandleFD
import qualified System.Linux.Input.Event as EvdevEvent

import Utils (errPutStrLn, (&), (.>))
import Bindings.Xkb ( xkbGetDescPtr
                    , xkbFetchControls
                    , xkbGetGroupsCount
                    , xkbGetDisplay
                    )
import Process (initReset, processEvents)
import qualified State
import qualified Keys


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


mainX :: IO ()
mainX = do
  putStrLn "~~~ begin ~~~"
  -- LI.doStuff

  let evfilepath = "/dev/input/by-id/usb-1d57_2.4G_Receiver-event-kbd"
  handle <- IOHandleFD.openFile evfilepath SysIO.ReadMode
  mainloop handle
  SysIO.hClose handle

  putStrLn "~~~ end ~~~"

  where mainloop :: IOHandle.Handle -> IO ()
        mainloop handle = do
          evMaybe <- EvdevEvent.hReadEvent handle
          case evMaybe of
            Maybe.Just EvdevEvent.KeyEvent
                         { EvdevEvent.evKeyCode = keyCode
                         , EvdevEvent.evKeyEventType = pressStatus
                         }
              -> putStrLn $ show pressStatus ++ ": " ++ show keyCode
            _ -> return ()

          mainloop handle


main :: IO ()
main = do

  putStrLn "~~~ begin ~~~"

  dpy <- xkbInit
  let rootWnd = defaultRootWindow dpy

  -- prevent errors with closed windows
  XExtras.xSetErrorHandler

  let state = State.initState { State.lastWindow = rootWnd
                              }

  initReset Keys.getRealKeyCodes dpy rootWnd

  let keyCodes = Keys.getKeyCodes
        Keys.VirtualKeyCodes {
                             }

  -- event loop
  processEvents keyCodes state dpy rootWnd
