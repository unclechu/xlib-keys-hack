-- Author: Viacheslav Lotsmanov
-- License: GPLv3 https://raw.githubusercontent.com/unclechu/xlib-keys-hack/master/LICENSE

{-# LANGUAGE ScopedTypeVariables #-}

module Main (main) where

import System.Exit (exitFailure, exitSuccess)
import System.Environment (getArgs)

import Control.Monad (when, unless)

import Data.Either (Either(Left, Right), either)
import Data.Maybe (Maybe(Just))

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

import Control.Lens ((.~), (%~), (^.))

import Utils (errPutStrLn, dieWith, (&), (.>), makeApoLenses)
import Bindings.Xkb ( xkbGetDescPtr
                    , xkbFetchControls
                    , xkbGetGroupsCount
                    , xkbGetDisplay
                    )
import Process (initReset, processEvents)
import qualified Options
import qualified State
import qualified Keys


xmobarPipeFile = ".xmonad/xmobar.fifo"


xkbInit :: IO Display
xkbInit = do

  (dpy :: Display) <- xkbGetDisplay >>= flip either return
    (\err -> dieWith $ "Xkb open display error: " ++ show err)

  xkbDescPtr <- xkbGetDescPtr dpy >>= flip either return
    (\err -> dieWith $ "Xkb error: get keyboard data error" ++ show err)

  xkbFetchControls dpy xkbDescPtr
    >>= flip unless (dieWith "Xkb error: fetch controls error")

  xkbGetGroupsCount xkbDescPtr
    >>= return . (> 0)
    >>= flip unless (dieWith "Xkb error: groups count is 0")

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
            Just EvdevEvent.KeyEvent
                   { EvdevEvent.evKeyCode = keyCode
                   , EvdevEvent.evKeyEventType = pressStatus
                   }
              -> putStrLn $ show pressStatus ++ ": " ++ show keyCode
            _ -> return ()

          mainloop handle


mainY :: IO ()
mainY = do

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


main :: IO ()
main = do

  putStrLn "~~~ begin ~~~"

  opts <- getArgs >>= \argv ->
    let header = "Usage: xlib-keys-hack [OPTION...] devices fd paths..."
    in case Options.extractOptions argv of

      Right opts -> do

        when (opts ^. Options.showHelp') $ do
          putStrLn Options.usageInfo
          exitSuccess

        opts ^. Options.handleDevicePath' & length & (> 0) &
          \x -> unless x $ do
            errPutStrLn Options.usageInfo
            dieWith "At least one device fd path must be specified!"

        return opts

      Left err -> do
        errPutStrLn Options.usageInfo
        dieWith err



  print opts
  putStrLn "~~~ end ~~~"
