-- Author: Viacheslav Lotsmanov
-- License: GPLv3 https://raw.githubusercontent.com/unclechu/xlib-keys-hack/master/LICENSE

{-# LANGUAGE DoAndIfThenElse #-}

module Process
  ( processEvents
  ) where

import System.Exit (exitFailure)

import Control.Monad (when, unless)

import qualified Data.Maybe as Maybe
import Data.Bits ((.|.))

import qualified Graphics.X11.Types      as XTypes
import qualified Graphics.X11.ExtraTypes as XTypes
import qualified Graphics.X11.Xlib.Event  as XEvent
import qualified Graphics.X11.Xlib.Extras as XExtras -- ev_keycode
import Graphics.X11.Xlib.Misc ( getInputFocus
                              , grabKey
                              , ungrabKey
                              )
import Graphics.X11.Xlib.Types (Display)
import Graphics.X11.Types (Window)

import Utils (nextEvent', errPutStrLn)
import Bindings.Xkb (xkbSetGroup)


-- caps lock key that remapped to escape key
capsEscKeycode = 66


processEvents :: Display -> Window -> IO ()
processEvents dpy rootWnd = do

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

  where again = processEvents dpy rootWnd
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
          isOkay <- xkbSetGroup dpy 0
          unless isOkay $ errPutStrLn "xkbSetGroup error" >> exitFailure
          return ()
