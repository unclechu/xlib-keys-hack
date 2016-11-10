-- Author: Viacheslav Lotsmanov
-- License: GPLv3 https://raw.githubusercontent.com/unclechu/xlib-keys-hack/master/LICENSE

{-# LANGUAGE DoAndIfThenElse #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Process
  ( processEvents
  ) where

import System.Exit (exitFailure)

import Control.Monad (when, unless)

import qualified Data.Maybe as Maybe
import Data.Bits ((.|.))

import qualified Graphics.X11.Types       as XTypes
import qualified Graphics.X11.ExtraTypes  as XTypes
import qualified Graphics.X11.Xlib.Event  as XEvent
import qualified Graphics.X11.Xlib.Extras as XExtras
import Graphics.X11.Xlib.Misc ( getInputFocus
                              , grabKey
                              , ungrabKey
                              )
import Graphics.X11.Xlib.Types (Display)
import Graphics.X11.Types (Window)

import Control.Lens ((.~), (^.))

import Utils ((&), (.>), nextEvent', errPutStrLn)
import Bindings.Xkb (xkbSetGroup)
import qualified State


-- caps lock key that remapped to escape key
capsEscKeycode = 66


-- reactive infinite monad
processEvents :: State.State -> Display -> Window -> IO ()
processEvents state dpy rootWnd = do

  (wnd, _) <- getInputFocus dpy
  if wnd == rootWnd
  then again state
  else do

    XEvent.sync dpy False

    XEvent.selectInput dpy wnd  (  XTypes.keyPressMask
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

    newState <- Maybe.fromMaybe (return state) m
    again newState

  where again newState = processEvents newState dpy rootWnd
        nextEvent = nextEvent'

        dealWithKey :: String -> XEvent.XEventPtr -> IO State.State
        dealWithKey = processKeyEvent dpy state

        dealWithFocus :: String -> XEvent.XEventPtr -> IO State.State
        dealWithFocus = processFocusEvent dpy state

        dealMap :: String -> XEvent.XEventPtr -> Maybe (IO State.State)
        dealMap evName evPtr =
          case evName of
               "FocusIn"    -> Maybe.Just $ dealWithFocus evName evPtr
               "FocusOut"   -> Maybe.Just $ dealWithFocus evName evPtr
               "KeyPress"   -> Maybe.Just $ dealWithKey   evName evPtr
               "KeyRelease" -> Maybe.Just $ dealWithKey   evName evPtr
               _            -> Maybe.Nothing


processKeyEvent :: Display
                -> State.State
                -> String
                -> XEvent.XEventPtr
                -> IO State.State
processKeyEvent dpy state evName evPtr = do

  exEv <- XExtras.getEvent evPtr
  let keycode :: XTypes.KeyCode
      keycode = XExtras.ev_keycode exEv

  putStrLn $ "key event: " ++ evName
  putStrLn $ "key code: " ++ show keycode
  putStrLn "--------------------"

  return state


processFocusEvent :: Display
                  -> State.State
                  -> String
                  -> XEvent.XEventPtr
                  -> IO State.State
processFocusEvent dpy prevState evName evPtr = do

  let lastWnd = State.lastWindow prevState
  curWnd <- XEvent.get_Window evPtr

  putStrLn $ "focus event: "  ++ evName
  putStrLn $ "wnd previous: " ++ show lastWnd
  putStrLn $ "wnd current: "  ++ show curWnd
  putStrLn "--------------------"

  when (evName == "FocusOut") $
    xkbSetGroup dpy 0
      >>= flip unless (errPutStrLn "xkbSetGroup error" >> exitFailure)

  let newState = if curWnd /= lastWnd
                    then prevState & State.lastWindow' .~ curWnd
                    else prevState

  return newState
