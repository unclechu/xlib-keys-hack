-- Author: Viacheslav Lotsmanov
-- License: GPLv3 https://raw.githubusercontent.com/unclechu/xlib-keys-hack/master/LICENSE

{-# LANGUAGE DoAndIfThenElse #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Process
  ( processEvents
  , initReset
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
                              , grabKeyboard
                              , ungrabKeyboard
                              )
import Graphics.X11.Xlib.Types (Display)
import Graphics.X11.Types (Window)

import Control.Lens ((.~), (^.))

import Utils ( (&), (.>), (<||>)
             , nextEvent'
             , errPutStrLn
             )
import Bindings.Xkb (xkbSetGroup)
import Bindings.XTest (fakeKeyEvent, fakeKeyCodeEvent)
import qualified State
import qualified Keys


-- reactive infinite monad
processEvents :: Keys.KeyCodes -> State.State -> Display -> Window -> IO ()
processEvents keyCodes state dpy rootWnd =
  fmap fst (getInputFocus dpy)
    >>= \wnd -> if wnd == rootWnd
                   then again state
                   else do

  XEvent.sync dpy False

  XEvent.selectInput dpy wnd  (  XTypes.keyPressMask
                             .|. XTypes.keyReleaseMask
                             .|. XTypes.focusChangeMask
                              )

  unless (state ^. State.debugFlag') $ do
    putStrLn "grab again"
    _ <- grabKeyboard dpy wnd
            -- XExtras.anyKey
            -- XTypes.anyModifier
            -- wnd
            False
            XTypes.grabModeAsync
            XTypes.grabModeAsync
            XExtras.currentTime
    return ()

  putStrLn "A"
  evPtr <- XEvent.allocaXEvent return
  putStrLn "A+"
  nextEvent dpy evPtr
  putStrLn "B"

  _ <- ungrabKeyboard dpy
            -- XExtras.anyKey
            -- XTypes.anyModifier
            -- wnd
            XExtras.currentTime
  putStrLn "C"

  ev <- XExtras.getEvent evPtr
  putStrLn "D"
  let m = dealMap (XExtras.eventName ev) evPtr
  putStrLn "E"
  newState <- Maybe.fromMaybe (return state) m
  putStrLn "F"
  again newState

  where again newState = processEvents keyCodes newState dpy rootWnd
        nextEvent = nextEvent'

        dealWithKey :: String -> XEvent.XEventPtr -> IO State.State
        dealWithKey = processKeyEvent dpy state keyCodes

        dealWithFocus :: String -> XEvent.XEventPtr -> IO State.State
        dealWithFocus = processFocusEvent dpy state keyCodes

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
                -> Keys.KeyCodes
                -> String
                -> XEvent.XEventPtr
                -> IO State.State
processKeyEvent dpy prevState keyCodes evName evPtr = do

  exEv <- XExtras.getEvent evPtr
  let keyCode = XExtras.ev_keycode exEv


  -- FIXME infinite loop
  -- XEvent.putBackEvent dpy evPtr

  -- doesn't help at all
  -- case evName of
  --      "KeyPress"   -> fakeKeyCodeEvent dpy keyCode True
  --      "KeyRelease" -> fakeKeyCodeEvent dpy keyCode False


  putStrLn "~~~~~~~~~~~~~~~~~~~~"
  putStrLn $ "key event: " ++ evName
  putStrLn $ "key code: " ++ show keyCode
  putStrLn "--------------------"

  return (prevState & State.debugFlag' .~ True)
  -- return prevState


processFocusEvent :: Display
                  -> State.State
                  -> Keys.KeyCodes
                  -> String
                  -> XEvent.XEventPtr
                  -> IO State.State
processFocusEvent dpy prevState keyCodes evName evPtr = do

  let lastWnd = State.lastWindow prevState
  curWnd <- XEvent.get_Window evPtr

  putStrLn "~~~~~~~~~~~~~~~~~~~~"
  putStrLn $ "focus event: "  ++ evName
  putStrLn $ "wnd previous: " ++ show lastWnd
  putStrLn $ "wnd current: "  ++ show curWnd
  putStrLn "--------------------"

  when (evName == "FocusOut") $ resetKbdLayout dpy

  let newState = if curWnd /= lastWnd
                    then prevState & State.lastWindow' .~ curWnd
                    else prevState

  return newState


resetKbdLayout :: Display -> IO ()
resetKbdLayout dpy =
  xkbSetGroup dpy 0
    >>= flip unless (errPutStrLn "xkbSetGroup error" >> exitFailure)


initReset :: Keys.RealKeyCodes -> Display -> Window -> IO ()
initReset realKeyCodes dpy rootWnd = do
  resetKbdLayout dpy
  fakeKeyEvent dpy XTypes.xK_ISO_Level3_Shift False
