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
import Graphics.X11.Xlib.Misc (getInputFocus)
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
  XEvent.selectInput dpy wnd XTypes.focusChangeMask

  evPtr <- XEvent.allocaXEvent return
  nextEvent dpy evPtr
  ev <- XExtras.getEvent evPtr

  Maybe.fromMaybe
      (return state)
      (dealMap (XExtras.eventName ev) evPtr)
    >>= again

  where again newState = processEvents keyCodes newState dpy rootWnd
        nextEvent = nextEvent'

        dealWithFocus :: String -> XEvent.XEventPtr -> IO State.State
        dealWithFocus = processFocusEvent dpy state keyCodes

        dealMap :: String -> XEvent.XEventPtr -> Maybe (IO State.State)
        dealMap evName evPtr
          | evName `elem` ["FocusIn", "FocusOut"] =
              Maybe.Just $ dealWithFocus evName evPtr
          | otherwise = Maybe.Nothing


processFocusEvent :: Display
                  -> State.State
                  -> Keys.KeyCodes
                  -> String
                  -> XEvent.XEventPtr
                  -> IO State.State
processFocusEvent dpy prevState keyCodes evName evPtr = do

  let lastWnd = State.lastWindow prevState
  curWnd <- XEvent.get_Window evPtr

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
  -- fakeKeyEvent dpy XTypes.xK_ISO_Level3_Shift False
