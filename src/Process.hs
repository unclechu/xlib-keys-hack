-- Author: Viacheslav Lotsmanov
-- License: GPLv3 https://raw.githubusercontent.com/unclechu/xlib-keys-hack/master/LICENSE

{-# LANGUAGE DoAndIfThenElse #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Process
  ( initReset
  , processXEvents
  , watchLeds
  , handleKeyboard
  ) where

import System.Exit (exitFailure)
import qualified GHC.IO.Handle as IOHandle

import Control.Monad (when, unless)
import qualified Control.Monad.State as St
import Control.Monad.State.Class (MonadState)
import Control.Lens ((.~), (%~), (^.), set, over, view)
import Control.Concurrent (forkIO, ThreadId, threadDelay)
import Control.Concurrent.MVar (MVar, modifyMVar_)
import Control.Concurrent.Chan (Chan, writeChan)

import Data.Maybe (Maybe(Just, Nothing), fromJust, isJust)
import Data.Bits ((.|.))

import qualified Graphics.X11.Types       as XTypes
import qualified Graphics.X11.ExtraTypes  as XTypes
import qualified Graphics.X11.Xlib.Event  as XEvent
import qualified Graphics.X11.Xlib.Extras as XExtras
import Graphics.X11.Xlib.Misc (getInputFocus)
import Graphics.X11.Xlib.Types (Display)
import Graphics.X11.Types (Window)

import Utils ( (&), (.>), (<||>)
             , nextEvent'
             , errPutStrLn
             , dieWith
             , updateState'
             , writeToFd
             )
import Bindings.Xkb (xkbSetGroup)
import Bindings.XTest (fakeKeyEvent, fakeKeyCodeEvent)
import Bindings.MoreXlib (getLeds, lockDisplay, unlockDisplay)
import qualified Options as O
import qualified Actions
import qualified State
import qualified Keys


resetKbdLayout :: Display -> IO ()
resetKbdLayout dpy =
  lockDisplay dpy
    >> xkbSetGroup dpy 0
    >>= flip unless (dieWith "xkbSetGroup error")
    >> unlockDisplay dpy


initReset :: O.Options -> Keys.RealKeyCodes -> Display -> Window -> IO ()
initReset opts realKeyCodes dpy rootWnd = do

  noise "Initial resetting of keyboard layout..."
  resetKbdLayout dpy

  let xmobarFd = opts ^. O.xmobarPipeFd'
  when (isJust xmobarFd) $ do
    noise "Initial resetting of xmobar leds..."
    let fd = fromJust xmobarFd
    writeToFd fd "capslock:off\n"
    writeToFd fd "numlock:off\n"
    writeToFd fd "alternative:off\n"

  where noise = O.noise opts


processXEvents :: State.CrossThreadVars
               -> O.Options
               -> Keys.KeyCodes
               -> Display
               -> Window
               -> IO ()
processXEvents ctVars opts keyCodes dpy rootWnd = process $ \wnd -> do

  XEvent.sync dpy False
  XEvent.selectInput dpy wnd XTypes.focusChangeMask

  evPtr <- XEvent.allocaXEvent return
  noise "Waiting for next X event..."

  nextEvent dpy evPtr
  ev <- XExtras.getEvent evPtr

  dealMap (XExtras.eventName ev) evPtr

  where nextEvent = nextEvent'

        noise :: String -> IO ()
        noise = Actions.noise opts ctVars

        process :: (Window -> IO ()) -> IO ()
        process m =
          fmap fst (getInputFocus dpy)
            >>= (\wnd -> when (wnd /= rootWnd) $ m wnd)

        dealMap :: String -> XEvent.XEventPtr -> IO ()
        dealMap evName evPtr
          | evName `elem` ["FocusIn", "FocusOut"] =
              processXFocusEvent evName evPtr
          | otherwise = return ()

        processXFocusEvent :: String -> XEvent.XEventPtr -> IO ()
        processXFocusEvent evName evPtr = f $ \prevState -> do

          noise $ "Handling focus event: " ++ evName ++ "..."

          let lastWnd = State.lastWindow prevState
          curWnd <- XEvent.get_Window evPtr

          when (evName == "FocusOut") $ do
            noise "Resetting keyboard layout..."
            resetKbdLayout dpy

          if curWnd == lastWnd
             then return prevState
             else do
               noise $ "Window focus moved from "
                         ++ show lastWnd
                         ++ " to " ++ show curWnd
               return $ prevState { State.lastWindow = curWnd }

          where f :: (State.State -> IO State.State) -> IO ()
                f = modifyMVar_ $ State.stateMVar ctVars


-- FIXME waiting in blocking-mode for new leds event
-- Watch for new leds state and when new leds state is coming
-- store it in State, notify xmobar pipe and log.
watchLeds :: State.CrossThreadVars
          -> O.Options
          -> Keys.KeyCodes
          -> Display
          -> Window
          -> IO ()
watchLeds ctVars opts keyCodes dpy rootWnd = f $ \leds prevState -> do

  let prevCapsLock = prevState ^. State.leds' . State.capsLockLed'
      newCapsLock  = leds ^. State.capsLockLed'
      prevNumLock  = prevState ^. State.leds' . State.numLockLed'
      newNumLock   = leds ^. State.numLockLed'

  when (view State.leds' prevState /= leds) $ do

    when (prevCapsLock /= newCapsLock) $ do
      notify $ "capslock:" ++ notifyStatus newCapsLock
      noise $ "Caps Lock is " ++ status newCapsLock

    when (prevNumLock /= newNumLock) $ do
      notify $ "numlock:" ++ notifyStatus newNumLock
      noise $ "Num Lock is " ++ status newNumLock

  return (prevState & State.leds' .~ leds)

  where noise :: String -> IO ()
        noise = Actions.noise opts ctVars

        status = "On" <||> "Off"
        notifyStatus = "on\n" <||> "off\n"

        notify :: String -> IO ()
        notify = Actions.notifyXmobar opts ctVars

        f :: (State.LedModes -> State.State -> IO State.State) -> IO ()
        f m = do
          leds <- getLeds dpy
          modifyMVar_ (State.stateMVar ctVars) (m leds)
          threadDelay $ 100 * 1000


-- Wait for key events and simulate them in X.
handleKeyboard :: State.CrossThreadVars
               -> O.Options
               -> Keys.KeyCodes
               -> Display
               -> Window
               -> IO ()
handleKeyboard = undefined
