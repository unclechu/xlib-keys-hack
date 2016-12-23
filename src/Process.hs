-- Author: Viacheslav Lotsmanov
-- License: GPLv3 https://raw.githubusercontent.com/unclechu/xlib-keys-hack/master/LICENSE

{-# LANGUAGE DoAndIfThenElse #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE PackageImports #-}

module Process
  ( initReset
  , processXEvents
  , watchLeds
  , handleKeyboard
  ) where


import "base" Control.Monad (when, unless)
import "mtl" Control.Monad.State.Class (MonadState)
import "transformers" Control.Monad.Trans.Class (lift)
import "lens" Control.Lens ((.~), (%~), (^.), set, over, view)
import "base" Control.Concurrent (threadDelay)
import "base" Control.Concurrent.MVar (MVar, modifyMVar_)
import "base" Control.Concurrent.Chan (Chan)

import "base" Data.Maybe (Maybe(Just, Nothing), fromJust, isJust)
import "text-format-simple" Text.Format (format)

import qualified "X11" Graphics.X11.Types       as XTypes
import qualified "X11" Graphics.X11.ExtraTypes  as XTypes
import qualified "X11" Graphics.X11.Xlib.Event  as XEvent
import qualified "X11" Graphics.X11.Xlib.Extras as XExtras
import "X11" Graphics.X11.Xlib.Misc (getInputFocus)
import "X11" Graphics.X11.Xlib.Types (Display)
import "X11" Graphics.X11.Types (Window)

-- local imports

import Utils ( (&), (.>), (<||>), (?)
             , nextEvent'
             , dieWith
             , writeToFd

             , BreakableT
             , runFromBreakableT
             , breakT, breakTOn, breakTOnWith, continueT
             )
import Bindings.Xkb (xkbSetGroup)
import Bindings.MoreXlib (getLeds, lockDisplay, unlockDisplay)
import qualified Options as O
import qualified Actions
import qualified State
import qualified Keys

import Process.Keyboard (handleKeyboard)
import qualified Process.CrossThread as CrossThread
  (turnCapsLockMode, justTurnCapsLockMode)


type Options         = O.Options
type KeyMap          = Keys.KeyMap
type State           = State.State
type LedModes        = State.LedModes
type CrossThreadVars = State.CrossThreadVars


resetKbdLayout :: Display -> IO ()
resetKbdLayout dpy =
  lockDisplay dpy
    >> xkbSetGroup dpy 0
    >>= flip unless (dieWith "xkbSetGroup error")
    >> unlockDisplay dpy


initReset :: Options -> KeyMap -> Display -> Window -> IO ()
initReset opts keyMap dpy rootWnd = do

  noise "Initial resetting of keyboard layout..."
  resetKbdLayout dpy

  noise "Initial resetting Caps Lock mode..."
  justTurnCapsLockMode False

  let xmobarFd = opts ^. O.xmobarPipeFd'
  when (isJust xmobarFd) $ do
    noise "Initial resetting of xmobar leds..."
    let fd = fromJust xmobarFd
    writeToFd fd "capslock:off\n"
    writeToFd fd "numlock:off\n"
    writeToFd fd "alternative:off\n"

  where noise = O.noise opts
        justTurnCapsLockMode =
          CrossThread.justTurnCapsLockMode dpy noise keyMap


processXEvents :: CrossThreadVars
               -> Options
               -> KeyMap
               -> Display
               -> Window
               -> IO ()
processXEvents ctVars opts keyMap dpy rootWnd = runFromBreakableT $ do

  (wnd, _) <- lift $ getInputFocus dpy
  breakTOn $ wnd == rootWnd

  lift $ XEvent.sync dpy False
  lift $ XEvent.selectInput dpy wnd XTypes.focusChangeMask

  evPtr <- lift $ XEvent.allocaXEvent return
  lift $ noise "Waiting for next X event..."

  lift $ nextEvent dpy evPtr
  ev <- lift $ XExtras.getEvent evPtr
  let evName = XExtras.eventName ev

  if
   | evName `elem` ["FocusIn", "FocusOut"] -> do
     let m f = modifyMVar_ (State.stateMVar ctVars) (runFromBreakableT . f)
     lift $ m $ \state -> do

       lift $ noise $ "Handling focus event: " ++ evName ++ "..."

       let lastWnd = State.lastWindow state
       curWnd <- lift $ XEvent.get_Window evPtr

       breakTOnWith (curWnd == lastWnd || evName /= "FocusOut") state

       lift $ noise "Resetting keyboard layout..."
       lift $ resetKbdLayout dpy

       state <- do
         lift $ noise "Resetting Caps Lock mode..."
         lift $ turnCapsLockMode state False

       lift $ noise $ format "Window focus moved from {0} to {1}"
                             [show lastWnd, show curWnd]
       return $ state { State.lastWindow = curWnd }

   | otherwise -> return ()

  where nextEvent = nextEvent'

        noise  = Actions.noise  opts ctVars ::  String  -> IO ()
        noise' = Actions.noise' opts ctVars :: [String] -> IO ()

        turnCapsLockMode :: State -> Bool -> IO State
        turnCapsLockMode = CrossThread.turnCapsLockMode dpy noise' keyMap


-- FIXME waiting in blocking-mode for new leds event
-- Watch for new leds state and when new leds state is coming
-- store it in State, notify xmobar pipe and log.
watchLeds :: CrossThreadVars -> Options -> KeyMap -> Display -> Window -> IO ()
watchLeds ctVars opts keyMap dpy rootWnd = f $ \leds prevState -> do

  let prevCapsLock = prevState ^. State.leds' . State.capsLockLed'
      newCapsLock  = leds ^. State.capsLockLed'
      prevNumLock  = prevState ^. State.leds' . State.numLockLed'
      newNumLock   = leds ^. State.numLockLed'

  when (view State.leds' prevState /= leds) $ do

    when (prevCapsLock /= newCapsLock) $ do
      notify $ "capslock:" ++ notifyStatus newCapsLock
      noise $ "Caps Lock is " ++ onOff newCapsLock

    when (prevNumLock /= newNumLock) $ do
      notify $ "numlock:" ++ notifyStatus newNumLock
      noise $ "Num Lock is " ++ onOff newNumLock

  return (prevState & State.leds' .~ leds)

  where noise = Actions.noise opts ctVars         :: String -> IO ()
        notify = Actions.notifyXmobar opts ctVars :: String -> IO ()
        notifyStatus = "on\n" <||> "off\n"        :: Bool -> String

        f :: (LedModes -> State -> IO State) -> IO ()
        f m = do
          leds <- getLeds dpy
          modifyMVar_ (State.stateMVar ctVars) (m leds)
          threadDelay $ 100 * 1000


onOff :: Bool -> String
onOff = "On" <||> "Off"
