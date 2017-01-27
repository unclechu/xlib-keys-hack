-- Author: Viacheslav Lotsmanov
-- License: GPLv3 https://raw.githubusercontent.com/unclechu/xlib-keys-hack/master/LICENSE

{-# LANGUAGE DoAndIfThenElse #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE PackageImports #-}
{-# LANGUAGE QuasiQuotes #-}

module Process
  ( initReset
  , processWindowFocus
  , watchLeds
  , handleKeyboard
  , processKeysActions
  , processKeyboardState
  ) where


import "base" Control.Monad (when, unless, forever, guard)
import "lens" Control.Lens ((.~), (%~), (^.), set, over, view)
import "base" Control.Concurrent (threadDelay)
import "base" Control.Concurrent.MVar (MVar, modifyMVar_)
import "base" Control.Concurrent.Chan (Chan)
import "transformers" Control.Monad.IO.Class (liftIO)
import "transformers" Control.Monad.Trans.Class (lift)
import "transformers" Control.Monad.Trans.Maybe (MaybeT, runMaybeT)
import "transformers" Control.Monad.Trans.State (execStateT)
import qualified "mtl" Control.Monad.State.Class as St (MonadState(get, put))

import "base" Data.Maybe (fromJust, isJust)

import qualified "X11" Graphics.X11.Types       as XTypes
import qualified "X11" Graphics.X11.ExtraTypes  as XTypes
import qualified "X11" Graphics.X11.Xlib.Event  as XEvent
import qualified "X11" Graphics.X11.Xlib.Extras as XExtras
import "X11" Graphics.X11.Xlib.Misc (getInputFocus)
import "X11" Graphics.X11.Xlib.Types (Display)
import "X11" Graphics.X11.Types (Window)

-- local imports

import Utils ( dieWith
             , writeToFd
             , modifyState, modifyStateM
             , continueIf, continueUnless
             )
import Utils.X (nextEvent')
import Utils.Sugar ((&), (.>), (?))
import Utils.String (qm)
import Bindings.Xkb ( xkbSetGroup
                    , xkbListenForKeyboardStateEvents
                    , xkbGetCurrentLayout
                    )
import Bindings.MoreXlib (getLeds)
import qualified Options as O
import qualified Actions
import qualified State
import qualified Keys

import Process.Keyboard (handleKeyboard)
import Process.KeysActions (processKeysActions)
import qualified Process.CrossThread as CrossThread
  ( turnAlternativeMode
  , turnCapsLockMode
  , justTurnCapsLockMode
  , resetKbdLayout
  )


type Options         = O.Options
type KeyMap          = Keys.KeyMap
type State           = State.State
type LedModes        = State.LedModes
type CrossThreadVars = State.CrossThreadVars


initReset :: Options -> KeyMap -> Display -> IO ()
initReset opts keyMap dpy = do

  noise "Initial resetting of keyboard layout..."
  initialResetKbdLayout dpy

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

        initialResetKbdLayout :: Display -> IO ()
        initialResetKbdLayout dpy =
          xkbSetGroup dpy 0 >>= flip unless (dieWith "xkbSetGroup error")


processWindowFocus :: CrossThreadVars -> Options -> KeyMap -> Display -> IO ()
processWindowFocus ctVars opts keyMap dpy =

  XEvent.allocaXEvent $ \evPtr -> loop evPtr True

  where nextEvent = nextEvent'

        noise   = Actions.noise  opts ctVars        ::  String  -> IO ()
        noise'  = Actions.noise' opts ctVars        :: [String] -> IO ()
        notify' = Actions.notifyXmobar' opts ctVars :: [String] -> IO ()

        turnCapsLockMode :: State -> Bool -> IO State
        turnCapsLockMode = CrossThread.turnCapsLockMode ctVars noise' keyMap

        turnAlternativeMode :: State -> Bool -> IO State
        turnAlternativeMode = CrossThread.turnAlternativeMode noise' notify'

        resetKbdLayout :: State -> IO State
        resetKbdLayout = CrossThread.resetKbdLayout ctVars noise'

        loop :: XEvent.XEventPtr -> Bool -> IO ()
        loop evPtr needReselect = do

          when needReselect $ do

            liftIO $ noise "Getting current focused window id..."
            (wnd, _) <- liftIO $ getInputFocus dpy

            noise [qm|Listening for window {wnd} for focus events...|]
            XEvent.selectInput dpy wnd XTypes.focusChangeMask

          noise "Waiting for next X event about new window focus..."
          nextEvent dpy evPtr

          handle evPtr >>= loop evPtr

        handle :: XEvent.XEventPtr -> IO Bool
        handle evPtr =

          fmap (maybe False $ const True) $ runMaybeT $ do

          ev <- liftIO $ XExtras.getEvent evPtr
          let evName = XExtras.eventName ev
          continueIf $ evName `elem` ["FocusIn", "FocusOut"]

          liftIO $ noise [qm|Handling focus event: {evName}...|]

          when (O.resetByWindowFocusEvent opts) $
            liftIO $ modifyMVar_ (State.stateMVar ctVars) $
                                  execStateT $ do

              liftIO $ noise "Resetting keyboard layout..."
              modifyStateM $ liftIO . resetKbdLayout

              liftIO $ noise "Resetting Caps Lock mode..."
              modifyStateM $ liftIO . flip turnCapsLockMode False

              liftIO $ noise "Resetting Alternative mode..."
              modifyStateM $ liftIO . flip turnAlternativeMode False


-- FIXME waiting in blocking-mode for new leds event
-- Watch for new leds state and when new leds state is coming
-- store it in State, notify xmobar pipe and log.
watchLeds :: CrossThreadVars -> Options -> KeyMap -> Display -> IO ()
watchLeds ctVars opts _ dpy = forever $ f $ \leds prevState -> do

  let prevCapsLock = prevState ^. State.leds' . State.capsLockLed'
      newCapsLock  = leds ^. State.capsLockLed'
      prevNumLock  = prevState ^. State.leds' . State.numLockLed'
      newNumLock   = leds ^. State.numLockLed'

  when (view State.leds' prevState /= leds) $ do

    when (prevCapsLock /= newCapsLock) $ do
      notify [qm| capslock:{ newCapsLock ? "on" $ "off" }\n |]
      noise  [qm| Caps Lock is {newCapsLock ? "On" $ "Off"} |]

    when (prevNumLock /= newNumLock) $ do
      notify [qm| numlock:{ newNumLock ? "on" $ "off" }\n |]
      noise  [qm| Num Lock is {newNumLock ? "On" $ "Off"} |]

  return (prevState & State.leds' .~ leds)

  where noise  = Actions.noise opts ctVars        :: String -> IO ()
        notify = Actions.notifyXmobar opts ctVars :: String -> IO ()

        f :: (LedModes -> State -> IO State) -> IO ()
        f m = do
          leds <- getLeds dpy
          modifyMVar_ (State.stateMVar ctVars) (m leds)
          threadDelay $ 100 * 1000


-- Watch for keyboard layout change
processKeyboardState :: CrossThreadVars -> Options -> KeyMap -> Display -> IO ()
processKeyboardState ctVars opts _ dpy = do
  xkbListenForKeyboardStateEvents dpy
  XEvent.allocaXEvent $ \evPtr -> forever $ do
    noise "Waiting for next X event about new keyboard layout state..."
    nextEvent' dpy evPtr
    XExtras.getEvent evPtr
    layoutNum <- xkbGetCurrentLayout dpy
    noise [qm| Keyboard layout switched to: {layoutNum} |]
    modifyMVar_ (State.stateMVar ctVars) $
                return . (State.kbdLayout' .~ layoutNum)

  where noise = Actions.noise opts ctVars :: String -> IO ()
