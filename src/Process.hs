-- Author: Viacheslav Lotsmanov
-- License: GPLv3 https://raw.githubusercontent.com/unclechu/xlib-keys-hack/master/LICENSE

{-# LANGUAGE DoAndIfThenElse #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE PackageImports #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE LambdaCase #-}

module Process
  ( initReset
  , processWindowFocus
  , watchLeds
  , handleKeyboard
  , processKeysActions
  , processKeyboardState
  ) where

import "base" System.IO ( BufferMode(NoBuffering)
                        , Handle
                        , hSetBinaryMode
                        , hSetBuffering
                        , hGetLine
                        , hClose
                        , hIsEOF
                        )
import "process" System.Process ( CreateProcess(std_in, std_out, std_err)
                                , StdStream(CreatePipe, NoStream)
                                , ProcessHandle
                                , createProcess
                                , proc
                                , waitForProcess
                                , terminateProcess
                                , getProcessExitCode
                                )

import "base" Control.Monad (when, unless, forever)
import "lens" Control.Lens ((.~), (%~), (^.), set, over, view)
import "base" Control.Concurrent (threadDelay)
import "base" Control.Concurrent.MVar (modifyMVar_)
import "transformers" Control.Monad.IO.Class (liftIO)
import "transformers" Control.Monad.Trans.State (execStateT)
import "base" Control.Exception (Exception(fromException), throw, catch)

import "base" Data.Maybe (fromJust, isJust)
import "base" Data.Typeable (Typeable)

import qualified "X11" Graphics.X11.Xlib.Event  as XEvent
import qualified "X11" Graphics.X11.Xlib.Extras as XExtras
import "X11" Graphics.X11.Xlib.Types (Display)

-- local imports

import Utils ( dieWith
             , writeToFd
             , modifyState, modifyStateM
             , continueIf, continueUnless
             )
import Utils.X (nextEvent')
import Utils.Sugar ((&), (.>), (?), (|?|))
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


data IsEOFException = IsEOFException deriving (Show, Typeable)
instance Exception IsEOFException

processWindowFocus :: CrossThreadVars -> Options -> KeyMap -> Display -> IO ()
processWindowFocus ctVars opts keyMap _ = forever $ do

  noise [qm| Spawning subprocess of '{appExecPath}'
           \ to get window focus events... |]

  (Nothing, Just outH, Nothing, procH) <-
    createProcess (proc appExecPath []) { std_in  = NoStream
                                        , std_out = CreatePipe
                                        , std_err = NoStream
                                        }

  hSetBinaryMode outH False
  hSetBuffering outH NoBuffering

  noise [qm| Starting listening for window focus events
           \ from '{appExecPath}' subprocess ... |]
  (forever $ hIsEOF outH >>= throw IsEOFException |?| handle outH)
    `catch` \IsEOFException -> handleFail outH procH

  threadDelay $ 100 * 1000 -- Little delay between restarts

  where handle :: Handle -> IO ()
        handle outH = do
          noise [qm| Got new window focus event
                   \ from '{appExecPath}' subprocess |]
          hGetLine outH
          when (O.resetByWindowFocusEvent opts) reset

        reset :: IO ()
        reset = modifyMVar_ (State.stateMVar ctVars) $ execStateT $ do

          liftIO $ noise "Resetting keyboard layout..."
          modifyStateM $ liftIO . resetKbdLayout

          liftIO $ noise "Resetting Caps Lock mode..."
          modifyStateM $ liftIO . flip turnCapsLockMode False

          liftIO $ noise "Resetting Alternative mode..."
          modifyStateM $ liftIO . flip turnAlternativeMode False

        appExecPath :: FilePath
        appExecPath = "xlib-keys-hack-watch-for-window-focus-events"

        handleFail :: Handle -> ProcessHandle -> IO ()
        handleFail outH procH = do
          noise [qm| Subprocess '{appExecPath}' unexpectedly
                   \ closed its stdout |]
          hClose outH
          getProcessExitCode procH >>= \case
            Nothing -> do
              noise [qm| Subprocess '{appExecPath}' for some reason
                       \ still running, terminating it... |]
              terminateProcess procH
              exitCode <- waitForProcess procH
              noise [qm| Subprocess '{appExecPath}' just terminated
                       \ with exit code: {exitCode} |]
            Just exitCode ->
              noise [qm| Subprocess '{appExecPath}' was terminated with
                       \ with exit code: {exitCode} |]

        noise   = Actions.noise  opts ctVars        ::  String  -> IO ()
        noise'  = Actions.noise' opts ctVars        :: [String] -> IO ()
        notify' = Actions.notifyXmobar' opts ctVars :: [String] -> IO ()

        turnCapsLockMode :: State -> Bool -> IO State
        turnCapsLockMode = CrossThread.turnCapsLockMode ctVars noise' keyMap

        turnAlternativeMode :: State -> Bool -> IO State
        turnAlternativeMode = CrossThread.turnAlternativeMode noise' notify'

        resetKbdLayout :: State -> IO State
        resetKbdLayout = CrossThread.resetKbdLayout ctVars noise'


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
