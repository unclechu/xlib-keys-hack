-- Author: Viacheslav Lotsmanov
-- License: GPLv3 https://raw.githubusercontent.com/unclechu/xlib-keys-hack/master/LICENSE

{-# LANGUAGE DoAndIfThenElse #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Process
  ( initReset
  , processWindowFocus
  , watchLeds

  , HandledKey
  , handleKeyEvent
  , getNextKeyboardDeviceKeyEvent

  , SoftwareDebouncer
  , getSoftwareDebouncer
  , getSoftwareDebouncerTiming
  , moveKeyThroughSoftwareDebouncer
  , handleNextSoftwareDebouncerEvent

  , processKeysActions
  , processKeyboardState
  ) where

import "base" System.IO ( BufferMode (NoBuffering)
                        , Handle
                        , hSetBinaryMode
                        , hSetBuffering
                        , hGetLine
                        , hClose
                        , hIsEOF
                        )
import "process" System.Process ( CreateProcess (std_in, std_out, std_err)
                                , StdStream (CreatePipe, NoStream)
                                , ProcessHandle
                                , createProcess
                                , proc
                                , waitForProcess
                                , terminateProcess
                                , getProcessExitCode
                                )

import "base" Control.Monad (when, unless, forever)
import "lens" Control.Lens ((.~), (^.), view)
import "base" Control.Concurrent (threadDelay)
import "base" Control.Concurrent.MVar (modifyMVar_)
import "transformers" Control.Monad.Trans.State (execStateT)
import "base" Control.Exception (Exception, throw, catch)

import "base" Data.Typeable (Typeable)
import "qm-interpolated-string" Text.InterpolatedString.QM (qm, qms)

import qualified "X11" Graphics.X11.Xlib.Event  as XEvent
import qualified "X11" Graphics.X11.Xlib.Extras as XExtras
import "X11" Graphics.X11.Xlib.Types (Display)

-- local imports

import           Utils (dieWith)
import           Utils.X (nextEvent')
import           Utils.Sugar ((&), (?), (|?|))
import           Bindings.Xkb ( xkbSetGroup
                              , xkbListenForKeyboardStateEvents
                              , xkbGetCurrentLayout
                              )
import           Bindings.MoreXlib (getLeds)
import           IPC (IPCHandle, setIndicatorState)
import           Options (type Options)
import qualified Options as O
import qualified Actions
import           State (type State, type LedModes, type CrossThreadVars)
import qualified State
import           Keys (type KeyMap)

import           Process.Keyboard.Types (HandledKey)
import           Process.Keyboard.HandlingKeyEventFlow (handleKeyEvent)
import           Process.Keyboard.RawDeviceHandling
                   ( getNextKeyboardDeviceKeyEvent
                   )
import           Process.Keyboard.SoftwareDebouncer
                   ( SoftwareDebouncer
                   , getSoftwareDebouncer
                   , getSoftwareDebouncerTiming
                   , moveKeyThroughSoftwareDebouncer
                   , handleNextSoftwareDebouncerEvent
                   )
import           Process.KeysActions (processKeysActions)
import qualified Process.CrossThread as CrossThread ( justTurnCapsLockMode
                                                    , resetAll
                                                    )


initReset :: Options -> Maybe IPCHandle -> KeyMap -> Display -> IO ()
initReset opts ipcHandle keyMap dpy = do

  noise "Initial resetting of keyboard layout..."
  initialResetKbdLayout dpy

  noise "Initial resetting Caps Lock mode..."
  justTurnCapsLockMode False

  when (O.xmobarIndicators opts) $ do
    noise "Initial resetting of xmobar indicators..."
    flip (maybe $ return ()) ipcHandle $ \ipc -> do
      setIndicatorState ipc $ Actions.XmobarNumLockFlag     False
      setIndicatorState ipc $ Actions.XmobarCapsLockFlag    False
      setIndicatorState ipc $ Actions.XmobarAlternativeFlag False

      xkbGetCurrentLayout dpy
        >>= setIndicatorState ipc . Actions.XmobarXkbLayout . fromIntegral

  where noise = O.noise opts
        justTurnCapsLockMode =
          CrossThread.justTurnCapsLockMode dpy noise keyMap

        initialResetKbdLayout :: Display -> IO ()
        initialResetKbdLayout _dpy =
          xkbSetGroup _dpy 0 >>= flip unless (dieWith "xkbSetGroup error")


data IsEOFException = IsEOFException deriving (Show, Typeable)
instance Exception IsEOFException

processWindowFocus :: CrossThreadVars -> Options -> IO ()
processWindowFocus ctVars opts = forever $ do

  noise [qms| Spawning subprocess of '{appExecPath}'
              to get window focus events... |]

  (Nothing, Just outH, Nothing, procH) <-
    createProcess (proc appExecPath []) { std_in  = NoStream
                                        , std_out = CreatePipe
                                        , std_err = NoStream
                                        }

  hSetBinaryMode outH False
  hSetBuffering outH NoBuffering

  noise [qm| Storing handlers of subprocess '{appExecPath}' in the state... |]
  modifyMVar_
    (State.stateMVar ctVars) $
    return . (State.windowFocusProc' .~ Just (appExecPath, procH, outH))

  noise [qms| Starting listening for window focus events
              from '{appExecPath}' subprocess... |]
  forever (hIsEOF outH >>= throw IsEOFException |?| handle outH)
    `catch` \IsEOFException -> handleFail outH procH

  threadDelay $ 100 * 1000 -- Little delay between restarts

  where handle :: Handle -> IO ()
        handle outH = do
          noise [qms| Got new window focus event
                      from '{appExecPath}' subprocess |]
          _ <- hGetLine outH
          when (O.resetByWindowFocusEvent opts) $
            modifyMVar_ (State.stateMVar ctVars) $ execStateT $
              CrossThread.resetAll opts ctVars noise' notify'

        appExecPath :: FilePath
        appExecPath = "xlib-keys-hack-watch-for-window-focus-events"

        handleFail :: Handle -> ProcessHandle -> IO ()
        handleFail outH procH = do
          scream [qms| Subprocess '{appExecPath}' unexpectedly
                       closed its stdout |]
          hClose outH
          getProcessExitCode procH >>= \case
            Nothing -> do
              scream [qms| Subprocess '{appExecPath}' for some reason
                           still running, terminating it... |]
              terminateProcess procH
              exitCode <- waitForProcess procH
              scream [qms| Subprocess '{appExecPath}' just terminated
                           with exit code: {exitCode} |]
            Just exitCode ->
              scream [qms| Subprocess '{appExecPath}' was terminated with
                           with exit code: {exitCode} |]

        noise   = Actions.noise  opts ctVars        ::  String  -> IO ()
        noise'  = Actions.noise' opts ctVars        :: [String] -> IO ()
        scream  = Actions.panicNoise ctVars         ::  String  -> IO ()

        notify' :: [Actions.XmobarFlag] -> IO ()
        notify' = Actions.notifyXmobar' opts ctVars


-- FIXME waiting in blocking-mode for new leds event
-- Watch for new leds state and when new leds state is coming
-- store it in State, notify xmobar pipe and log.
watchLeds :: CrossThreadVars -> Options -> Display -> IO ()
watchLeds ctVars opts dpy = forever $ f $ \leds prevState -> do

  let prevCapsLock = prevState ^. State.leds' . State.capsLockLed'
      newCapsLock  = leds ^. State.capsLockLed'
      prevNumLock  = prevState ^. State.leds' . State.numLockLed'
      newNumLock   = leds ^. State.numLockLed'

  when (view State.leds' prevState /= leds) $ do

    when (prevCapsLock /= newCapsLock) $ do
      notify $ Actions.XmobarCapsLockFlag newCapsLock
      noise  [qm| Caps Lock is {newCapsLock ? "On" $ "Off"} |]

    when (prevNumLock /= newNumLock) $ do
      notify $ Actions.XmobarNumLockFlag newNumLock
      noise  [qm| Num Lock is {newNumLock ? "On" $ "Off"} |]

  return $ prevState & State.leds' .~ leds

  where noise  = Actions.noise opts ctVars        :: String -> IO ()
        notify = Actions.notifyXmobar opts ctVars :: Actions.XmobarFlag -> IO ()

        f :: (LedModes -> State -> IO State) -> IO ()
        f m = do
          leds <- getLeds dpy
          modifyMVar_ (State.stateMVar ctVars) (m leds)
          threadDelay $ 100 * 1000


-- Watch for keyboard layout change
processKeyboardState :: CrossThreadVars -> Options -> Display -> IO ()
processKeyboardState ctVars opts dpy = do
  xkbListenForKeyboardStateEvents dpy
  XEvent.allocaXEvent $ \evPtr -> forever $ do
    noise "Waiting for next X event about new keyboard layout state..."
    nextEvent' dpy evPtr
    _ <- XExtras.getEvent evPtr
    (fromIntegral -> layoutNum) <- xkbGetCurrentLayout dpy

    modifyMVar_ (State.stateMVar ctVars) $ \state -> do

      when (State.kbdLayout state /= layoutNum) $ do
        notify $ Actions.XmobarXkbLayout layoutNum
        noise  [qm| Keyboard layout switched to: {layoutNum} |]

      return state { State.kbdLayout = layoutNum }

  where noise  = Actions.noise opts ctVars :: String -> IO ()
        notify = Actions.notifyXmobar opts ctVars :: Actions.XmobarFlag -> IO ()
