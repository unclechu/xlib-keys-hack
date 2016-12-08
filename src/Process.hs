-- Author: Viacheslav Lotsmanov
-- License: GPLv3 https://raw.githubusercontent.com/unclechu/xlib-keys-hack/master/LICENSE

{-# LANGUAGE DoAndIfThenElse #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE MultiWayIf #-}

module Process
  ( initReset
  , processXEvents
  , watchLeds
  , handleKeyboard
  ) where

import System.Exit (exitFailure)
import qualified GHC.IO.Handle as IOHandle
import qualified System.Linux.Input.Event as EvdevEvent

import Control.Monad (when, unless, forM_)
import qualified Control.Monad.State as St
import Control.Monad.State.Class (MonadState)
import Control.Lens ((.~), (%~), (^.), set, over, view)
import Control.Concurrent (forkIO, ThreadId, threadDelay)
import Control.Concurrent.MVar (MVar, modifyMVar_)
import Control.Concurrent.Chan (Chan, writeChan)
import Control.Applicative ((<$>))

import Data.Maybe (Maybe(Just, Nothing), fromJust, isJust)
import Data.Bits ((.|.))
import qualified Data.Set as Set
import Text.Format (format)

import qualified Graphics.X11.Types       as XTypes
import qualified Graphics.X11.ExtraTypes  as XTypes
import qualified Graphics.X11.Xlib.Event  as XEvent
import qualified Graphics.X11.Xlib.Extras as XExtras
import Graphics.X11.Xlib.Misc (getInputFocus)
import Graphics.X11.Xlib.Types (Display)
import Graphics.X11.Types (Window)

import Utils ( (&), (.>), (<||>), (?)
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


initReset :: O.Options -> Keys.KeyMap -> Display -> Window -> IO ()
initReset opts keyMap dpy rootWnd = do

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
               -> Keys.KeyMap
               -> Display
               -> Window
               -> IO ()
processXEvents ctVars opts keyMap dpy rootWnd = process $ \wnd -> do

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
               noise $ format "Window focus moved from {0} to {1}"
                              [show lastWnd, show curWnd]
               return $ prevState { State.lastWindow = curWnd }

          where f :: (State.State -> IO State.State) -> IO ()
                f = modifyMVar_ $ State.stateMVar ctVars


-- FIXME waiting in blocking-mode for new leds event
-- Watch for new leds state and when new leds state is coming
-- store it in State, notify xmobar pipe and log.
watchLeds :: State.CrossThreadVars
          -> O.Options
          -> Keys.KeyMap
          -> Display
          -> Window
          -> IO ()
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

  where noise :: String -> IO ()
        noise = Actions.noise opts ctVars

        notifyStatus = "on\n" <||> "off\n"

        notify :: String -> IO ()
        notify = Actions.notifyXmobar opts ctVars

        f :: (State.LedModes -> State.State -> IO State.State) -> IO ()
        f m = do
          leds <- getLeds dpy
          modifyMVar_ (State.stateMVar ctVars) (m leds)
          threadDelay $ 100 * 1000


-- Wait for key events and simulate them in X server.
handleKeyboard :: State.CrossThreadVars
               -> O.Options
               -> Keys.KeyMap
               -> Display
               -> Window
               -> IOHandle.Handle
               -> IO ()
handleKeyboard ctVars opts keyMap dpy rootWnd fd =
  onEv $ \keyName keyCode isPressed state ->

  let pressed     = state ^. State.pressedKeys'
      alternative = getAlternative keyName

      onOnlyBothAltsPressed =
        (keyName == Keys.AltLeftKey &&
         pressed == Set.singleton Keys.AltRightKey) ||
        (keyName == Keys.AltRightKey &&
         pressed == Set.singleton Keys.AltLeftKey)
        :: Bool

      onAppleMediaPressed =
        Keys.FNKey `Set.member` pressed && isMedia keyName
        :: Bool

      justTrigger = trigger keyName keyCode isPressed :: IO ()

  in if

  -- Alternative mode on/off by Alts handling
  | onOnlyBothAltsPressed -> do
    justTrigger
    let newState = state & State.alternative' %~ not
    noise $ "Notifying xmobar about alternative mode is "
         ++ onOff (State.alternative newState)
         ++ "..."
    notify $ altModeNotifyMsg $ State.alternative newState
    return newState

  -- Alternative mode keys handling
  | state ^. State.alternative' && isJust alternative -> do
    let Just (keyNameTo, keyCodeTo) = alternative
    let status = "pressing" <||> "releasing"
        msg = "Triggering {0} of alternative {1}\
              \ (X key code: {2}) by {3}..."
     in noise $ format msg [ status isPressed
                           , show keyNameTo
                           , show keyCodeTo
                           , show keyName
                           ]
    -- fakeKeyCodeEvent dpy keyCodeTo isPressed
    return state

  -- Hadling `FNKey` pressing on apple keyboard
  | keyName == Keys.FNKey -> if

    -- Prevent triggering when just pressed
    | isPressed -> return state

    -- When releasing `FNKey` after some media keys pressed
    | state ^. State.comboState' . State.appleMediaPressed' -> do
      restPressed <- releaseAppleMedia $ State.pressedKeys state
      state
        & State.comboState' . State.appleMediaPressed' .~ False
        & State.pressedKeys' .~ restPressed
        & return

    -- As `InsertKey` (because no media pressed)
    | otherwise -> do
      asPressRelease keyName keyCode
      return state

  -- When held `FNKey` on apple keyboard and press some media key
  | onAppleMediaPressed -> do
    let msg = "Apple media key pressed, preventing triggering {0} as {1}..."
     in noise $ format msg [show Keys.FNKey, show Keys.InsertKey]
    justTrigger
    return (state & State.comboState' . State.appleMediaPressed' .~ True)

  -- Handling of `CapsLockKey` key (Escape/Caps Lock or Left Control)
  | keyName == Keys.CapsLockKey -> if

    -- Prevent triggering when just pressed
    | isPressed -> return state

    -- Trigger `ControlLeftKey` releasing because when you press
    -- `CapsLockKey` with another key
    -- it triggers `ControlLeftKey` pressing.
    | state ^. State.comboState' . State.isCapsLockUsedWithCombos' -> do
      let keyCode = fromJust $ getKeyCodeByName Keys.ControlLeftKey
      let msg1 = "{0} released after pressed with combos,\
                 \ it means it was interpreted as {1}"
          msg2 = "Triggering releasing of {0} (X key code: {1})..."
          params1 = [show Keys.CapsLockKey, show Keys.ControlLeftKey]
          params2 = [show Keys.ControlLeftKey, show keyCode]
       in noise' [format msg1 params1, format msg2 params2]
      fakeKeyCodeEvent dpy keyCode False
      state
        & State.comboState' . State.isCapsLockUsedWithCombos' .~ False
        & return

    -- Just triggering default key code (Escape/Caps Lock)
    | otherwise -> do
      asPressRelease keyName keyCode
      return state

  -- When `CapsLockKey` pressed with combo
  | Keys.CapsLockKey `Set.member` pressed -> if

    -- When press of `ControlLeftKey` already triggered
    | state ^. State.comboState' . State.isCapsLockUsedWithCombos' ->
      justTrigger >> return state

    -- `CapsLockKey` pressed with combo,
    -- it means it should be interpreted as `ControlLeftKey`.
    | otherwise -> do
      let keyCode = fromJust $ getKeyCodeByName Keys.ControlLeftKey
      let msg = "{0} pressed with combo, triggering {1} (X key code: {2})..."
          params = [ show Keys.CapsLockKey
                   , show Keys.ControlLeftKey
                   , show keyCode
                   ]
       in noise $ format msg params
      fakeKeyCodeEvent dpy keyCode True
      justTrigger
      state
        & State.comboState' . State.isCapsLockUsedWithCombos' .~ True
        & return

  -- Usual key handling
  | otherwise -> justTrigger >> return state

  where

  noise  = Actions.noise        opts ctVars ::  String  -> IO ()
  noise' = Actions.noise'       opts ctVars :: [String] -> IO ()
  notify = Actions.notifyXmobar opts ctVars ::  String  -> IO ()

  getAlias :: EvdevEvent.Key -> Maybe Keys.KeyAlias
  getAlias = Keys.getAliasByKey keyMap

  getKeyCodeByName :: Keys.KeyName -> Maybe XTypes.KeyCode
  getKeyCodeByName = Keys.getKeyCodeByName keyMap

  getAlternative :: Keys.KeyName -> Maybe (Keys.KeyName, XTypes.KeyCode)
  getAlternative = Keys.getAlternative keyMap

  isAlternative :: Keys.KeyName -> Bool
  isAlternative = Keys.isAlternative keyMap

  getMedia = Keys.getMedia keyMap :: Keys.KeyName -> Maybe XTypes.KeyCode
  isMedia  = Keys.isMedia keyMap  :: Keys.KeyName -> Bool

  getAsName = Keys.getAsName keyMap :: Keys.KeyName -> Keys.KeyName

  -- Wait and extract event, make preparations and call handler
  onEv :: (Keys.KeyName
           -> XTypes.KeyCode
           -> Bool
           -> State.State
           -> IO State.State)
       -> IO ()
  onEv m = do
    evMaybe <- EvdevEvent.hReadEvent fd
    case evMaybe of
      Just EvdevEvent.KeyEvent
             { EvdevEvent.evKeyCode      = (getAlias -> Just (name, _, code))
             , EvdevEvent.evKeyEventType = (checkPress -> Just isPressed)
             }
        -> chain (name, isPressed) $ m name code isPressed
      _ -> return ()

    where checkPress :: EvdevEvent.KeyEventType -> Maybe Bool
          checkPress x = case x of
                              EvdevEvent.Depressed -> Just True
                              EvdevEvent.Released  -> Just False
                              _ -> Nothing

  -- Composed prepare actions
  chain :: (Keys.KeyName, Bool)
        -> (State.State -> IO State.State)
        -> IO ()
  chain (keyName, isPressed) = storeKey

    where throughState :: (State.State -> IO State.State) -> IO ()
          throughState m = do
            -- Log key user pressed (even if it's ignored or replaced)
            let status = "is pressed" <||> "is released"
             in noise $ format "Key {0} {1}" [show keyName, status isPressed]
            modifyMVar_ (State.stateMVar ctVars) m

          -- Prevent doing anything when key state is the same
          ignoreDuplicates :: (State.State -> IO State.State) -> IO ()
          ignoreDuplicates m = throughState $ \state ->
            let pressed     = state ^. State.pressedKeys'
                isMember    = keyName `Set.member` pressed
                isDuplicate = isPressed == isMember
             in if isDuplicate then return state else m state

          -- Store key user pressed in state
          storeKey :: (State.State -> IO State.State) -> IO ()
          storeKey m = ignoreDuplicates $ \state ->
            let action = if isPressed then Set.insert else Set.delete
             in state & State.pressedKeys' %~ action keyName & m

  altModeNotifyMsg :: Bool -> String
  altModeNotifyMsg = "alternative:on\n" <||> "alternative:off\n"

  abstractRelease :: String -- `releaseMsg`
                  -> String -- `releaseItemMsgMask`
                  -> (Keys.KeyName -> Bool) -- `splitter`
                  -> (Keys.KeyName -> Maybe XTypes.KeyCode) -- `getter`
                  -> Set.Set Keys.KeyName -- `pressed`
                  -> IO (Set.Set Keys.KeyName) -- returns rest of `pressed`
  abstractRelease releaseMsg releaseItemMsgMask splitter getter pressed = do
    let (toRelease, rest) = Set.partition splitter pressed
    when (Set.size toRelease > 0) $ do
      noise releaseMsg
      forM_ (Set.toList toRelease) $ \keyName -> do
        noise $ format releaseItemMsgMask [show keyName]
        let Just keyCode = getter keyName
         in fakeKeyCodeEvent dpy keyCode False
    return rest

  -- Release alternative keys.
  -- Useful when alternative mode turns off not by both alts
  -- and key could be still pressed.
  releaseAlternative :: Set.Set Keys.KeyName -> IO (Set.Set Keys.KeyName)
  releaseAlternative = abstractRelease
    "Releasing alternative keys during turning alternative mode off..."
    "Releasing alternative {0} during turning alternative mode off..."
    isAlternative
    (getAlternative .> fmap snd)

  -- Release apple media keys.
  -- Useful when user released `FNKey` erlier than media key.
  releaseAppleMedia :: Set.Set Keys.KeyName -> IO (Set.Set Keys.KeyName)
  releaseAppleMedia = abstractRelease
    ("Releasing held media keys of apple keyboard after "
      ++ show Keys.FNKey ++ " released...")
    ("Releasing held media {0} of apple keyboard after "
      ++ show Keys.FNKey ++ " released...")
    isMedia
    getMedia

  -- Simple triggering key user pressed to X server
  trigger :: Keys.KeyName -> XTypes.KeyCode -> Bool -> IO ()
  trigger keyName keyCode isPressed = do
    let status = "pressing" <||> "releasing"
        msg = "Triggering {0} of {1} (X key code: {2})..."
     in noise $ format msg [status isPressed, show keyName, show keyCode]
    fakeKeyCodeEvent dpy keyCode isPressed

  -- Triggering both press and release events to X server
  pressRelease :: Keys.KeyName -> XTypes.KeyCode -> IO ()
  pressRelease keyName keyCode = do
    let msg = "Triggering pressing and releasing of {0} (X key code: {1})..."
     in noise $ format msg [show keyName, show keyCode]
    let f = fakeKeyCodeEvent dpy keyCode in f True >> f False

  -- Triggering both press and release events to X server.
  -- Also write to log about to which key this key remapped.
  asPressRelease :: Keys.KeyName -> XTypes.KeyCode -> IO ()
  asPressRelease keyName keyCode = do
    let msg = "Triggering pressing and releasing\
              \ of {0} as {1} (X key code: {2})..."
        params = [show keyName, show (getAsName keyName), show keyCode]
     in noise $ format msg params
    let f = fakeKeyCodeEvent dpy keyCode in f True >> f False


onOff :: Bool -> String
onOff = "On" <||> "Off"
