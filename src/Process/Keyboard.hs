-- Author: Viacheslav Lotsmanov
-- License: GPLv3 https://raw.githubusercontent.com/unclechu/xlib-keys-hack/master/LICENSE

{-# LANGUAGE ImpredicativeTypes #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE ViewPatterns #-}

module Process.Keyboard
  ( handleKeyboard
  ) where

import qualified GHC.IO.Handle as IOHandle
import qualified System.Linux.Input.Event as EvdevEvent

import Control.Monad (when, unless, forM_)
import Control.Lens ((.~), (%~), (^.), set, over, view, Lens')
import Control.Concurrent.MVar (MVar, modifyMVar_)
import Control.Concurrent.Chan (Chan)

import Data.Maybe (Maybe(Just, Nothing), fromJust, isJust)
import qualified Data.Set as Set
import Text.Format (format)

import qualified Graphics.X11.Types       as XTypes
import qualified Graphics.X11.ExtraTypes  as XTypes
import Graphics.X11.Xlib.Types (Display)
import Graphics.X11.Types (Window)

import Utils ((&), (.>), (<||>), (?))
import Bindings.XTest (fakeKeyCodeEvent)
import qualified Options as O
import qualified Actions
import qualified State
import qualified Keys


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

      onOnlyBothAltsPressed :: Bool
      onOnlyBothAltsPressed =
        let set = Set.fromList [Keys.AltLeftKey, Keys.AltRightKey]
         in keyName `Set.member` set && pressed == set

      onAppleMediaPressed =
        Keys.FNKey `Set.member` pressed && isMedia keyName
        :: Bool

      onOnlyTwoControlsPressed :: Bool
      onOnlyTwoControlsPressed =
        let ctrls = Set.fromList [Keys.ControlLeftKey, Keys.ControlRightKey]
            additionalControls = Set.fromList [Keys.CapsLockKey, Keys.EnterKey]
         in pressed == ctrls
         || (O.additionalControls opts && pressed == additionalControls)

      onAdditionalControlKey =
        O.additionalControls opts &&
        keyName `elem` [Keys.CapsLockKey, Keys.EnterKey] &&
        not (
          keyName == Keys.EnterKey &&
          state ^. State.comboState' . State.isEnterPressedWithMods'
        )
        :: Bool

      onWithAdditionalControlKey =
        O.additionalControls opts &&
        any (`Set.member` pressed) [Keys.CapsLockKey, Keys.EnterKey] &&
        not (
          Keys.EnterKey `Set.member` pressed &&
          Keys.CapsLockKey `Set.notMember` pressed &&
          state ^. State.comboState' . State.isEnterPressedWithMods'
        )
        :: Bool

      onEnterOnlyWithMods :: Bool
      onEnterOnlyWithMods =
        O.additionalControls opts &&
        keyName == Keys.EnterKey &&
        let mods :: Set.Set Keys.KeyName
            mods = Set.fromList
              [ Keys.ControlLeftKey, Keys.ControlRightKey
              , Keys.ShiftLeftKey,   Keys.ShiftRightKey
              , Keys.AltLeftKey,     Keys.AltRightKey
              , Keys.SuperLeftKey,   Keys.SuperRightKey
              , Keys.CapsLockKey
              ]
            remappedMods :: Set.Set Keys.KeyName
            remappedMods =
              Set.foldr (Set.union . getRemappedByName) Set.empty mods

            allMods = mods `Set.union` remappedMods
            otherPressed = Set.delete Keys.EnterKey pressed

            pressedCase =
              isPressed &&
              not (Set.null otherPressed) &&
              Set.null (Set.foldr Set.delete otherPressed allMods)
            releasedCase =
              not isPressed &&
              state ^. State.comboState' . State.isEnterPressedWithMods'

         in pressedCase || releasedCase

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
        msg = "Triggering {0} of alternative {1} (X key code: {2}) by {3}..."
     in noise $ format msg [ status isPressed
                           , show keyNameTo
                           , show keyCodeTo
                           , show keyName
                           ]
    fakeKeyCodeEvent dpy keyCodeTo isPressed
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

  | onOnlyTwoControlsPressed -> do

    let isCapsLockModeOn = state ^. State.leds' . State.capsLockLed'
        msg = "Two controls pressed, turning {0} Caps Lock mode..."
     in noise $ format msg [("on" <||> "off") (not isCapsLockModeOn)]

    let off keyName =
          when (keyName `Set.member` pressed) $
            trigger keyName (fromJust $ getKeyCodeByName keyName) False
     in off Keys.ControlLeftKey
     >> off Keys.ControlRightKey

    let keyName = Keys.RealCapsLockKey
        keyCode = fromJust $ getRealKeyCodeByName keyName
     in pressRelease keyName keyCode

    let toDelete = [Keys.ControlLeftKey, Keys.ControlRightKey]
                     ++ if O.additionalControls opts
                           then [Keys.CapsLockKey, Keys.EnterKey]
                           else []
     in state
          & State.pressedKeys' .~ foldr Set.delete pressed toDelete
          & return

  -- Ability to press combos like Shift+Enter, Alt+Enter, etc.
  | onEnterOnlyWithMods -> do
    when isPressed $ noise $ show keyName ++ " pressed only with modifiers"
    justTrigger
    let lens = State.comboState' . State.isEnterPressedWithMods'
     in return (state & lens .~ isPressed)

  -- Handling of additional controls by `CapsLockKey` and `EnterKey`
  | onAdditionalControlKey ->

    let (flagLens, controlKeyName) = case keyName of
          Keys.CapsLockKey ->
            ( State.comboState' . State.isCapsLockUsedWithCombos'
            , Keys.ControlLeftKey
            )
          Keys.EnterKey ->
            ( State.comboState' . State.isEnterUsedWithCombos'
            , Keys.ControlRightKey
            )
          :: (Lens' State.State Bool, Keys.KeyName)
    in if

    -- Prevent triggering when just pressed
    | isPressed -> return state

    -- Trigger Control releasing because when you press
    -- `CapsLockKey` or `EnterKey` with combo (see below)
    -- it triggers Control pressing.
    | state ^. flagLens -> do
      let keyCode = fromJust $ getKeyCodeByName controlKeyName
      let msg1 = "{0} released after pressed with combos,\
                 \ it means it was interpreted as {1}"
          msg2 = "Triggering releasing of {0} (X key code: {1})..."
          params1 = [show keyName, show controlKeyName]
          params2 = [show controlKeyName, show keyCode]
       in noise' [format msg1 params1, format msg2 params2]
      fakeKeyCodeEvent dpy keyCode False
      return (state & flagLens .~ False)

    -- Just triggering default aliased key code to `CapsLockKey` or `EnterKey`
    | otherwise -> do
      case keyName of
           Keys.CapsLockKey -> asPressRelease keyName keyCode
           Keys.EnterKey    -> pressRelease   keyName keyCode
      return state

  -- When `CapsLockKey` or `EnterKey` pressed with combo
  | onWithAdditionalControlKey ->

    let (mainKeyName, flagLens, controlKeyName) = x
          where x :: (Keys.KeyName, Lens' State.State Bool, Keys.KeyName)
                x = if
                  | Keys.CapsLockKey `Set.member` pressed ->
                    ( Keys.CapsLockKey
                    , State.comboState' . State.isCapsLockUsedWithCombos'
                    , Keys.ControlLeftKey
                    )
                  | Keys.EnterKey `Set.member` pressed ->
                    ( Keys.EnterKey
                    , State.comboState' . State.isEnterUsedWithCombos'
                    , Keys.ControlRightKey
                    )
    in if

    -- When pressing of Control already triggered
    | state ^. flagLens -> justTrigger >> return state

    -- `CapsLockKey` or `EnterKey` pressed with combo,
    -- it means it should be interpreted as Control key.
    | otherwise -> do
      let keyCode = fromJust $ getKeyCodeByName controlKeyName
      let msg = "{0} pressed with combo, triggering {1} (X key code: {2})..."
          params = [show mainKeyName, show controlKeyName, show keyCode]
       in noise $ format msg params
      fakeKeyCodeEvent dpy keyCode True
      justTrigger
      return (state & flagLens .~ True)

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

  getRealKeyCodeByName :: Keys.KeyName -> Maybe XTypes.KeyCode
  getRealKeyCodeByName = Keys.getRealKeyCodeByName keyMap

  isAlternative = Keys.isAlternative keyMap :: Keys.KeyName -> Bool
  getAlternative :: Keys.KeyName -> Maybe (Keys.KeyName, XTypes.KeyCode)
  getAlternative = Keys.getAlternative keyMap

  isMedia  = Keys.isMedia keyMap  :: Keys.KeyName -> Bool
  getMedia = Keys.getMedia keyMap :: Keys.KeyName -> Maybe XTypes.KeyCode

  getAsName = Keys.getAsName keyMap :: Keys.KeyName -> Keys.KeyName
  getRemappedByName :: Keys.KeyName -> Set.Set Keys.KeyName
  getRemappedByName = Keys.getRemappedByName keyMap

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
            let status = "pressed" <||> "released"
             in noise $ format "{0} is {1}" [show keyName, status isPressed]
            modifyMVar_ (State.stateMVar ctVars) m

          -- Prevent doing anything when key state is the same
          ignoreDuplicates :: (State.State -> IO State.State) -> IO ()
          ignoreDuplicates m = throughState $ \state ->
            let pressed     = state ^. State.pressedKeys'
                isMember    = keyName `Set.member` pressed
                isDuplicate = isPressed == isMember
             in isDuplicate ? return state $ m state

          -- Store key user pressed in state
          storeKey :: (State.State -> IO State.State) -> IO ()
          storeKey m = ignoreDuplicates $ \state ->
            let action = isPressed ? Set.insert $ Set.delete
             in state & State.pressedKeys' %~ action keyName & m

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


altModeNotifyMsg :: Bool -> String
altModeNotifyMsg = "alternative:on\n" <||> "alternative:off\n"

onOff :: Bool -> String
onOff = "On" <||> "Off"
