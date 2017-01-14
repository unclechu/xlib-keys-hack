-- Author: Viacheslav Lotsmanov
-- License: GPLv3 https://raw.githubusercontent.com/unclechu/xlib-keys-hack/master/LICENSE

{-# LANGUAGE ImpredicativeTypes #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE PackageImports #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE LambdaCase #-}

module Process.Keyboard
  ( handleKeyboard
  ) where

import qualified "base" GHC.IO.Handle as IOHandle
import qualified "linux-evdev" System.Linux.Input.Event as EvdevEvent

import "transformers" Control.Monad.Trans.Class (lift)
import "base" Control.Monad (when, unless, forM_)
import "lens" Control.Lens ((.~), (%~), (^.), set, over, view, Lens')
import "base" Control.Concurrent.MVar (MVar, modifyMVar_)
import "base" Control.Concurrent.Chan (Chan)
import "either" Control.Monad.Trans.Either (EitherT, runEitherT, left, right)

import "base" Data.Maybe (fromJust, isJust)
import qualified "containers" Data.Set as Set

import qualified "X11" Graphics.X11.Types      as XTypes
import qualified "X11" Graphics.X11.ExtraTypes as XTypes
import "X11" Graphics.X11.Xlib.Types (Display)
import "X11" Graphics.X11.Types (Window)

-- local imports

import Utils ((&), (.>), (<||>), (?))
import Utils.String (qm)
import Bindings.XTest (fakeKeyCodeEvent)
import qualified Options as O
import qualified Actions
import qualified State
import qualified Keys

import qualified Process.CrossThread as CrossThread
  ( handleCapsLockModeChange
  , handleAlternativeModeChange
  , toggleCapsLock
  , toggleAlternative
  , handleResetKbdLayout
  )


type KeyCode         = XTypes.KeyCode
type Handle          = IOHandle.Handle
type Set             = Set.Set

type Options         = O.Options
type KeyMap          = Keys.KeyMap
type KeyName         = Keys.KeyName
type KeyAlias        = Keys.KeyAlias
type State           = State.State
type CrossThreadVars = State.CrossThreadVars


-- Wait for key events and simulate them in X server.
handleKeyboard :: CrossThreadVars
               -> Options
               -> KeyMap
               -> Display
               -> Window
               -> Handle
               -> IO ()
handleKeyboard ctVars opts keyMap dpy rootWnd fd =
  onEv $ \keyName keyCode isPressed state ->

  let pressed     = state ^. State.pressedKeys'
      alternative = getAlternative keyName

      onOnlyBothAltsPressed :: Bool
      onOnlyBothAltsPressed =
        let set = Set.fromList [Keys.AltLeftKey, Keys.AltRightKey]
         in keyName `Set.member` set && pressed == set

      onAppleMediaPressed :: Bool
      onAppleMediaPressed = Keys.FNKey `Set.member` pressed && isMedia keyName

      onOnlyTwoControlsPressed :: Bool
      onOnlyTwoControlsPressed =
        let ctrls = Set.fromList [Keys.ControlLeftKey, Keys.ControlRightKey]
            additionalControls = Set.fromList [Keys.CapsLockKey, Keys.EnterKey]
         in pressed == ctrls
         || (O.additionalControls opts && pressed == additionalControls)

      onAdditionalControlKey :: Bool
      onAdditionalControlKey =
        O.additionalControls opts &&
        keyName `elem` [Keys.CapsLockKey, Keys.EnterKey] &&
        not (
          keyName == Keys.EnterKey &&
          state ^. State.comboState' . State.isEnterPressedWithMods'
        )

      onWithAdditionalControlKey :: Bool
      onWithAdditionalControlKey =
        O.additionalControls opts &&
        any (`Set.member` pressed) [Keys.CapsLockKey, Keys.EnterKey] &&
        not (
          Keys.EnterKey `Set.member` pressed &&
          Keys.CapsLockKey `Set.notMember` pressed &&
          state ^. State.comboState' . State.isEnterPressedWithMods'
        )

      onEnterOnlyWithMods :: Bool
      onEnterOnlyWithMods =
        O.additionalControls opts &&
        keyName == Keys.EnterKey &&
        let mods :: Set KeyName
            mods = Set.fromList
              [ Keys.ControlLeftKey, Keys.ControlRightKey
              , Keys.ShiftLeftKey,   Keys.ShiftRightKey
              , Keys.AltLeftKey,     Keys.AltRightKey
              , Keys.SuperLeftKey,   Keys.SuperRightKey
              , Keys.CapsLockKey
              ]
            remappedMods =
              Set.foldr (Set.union . getRemappedByName) Set.empty mods
              :: Set KeyName

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

      off :: KeyName -> IO ()
      off keyName =
        when (keyName `Set.member` pressed) $
          trigger keyName (fromJust $ getKeyCodeByName keyName) False

  in if

  -- Alternative mode on/off by Alts handling
  | onOnlyBothAltsPressed -> do

    noise "Two alts pressed, it means Alternative mode toggling"
    let toDelete = [Keys.AltLeftKey, Keys.AltRightKey]
    forM_ toDelete off
    state
      & State.pressedKeys' .~ foldr Set.delete pressed toDelete
      & toggleAlternative

  -- Alternative mode keys handling
  | state ^. State.alternative' && isJust alternative -> do
    let Just (keyNameTo, keyCodeTo) = alternative
    noise [qm| Triggering {isPressed ? "pressing" $ "releasing"}
             \ of alternative {keyNameTo}
             \ (X key code: {keyCodeTo}) by {keyName}... |]
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
    noise [qm| Apple media key pressed, preventing triggering
             \ {Keys.FNKey} as {Keys.InsertKey}... |]
    justTrigger
    return (state & State.comboState' . State.appleMediaPressed' .~ True)

  | onOnlyTwoControlsPressed -> do

    noise "Two controls pressed, it means Caps Lock mode toggling"

    off Keys.ControlLeftKey
    off Keys.ControlRightKey

    let toDelete = [Keys.ControlLeftKey, Keys.ControlRightKey]
                     ++ if O.additionalControls opts
                           then [Keys.CapsLockKey, Keys.EnterKey]
                           else []
     in state
      & State.pressedKeys' .~ foldr Set.delete pressed toDelete
      & toggleCapsLock

  -- Ability to press combos like Shift+Enter, Alt+Enter, etc.
  | onEnterOnlyWithMods -> do
    when isPressed $ noise [qm|{keyName} pressed only with modifiers|]
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
          :: (Lens' State Bool, KeyName)
    in if

    -- Prevent triggering when just pressed
    | isPressed -> return state

    -- Trigger Control releasing because when you press
    -- `CapsLockKey` or `EnterKey` with combo (see below)
    -- it triggers Control pressing.
    | state ^. flagLens -> do
      let keyCode = fromJust $ getKeyCodeByName controlKeyName
      noise' [ [qm| {keyName} released after pressed with combos,
                  \ it means it was interpreted as {controlKeyName} |]
             , [qm| Triggering releasing of {controlKeyName}
                  \ (X key code: {keyCode})... |]
             ]
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
          where x :: (KeyName, Lens' State Bool, KeyName)
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
      noise [qm| {mainKeyName} pressed with combo,
               \ triggering {controlKeyName} (X key code: {keyCode})... |]
      fakeKeyCodeEvent dpy keyCode True
      justTrigger
      return (state & flagLens .~ True)

  -- Usual key handling
  | otherwise -> justTrigger >> return state

  where

  noise   = Actions.noise         opts ctVars ::  String  -> IO ()
  noise'  = Actions.noise'        opts ctVars :: [String] -> IO ()
  notify' = Actions.notifyXmobar' opts ctVars :: [String] -> IO ()

  getAlias :: EvdevEvent.Key -> Maybe KeyAlias
  getAlias = Keys.getAliasByKey keyMap

  getKeyCodeByName :: KeyName -> Maybe KeyCode
  getKeyCodeByName = Keys.getKeyCodeByName keyMap

  getRealKeyCodeByName :: KeyName -> Maybe KeyCode
  getRealKeyCodeByName = Keys.getRealKeyCodeByName keyMap

  isAlternative = Keys.isAlternative keyMap :: KeyName -> Bool
  getAlternative :: KeyName -> Maybe (KeyName, KeyCode)
  getAlternative = Keys.getAlternative keyMap

  isMedia  = Keys.isMedia keyMap  :: KeyName -> Bool
  getMedia = Keys.getMedia keyMap :: KeyName -> Maybe KeyCode

  getAsName = Keys.getAsName keyMap :: KeyName -> KeyName
  getRemappedByName :: KeyName -> Set KeyName
  getRemappedByName = Keys.getRemappedByName keyMap

  toggleCapsLock :: State -> IO State
  toggleCapsLock = CrossThread.toggleCapsLock dpy noise' keyMap

  toggleAlternative :: State -> IO State
  toggleAlternative = CrossThread.toggleAlternative noise' notify'

  handleCapsLockModeChange :: State -> IO State
  handleCapsLockModeChange =
    CrossThread.handleCapsLockModeChange dpy noise' keyMap

  handleAlternativeModeChange :: State -> IO State
  handleAlternativeModeChange =
    CrossThread.handleAlternativeModeChange dpy noise' notify' keyMap

  handleResetKbdLayout :: State -> IO State
  handleResetKbdLayout = CrossThread.handleResetKbdLayout dpy noise'

  -- Wait and extract event, make preparations and call handler
  onEv :: (KeyName -> KeyCode -> Bool -> State -> IO State) -> IO ()
  onEv m = EvdevEvent.hReadEvent fd >>= \case

    Just EvdevEvent.KeyEvent
           { EvdevEvent.evKeyCode      = (getAlias -> Just (name, _, code))
           , EvdevEvent.evKeyEventType = (checkPress -> Just isPressed)
           }
             -> chain (name, isPressed) $ m name code isPressed

    _ -> return ()

    where checkPress :: EvdevEvent.KeyEventType -> Maybe Bool
          checkPress = \case EvdevEvent.Depressed -> Just True
                             EvdevEvent.Released  -> Just False
                             _ -> Nothing

  -- Composed prepare actions
  chain :: (KeyName, Bool) -> (State -> IO State) -> IO ()
  chain (keyName, isPressed) handleM = do

    -- Log key user pressed (even if it's ignored or replaced)
    noise [qm| {keyName} is {isPressed ? "pressed" $ "released"} |]

    let f :: State -> IO State
        f = ignoreDuplicates
         .> (>>= storeKey)
         .> (>>= lift . handleM)
         .> (>>= lift . handleResetKbdLayout)
         .> (>>= lift . handleCapsLockModeChange)
         .> (>>= lift . handleAlternativeModeChange)
         .> runEitherT
         .> fmap (either id id)
     in modifyMVar_ (State.stateMVar ctVars) f

    where -- Prevent doing anything when key state is the same
          ignoreDuplicates :: State -> EitherT State IO State
          ignoreDuplicates state =
            let pressed     = state ^. State.pressedKeys'
                isMember    = keyName `Set.member` pressed
                isDuplicate = isPressed == isMember
             in (isDuplicate ? left $ right) state

          -- Store key user pressed in state
          storeKey :: State -> EitherT State IO State
          storeKey state =
            let action = isPressed ? Set.insert $ Set.delete
             in return (state & State.pressedKeys' %~ action keyName)

  abstractRelease :: String -- `releaseMsg`
                  -> (KeyName -> String) -- `releaseItemMsgMask`
                  -> (KeyName -> Bool) -- `splitter`
                  -> (KeyName -> Maybe KeyCode) -- `getter`
                  -> Set KeyName -- `pressed`
                  -> IO (Set KeyName) -- returns rest of `pressed`
  abstractRelease releaseMsg releaseItemMsgMask splitter getter pressed = do
    let (toRelease, rest) = Set.partition splitter pressed
    when (Set.size toRelease > 0) $ do
      noise releaseMsg
      forM_ (Set.toList toRelease) $ \keyName -> do
        noise $ releaseItemMsgMask keyName
        let Just keyCode = getter keyName
         in fakeKeyCodeEvent dpy keyCode False
    return rest

  -- Release alternative keys.
  -- Useful when alternative mode turns off not by both alts
  -- and key could be still pressed.
  -- -- It's commented because it's never used anywhere
  -- releaseAlternative :: Set KeyName -> IO (Set KeyName)
  -- releaseAlternative = abstractRelease
  --   "Releasing alternative keys during turning alternative mode off..."
  --   (\keyName -> [qm| Releasing alternative {keyName}
  --                   \ during turning alternative mode off... |])
  --   isAlternative
  --   (getAlternative .> fmap snd)

  -- Release apple media keys.
  -- Useful when user released `FNKey` erlier than media key.
  releaseAppleMedia :: Set KeyName -> IO (Set KeyName)
  releaseAppleMedia = abstractRelease
    [qm| Releasing held media keys of apple keyboard
       \ after {Keys.FNKey} released... |]
    (\keyName -> [qm| Releasing held media {keyName} of apple keyboard
                    \ after {Keys.FNKey} released... |])
    isMedia
    getMedia

  -- Simple triggering key user pressed to X server
  trigger :: KeyName -> KeyCode -> Bool -> IO ()
  trigger keyName keyCode isPressed = do
    noise [qm| Triggering {isPressed ? "pressing" $ "releasing"}
             \ of {keyName} (X key code: {keyCode})... |]
    fakeKeyCodeEvent dpy keyCode isPressed

  -- Triggering both press and release events to X server
  pressRelease :: KeyName -> KeyCode -> IO ()
  pressRelease keyName keyCode = do
    noise [qm| Triggering pressing and releasing of {keyName}
             \ (X key code: {keyCode})... |]
    let f = fakeKeyCodeEvent dpy keyCode in f True >> f False

  -- Triggering both press and release events to X server.
  -- Also write to log about to which key this key remapped.
  asPressRelease :: KeyName -> KeyCode -> IO ()
  asPressRelease keyName keyCode = do
    noise [qm| Triggering pressing and releasing
             \ of {keyName} as {getAsName keyName}
             \ (X key code: {keyCode})... |]
    let f = fakeKeyCodeEvent dpy keyCode in f True >> f False


onOff :: Bool -> String
onOff = "On" <||> "Off"
