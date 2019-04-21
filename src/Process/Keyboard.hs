-- Author: Viacheslav Lotsmanov
-- License: GPLv3 https://raw.githubusercontent.com/unclechu/xlib-keys-hack/master/LICENSE

{-# LANGUAGE NoMonomorphismRestriction, ImpredicativeTypes #-}
{-# LANGUAGE ScopedTypeVariables, TupleSections, RecordWildCards #-}

module Process.Keyboard
  ( HandledKey
  , handleKeyEvent
  , getNextKeyboardDeviceKeyEvent

  , SoftwareDebouncer
  , getSoftwareDebouncer
  , getSoftwareDebouncerTiming
  , moveKeyThroughSoftwareDebouncer
  , handleNextSoftwareDebouncerEvent
  ) where

import "base" System.IO (IOMode (WriteMode), withFile)
import "base" GHC.IO.Handle (type Handle)
import qualified "process" System.Process as P
import qualified "linux-evdev" System.Linux.Input.Event as EvdevEvent

import "type-operators" Control.Type.Operator (type ($))
import "transformers" Control.Monad.Trans.Class (lift)
import "base" Control.Monad ((>=>), when, unless, forM_)
import "base" Control.Concurrent.MVar (modifyMVar_)
import "transformers" Control.Monad.Trans.State (execStateT)
import "transformers" Control.Monad.Trans.Except (ExceptT, runExceptT, throwE)
import "base" Control.Concurrent (forkIO, threadDelay)
import qualified "stm" Control.Monad.STM as STM
import qualified "stm" Control.Concurrent.STM.TVar as STM
import qualified "stm" Control.Concurrent.STM.TChan as STM

import "lens" Control.Lens ( (.~), (%~), (^.), (&~), (.=), (%=)
                           , set, mapped, view, _1, _2, _3
                           , Lens'
                           )

import "base" Data.Word (Word8)
import "base" Data.Maybe (fromMaybe, fromJust, isJust, isNothing)
import qualified "containers" Data.Set as Set
import "containers" Data.Set (type Set, (\\))
import "base" Data.Function (fix)
import "time" Data.Time.Clock.POSIX (POSIXTime, getPOSIXTime)
import "qm-interpolated-string" Text.InterpolatedString.QM (qm, qms, qns)

import "X11" Graphics.X11.Types (type KeyCode)

-- local imports

import           Utils.StateMonad (ExceptStateT)
import           Utils.Sugar ((&), (?), (<&>), (<$.), (.>), preserve')
import           Utils.Lens ((%=<&~>))
import           Options (type Options)
import qualified Options as O
import qualified Actions
import           State (type State, CrossThreadVars)
import qualified State
import           Keys (type KeyMap, type KeyName)
import qualified Keys

import qualified Process.CrossThread as CrossThread
               ( handleCapsLockModeChange
               , handleAlternativeModeChange
               , handleResetKbdLayout

               , toggleCapsLock
               , toggleAlternative

               , resetAll
               )


-- | Waiting for next key press/release event
--   from raw keyboard device file descriptor.
--
-- You're supposed to run this forever again and again in own thread, like that:
--
-- @
-- let keyEventHandler = handleKeyEvent ctVars opts keyMap
--
-- forkIO $ forever $
--   getNextKeyboardDeviceKeyEvent keyMap deviceFd >>= keyEventHandler
-- @
getNextKeyboardDeviceKeyEvent :: KeyMap -> Handle -> IO HandledKey
getNextKeyboardDeviceKeyEvent keyMap fd = fix $ \again ->
  EvdevEvent.hReadEvent fd >>= \case
    Just EvdevEvent.KeyEvent
           { EvdevEvent.evKeyCode =
               k@(Keys.getAliasByKey keyMap -> Just (!name, _, !code))
           , EvdevEvent.evKeyEventType =
               (isKeyPressed -> Just !isPressed)
           }
             -> let !ev = HandledKey (OrderedKey k) name code isPressed
                 in pure ev
    _ -> again


-- | Key event handling flow.
--
-- Moving key event through all features heuristics, logging it and finally
-- adding fake key event triggering to the queue (see "Actions" for details).
--
-- You're supposed to run this forever again in again in own thread, like that:
--
-- @
-- let keyEventHandler = handleKeyEvent ctVars opts keyMap
--
-- forkIO $ forever $
--   getNextKeyboardDeviceKeyEvent keyMap deviceFd >>= keyEventHandler
-- @
--
-- TODO Better logging by mentioning exact device file descriptor an event came
--      from or by a thread name marker which already has it
--      (in this case we don't have to pass device file descriptor here
--      but just a string).
handleKeyEvent :: CrossThreadVars -> Options -> KeyMap -> HandledKey -> IO ()
handleKeyEvent ctVars opts keyMap =
  onEv $ fix $ \again time keyName keyCode isPressed state ->

  let reprocess :: State -> IO State
      reprocess = again time keyName keyCode isPressed

      pressed, otherPressed :: Set KeyName
      -- All pressed keys at this time.
      -- Curretly pressed or released key (`keyName`) automatically
      -- added to (or removed from) this Set at the same time.
      pressed = State.pressedKeys state
      -- All pressed keys at this time excluding currently pressed key
      otherPressed = Set.delete keyName pressed

      -- Alternative version of currently pressed or released key
      alternative :: Maybe (KeyName, KeyCode)
      alternative = getAlternative keyName

      onOnlyBothAltsPressed :: Bool
      onOnlyBothAltsPressed =
        O.toggleAlternativeModeByAlts opts &&
        let altsSet = Set.fromList [Keys.AltLeftKey, Keys.AltRightKey]
         in keyName `Set.member` altsSet && pressed == altsSet

      -- When Alternative mode is on and current key has alternative map
      onAlternativeKey :: Bool
      onAlternativeKey = State.alternative state && isJust alternative

      onAppleMediaPressed :: Bool
      onAppleMediaPressed = Keys.FNKey `Set.member` pressed && isMedia keyName

      onOnlyTwoControlsPressed :: Bool
      onOnlyTwoControlsPressed =
        let ctrls = Set.fromList [Keys.ControlLeftKey, Keys.ControlRightKey]
            additionalControls = Set.fromList [Keys.CapsLockKey, Keys.EnterKey]
         in pressed == ctrls
         || (O.additionalControls opts && pressed == additionalControls)

      -- Caps Lock or Enter pressed (current key)
      -- but not Enter with modifiers.
      onAdditionalControlKey :: Bool
      onAdditionalControlKey =
        O.additionalControls opts &&
        keyName `elem` [Keys.CapsLockKey, Keys.EnterKey] &&
        not (
          keyName == Keys.EnterKey &&
          isJust (state ^. State.comboState' . State.isEnterPressedWithMods')
        )

      -- Caps Lock or Enter pressed (previously pressed)
      -- but ignoring just Enter with modifiers.
      onWithAdditionalControlKey :: Bool
      onWithAdditionalControlKey =
        O.additionalControls opts &&
        any (`Set.member` pressed) [Keys.CapsLockKey, Keys.EnterKey] &&
        not (
          Keys.EnterKey `Set.member` pressed &&
          Keys.CapsLockKey `Set.notMember` pressed &&
          isJust (state ^. State.comboState' . State.isEnterPressedWithMods')
        )

      -- Only for additional controls.
      -- Enter key just pressed after some modifiers (only) was pressed before.
      -- Or Enter key just released after pressed with some modes keys.
      onEnterOnlyWithMods :: Bool
      onEnterOnlyWithMods =
        O.additionalControls opts &&
        keyName == Keys.EnterKey &&

        let -- When Enter key just pressed
            -- after some modifiers pressed before.
            pressedCase =
              isPressed &&
              not (Set.null otherPressed) && -- Have some keys pressed
                                             -- along with Enter key.
              -- Enter key was pressed with modifiers only,
              -- not any other key was pressed before.
              Set.null (Set.foldr Set.delete otherPressed allModifiersKeys)

            -- When Enter key is just released
            -- and before it was pressed only with modifiers.
            releasedCase =
              not isPressed &&
              isJust
                (state ^. State.comboState' . State.isEnterPressedWithMods')

         in pressedCase || releasedCase

      -- When Enter pressed with only modifiers before and not released yet
      onEnterWithModsOnlyInProgress :: Bool
      onEnterWithModsOnlyInProgress =

        let lens = State.comboState' . State.isEnterPressedWithMods'
            mods = state ^. lens

         in O.additionalControls opts &&
            isJust mods &&
            keyName /= Keys.EnterKey &&

            -- Preventing infinite loop, it's already stored in state,
            -- so we're just going to handle it recursively again.
            not (isPressed && keyName `Set.member` fromJust mods)

      intervalLimit :: Rational
      intervalLimit = 0.5

      -- Super-Double-Press feature.
      -- 1st step: first press of Super key.
      onSuperDoubleFirstPress :: Bool
      onSuperDoubleFirstPress =
        O.superDoublePress opts &&
        not (state ^. State.comboState' . State.superDoublePressProceeded') &&
        maybeAsName keyName `elem` [Keys.SuperLeftKey, Keys.SuperRightKey] &&
        isPressed && pressed == Set.singleton keyName &&
        isNothing (state ^. State.comboState' . State.superDoublePress')

      -- Super-Double-Press feature.
      -- 2nd step: first release of Super key.
      onSuperDoubleFirstRelease :: Bool
      onSuperDoubleFirstRelease =
        O.superDoublePress opts &&
        not (state ^. State.comboState' . State.superDoublePressProceeded') &&

        Just True ==
          state ^. State.comboState' . State.superDoublePress' <&> \x ->
            toRational time < toRational (x ^. _3) + intervalLimit &&
            x ^. _2 == State.WaitForFirstRelease &&
            keyName == x ^. _1 && not isPressed && Set.null pressed

      -- Super-Double-Press feature.
      -- 3rd step: second press of Super key.
      onSuperDoubleSecondPress :: Bool
      onSuperDoubleSecondPress =
        O.superDoublePress opts &&
        not (state ^. State.comboState' . State.superDoublePressProceeded') &&

        Just True ==
          state ^. State.comboState' . State.superDoublePress' <&> \x ->
            toRational time < toRational (x ^. _3) + intervalLimit &&
            x ^. _2 == State.WaitForSecondPressAgain &&
            keyName == x ^. _1 && isPressed && pressed == Set.singleton keyName

      -- Super-Double-Press feature.
      -- 4th step: second release of Super key.
      onSuperDoubleSecondRelease :: Bool
      onSuperDoubleSecondRelease =
        O.superDoublePress opts &&
        not (state ^. State.comboState' . State.superDoublePressProceeded') &&

        Just True ==
          state ^. State.comboState' . State.superDoublePress' <&> \x ->
            toRational time < toRational (x ^. _3) + intervalLimit &&
            x ^. _2 == State.WaitForSecondReleaseOrPressAlternativeKey &&
            keyName == x ^. _1 && not isPressed && Set.null pressed

      -- Super-Double-Press feature.
      -- 4th step: pressed some alternative key while Super key still pressed.
      --
      -- P.S. If alternative mode it is already turned on we pass this as it
      -- isn't to make behavior consistent for user-experience, for example:
      -- user could accidetly press and release Super key twice but last time
      -- accidently release it before pressing alternative mode, in this case
      -- alternative mode will be just turned on, so then user tries again do
      -- the same but right this time and he wan't be able to do this if we do
      -- not pass this (it would be alternative key with actual Super modifier
      -- and after releasing alternative mode won't be turned off).
      onSuperDoubleWithAlternativePressed :: Bool
      onSuperDoubleWithAlternativePressed =
        O.superDoublePress opts &&
        not (state ^. State.comboState' . State.superDoublePressProceeded') &&

        -- see P.S. section which explains why this is commented
        -- not (State.alternative state) &&

        Just True ==
          state ^. State.comboState' . State.superDoublePress' <&> \x ->
            x ^. _2 == State.WaitForSecondReleaseOrPressAlternativeKey &&
            isAlternative keyName && isPressed &&

            let superAndAlternative = [x ^. _1, keyName]
                onlyModifiers = foldr Set.delete pressed superAndAlternative
                mappedAs = Set.map maybeAsName onlyModifiers

             in Set.fromList superAndAlternative `Set.isSubsetOf` pressed &&
                Set.null (mappedAs \\ onlyRealModifiers)

      -- Super-Double-Press feature.
      -- 5th step: held Super key is released after
      -- some alternative keys had pressed.
      onSuperDoubleReleasedAfterAlternative :: Bool
      onSuperDoubleReleasedAfterAlternative =
        O.superDoublePress opts &&
        not (state ^. State.comboState' . State.superDoublePressProceeded') &&

        Just True ==
          state ^. State.comboState' . State.superDoublePress' <&> \x ->
            x ^. _2 == State.WaitForSecondReleaseAfterAlternativeKeys &&
            keyName == x ^. _1 && not isPressed

      onSuperDoubleElse :: Bool
      onSuperDoubleElse =
        O.superDoublePress opts &&
        not (state ^. State.comboState' . State.superDoublePressProceeded') &&
        isJust (state ^. State.comboState' . State.superDoublePress') &&

        Just False == -- Letting modifiers to be passed.
          state ^. State.comboState' . State.superDoublePress' <&> \x ->
            -- Some modifier is pressed.
            (x ^. _2 == State.WaitForSecondReleaseOrPressAlternativeKey &&
            isPressed && maybeAsName keyName `Set.member` onlyRealModifiers) ||
            -- Alternative mode on while holding Super key.
            x ^. _2 == State.WaitForSecondReleaseAfterAlternativeKeys

      justTrigger, justAsTrigger, smartTrigger, alternativeTrigger :: IO ()
      justTrigger   = trigger   keyName keyCode isPressed
      justAsTrigger = asTrigger keyName keyCode isPressed
      smartTrigger  = onAlternativeKey ? alternativeTrigger $ justTrigger

      alternativeTrigger = do

        let Just (keyNameTo, keyCodeTo) = alternative

        noise [qms| Triggering {isPressed ? "pressing" $ "releasing"}
                    of alternative {keyNameTo}
                    (X key code: {keyCodeTo}) by {keyName}... |]

        (isPressed ? pressKey $ releaseKey) keyCodeTo

      off :: KeyName -> IO ()
      off keyNameToOff =
        when (keyNameToOff `Set.member` pressed) $
          trigger keyNameToOff (fromJust $ getKeyCodeByName keyNameToOff) False

  in
  if

  | onSuperDoubleFirstPress -> do

    noise [qms| {maybeAsKeyStr keyName} pressed first time,
                storing this in state for double press of Super key feature to
                wait for first release of this key... |]

    reprocess $ state &~ do

      State.comboState' . State.superDoublePress' .=
        Just (keyName, State.WaitForFirstRelease, time)

      State.comboState' . State.superDoublePressProceeded' .= True

  | onSuperDoubleFirstRelease -> do

    noise [qms| {maybeAsKeyStr keyName} released first time,
                storing this in state for double press of Super key feature to
                wait for second press of this key... |]

    reprocess $ state &~ do

      State.comboState' . State.superDoublePress' %=<&~> do
        _2 .= State.WaitForSecondPressAgain
        _3 .= time

      State.comboState' . State.superDoublePressProceeded' .= True

  | onSuperDoubleSecondPress -> do

    noise [qms| {maybeAsKeyStr keyName} pressed second time,
                storing this in state for double press of Super key feature to
                wait for second release of this key
                or for alternative key press... |]

    reprocess $ state &~ do

      State.comboState' . State.superDoublePress' %=<&~> do
        _2 .= State.WaitForSecondReleaseOrPressAlternativeKey
        _3 .= time

      State.comboState' . State.superDoublePressProceeded' .= True

  | onSuperDoubleSecondRelease -> do

    let cmd = case maybeAsName keyName of
                   Keys.SuperLeftKey  -> O.leftSuperDoublePressCmd  opts
                   Keys.SuperRightKey -> O.rightSuperDoublePressCmd opts
                   _ -> error "unexpected value"

        actionMsg :: String
        actionMsg = isNothing cmd

                  ? [qms| toggling alternative mode (turning it
                          {State.alternative state ? "off" $ "on"}) |]

                  $ [qm| spawning shell command "{fromJust cmd}" |]

        newState = state &~ do
          when (isNothing cmd) $ State.alternative' %= not
          State.comboState' . State.superDoublePress'          .= Nothing
          State.comboState' . State.superDoublePressProceeded' .= True

    noise [qms| {maybeAsKeyStr keyName} released second time in context of
                double press of Super key feature, {actionMsg}... |]

    let spawnCmd :: Maybe (IO ())
        spawnCmd = _process <$> cmd

        _process :: String -> IO ()
        _process shellCmd = fmap (const ()) $ forkIO $
          fmap (\(_, _, _, !_) -> ()) $
            withFile "/dev/null" WriteMode $ \hNull ->
              P.createProcess (P.shell shellCmd)
                { P.std_in  = P.NoStream
                , P.std_out = P.UseHandle hNull
                , P.std_err = O.verboseMode opts ? P.Inherit $ P.UseHandle hNull
                }

    flip fromMaybe spawnCmd $
      notify $ Actions.XmobarAlternativeFlag $ State.alternative newState

    reprocess newState

  | onSuperDoubleWithAlternativePressed -> do

    let superKey = view _1 $ fromJust $
          state ^. State.comboState' . State.superDoublePress'

        superKeyCode = fromJust $ getKeyCodeByName superKey
        alternativeKey = fromJust $ fmap fst $ alternative

    noise [qms| Pressed alternative {keyName} as {alternativeKey}
                while {maybeAsKeyStr superKey} is pressed
                in context of double press of Super key feature,
                triggering {maybeAsKeyStr superKey} off and enabling alternative
                mode on until {maybeAsKeyStr superKey} will be released... |]

    maybeAsTrigger (maybeAsName superKey) superKeyCode False
    notify $ Actions.XmobarAlternativeFlag True

    reprocess $ state &~ do

      State.comboState' . State.superDoublePress' %=<&~> do
        _2 .= State.WaitForSecondReleaseAfterAlternativeKeys

      State.comboState' . State.superDoublePressProceeded' .= True
      State.alternative' .= True

  | onSuperDoubleReleasedAfterAlternative -> do

    noise [qms| {maybeAsKeyStr keyName} released after some alternative keys
                had triggered in context of double press of Super key feature,
                triggering off events for unreleased keys: {Set.toList pressed},
                turning alternative mode off and
                resetting state of this feature... |]

    alternativeMaybeAsTrigger' (Set.toList pressed) False
    notify $ Actions.XmobarAlternativeFlag False

    return $ state &~ do
      State.comboState' . State.superDoublePressProceeded' .= True
      State.comboState' . State.superDoublePress'          .= Nothing
      State.pressedKeys'                                   .= Set.empty
      State.alternative'                                   .= False

  | onSuperDoubleElse -> do

    noise [qns| Double press of Super key feature did not match
                required conditions, resetting state of it
                (a reason could be one of these:
                  1. different key is pressed;
                  2. interval limit is exceeded)... |]

    reprocess $ state &~ do
      State.comboState' . State.superDoublePress'          .= Nothing
      State.comboState' . State.superDoublePressProceeded' .= True

  | onEnterWithModsOnlyInProgress ->

    let lens = State.comboState' . State.isEnterPressedWithMods'
        Just pressedModifiers = state ^. lens

     in if -- When Enter had been pressed only with modifiers
           -- and one of the modifiers released erlier than Enter.
           | not isPressed && keyName `Set.member` pressedModifiers -> do

             noise' [ [qms| In sequence of 'modifier(s) + Enter' combo
                            modifier ({keyName}) was released before Enter,
                            we're about to take it as 'modifier(s) + Enter'
                            ({Set.toList pressedModifiers} + {Keys.EnterKey}) |]

                    , [qms| Triggering pressing and releasing {Keys.EnterKey}
                            right now and
                            recursively calling handler again to release
                            modifier ({keyName})... |]
                    ]

             -- Triggering Enter pressing and releasing first
             let enterKeyCode = fromJust $ getKeyCodeByName Keys.EnterKey

                 -- Handle it recursively again (without flag in state)
                 -- to press modifier befure triggering Enter.
              in pressRelease Keys.EnterKey enterKeyCode

             -- Triggering releasing of modifier
             -- and flush modifiers for enter list in state.
             state
               & lens .~ Nothing
               & State.pressedKeys' %~ (Set.delete Keys.EnterKey)
               & reprocess

           -- Another modifier pressed, adding it to stored list
           | isPressed && keyName `Set.member` allModifiersKeys -> do

             noise' [ [qms| In sequence of 'modifiers + Enter' combo
                            another modifier ({keyName}) was pressed,
                            adding this modifier to state... |]

                    , [qms| Calling handler recursively again
                            to trigger modifier ({keyName}) key... |]
                    ]

             -- Let's trigger this modifier key by recursively handle it again
             state & lens . mapped %~ Set.insert keyName & reprocess

           -- Another key event, it's not 'Enter with modifiers only' anymore,
           -- it should be handled as additional control in this case.
           | otherwise -> do

             noise' [ [qms| In sequence of 'modifier(s) + Enter' combo
                            ({Set.toList pressedModifiers} + {Keys.EnterKey})
                            some another key was detected
                            ({keyName} was {isPressed ? "pressed" $ "released"}),
                            so it means it's not that kind of combo anymore,
                            Enter key will be interpreted as additional control
                            (it could be Ctrl+Shift+C for example) |]

                    , [qns| Removing 'modifier(s) + Enter' flag from state
                            and calling handler recursively again
                            to handle Enter as additional control... |]
                    ]

             state & lens .~ Nothing & reprocess

  -- Alternative mode on/off by Alts handling
  | onOnlyBothAltsPressed -> do

    noise "Two alts pressed, it means Alternative mode toggling"
    let toDelete = [Keys.AltLeftKey, Keys.AltRightKey]
    forM_ toDelete off
    state
      & State.pressedKeys' .~ foldr Set.delete pressed toDelete
      & toggleAlternative

  -- Hadling `FNKey` pressing on apple keyboard
  | keyName == Keys.FNKey ->

    if

    -- Prevent triggering when just pressed
    | isPressed -> return state

    -- When releasing `FNKey` after some media keys pressed
    | state ^. State.comboState' . State.appleMediaPressed' -> do
      restPressed <- releaseAppleMedia $ State.pressedKeys state
      return $ state
        & State.comboState' . State.appleMediaPressed' .~ False
        & State.pressedKeys' .~ restPressed

    -- As `InsertKey` (because no media pressed)
    | otherwise -> do
      asPressRelease keyName keyCode
      return state

  -- When held `FNKey` on apple keyboard and press some media key
  | onAppleMediaPressed -> do
    noise [qms| Apple media key pressed, preventing triggering
                {Keys.FNKey} as {Keys.InsertKey}... |]
    smartTrigger
    return $ state & State.comboState' . State.appleMediaPressed' .~ True

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
  | onEnterOnlyWithMods ->

    let lens      = State.comboState' . State.isEnterPressedWithMods'
        Just mods = state ^. lens

     in if isPressed

           -- On Enter key pressed.
           -- Storing pressed modifiers in state.
           then set lens (Just otherPressed) state
                <$ noise [qms| {keyName} pressed only with modifiers for now,
                               storing these modifiers list in state... |]

           -- On Enter key released.
           -- Triggering Enter key pressing+releasing
           -- (with triggered modifiers before),
           -- so it means we're triggering modifier + Enter combo
           -- (like Shift+Enter, Ctrl+Enter, etc.).
           else do noise [qms| {keyName} released and had pressed before
                               only with modifiers, triggering it as
                               {Set.toList mods} + {keyName}... |]
                   set lens Nothing state <$ pressRelease keyName keyCode

  -- Handling of additional controls by `CapsLockKey` and `EnterKey`.
  -- They can't be pressed both in the same time here, it handled above.
  | onAdditionalControlKey ->

    let ( withCombosFlagLens,
          pressedBeforeLens,
          controlKeyName,
          Just controlKeyCode ) =

          case keyName of
               Keys.CapsLockKey ->
                 ( State.comboState' . State.isCapsLockUsedWithCombos'
                 , State.comboState' . State.keysPressedBeforeCapsLock'
                 , Keys.ControlLeftKey
                 , getKeyCodeByName Keys.ControlLeftKey
                 )
               Keys.EnterKey ->
                 ( State.comboState' . State.isEnterUsedWithCombos'
                 , State.comboState' . State.keysPressedBeforeEnter'
                 , Keys.ControlRightKey
                 , getRealKeyCodeByName Keys.RealControlRightKey
                 )
               _ -> error [qms| Got unexpected key, it supposed to be only
                                {Keys.CapsLockKey} or {Keys.EnterKey} |]

          :: ( Lens' State Bool,
               Lens' State (Set KeyName),
               KeyName,
               Maybe KeyCode )
     in if

        -- Prevent triggering when just pressed.
        -- But store keys that hadn't released in time.
        | isPressed -> do
          unless (Set.null otherPressed) $
            noise' [ [qms| {keyName} was pressed with some another keys
                           that hadn't be released in time, these another keys
                           WONT be taken as combo with additional control |]
                   , [qms| Storing keys was pressed before {keyName}:
                           {Set.toList otherPressed}... |]
                   ]
          return $ state & pressedBeforeLens .~ otherPressed

        -- Trigger Control releasing because when you press
        -- `CapsLockKey` or `EnterKey` with combo (see below)
        -- it triggers Control pressing.
        | state ^. withCombosFlagLens -> do
          noise' [ [qms| {controlKeyCode} released after pressed with combos,
                         it means it was interpreted as {controlKeyName} |]
                 , [qms| Triggering releasing of {controlKeyName}
                         (X key code: {controlKeyCode})... |]
                 ]
          releaseKey controlKeyCode
          return $ state & withCombosFlagLens .~ False

        -- Just triggering default aliased key code
        -- to `CapsLockKey` or `EnterKey`.
        | otherwise ->
          case keyName of
               Keys.CapsLockKey -> do

                 ( if O.realCapsLock opts
                      then pressRelease
                      else asPressRelease ) keyName keyCode

                 if O.resetByEscapeOnCapsLock opts
                    then execStateT (runExceptT resetAll) state
                    else return state

               Keys.EnterKey -> state <$ pressRelease keyName keyCode
               _ -> return state

  -- When either `CapsLockKey` or `EnterKey` pressed with combo.
  -- They couldn't be pressed both, it handled above.
  | onWithAdditionalControlKey ->

    let _f :: ( KeyName,
                Lens' State Bool,
                Lens' State (Set KeyName),
                KeyName, Maybe KeyCode )

        _f | Keys.CapsLockKey `Set.member` pressed =
             ( Keys.CapsLockKey
             , State.comboState' . State.isCapsLockUsedWithCombos'
             , State.comboState' . State.keysPressedBeforeCapsLock'
             , Keys.ControlLeftKey
             , getKeyCodeByName Keys.ControlLeftKey
             )
           | Keys.EnterKey `Set.member` pressed =
             ( Keys.EnterKey
             , State.comboState' . State.isEnterUsedWithCombos'
             , State.comboState' . State.keysPressedBeforeEnter'
             , Keys.ControlRightKey
             , getRealKeyCodeByName Keys.RealControlRightKey
             )
           | otherwise =
             error [qms| Got unexpected key, it supposed to be only
                         {Keys.CapsLockKey} or {Keys.EnterKey} |]

        ( mainKeyName,
          withCombosFlagLens,
          pressedBeforeLens,
          controlKeyName,
          Just controlKeyCode ) = _f

        pressedBeforeList = state ^. pressedBeforeLens

     in if

        -- Some key is released and this key was pressed before
        -- additional control, so we just remove this key from
        -- list of keys that pressed before additional control.
        | not isPressed && (keyName `Set.member` pressedBeforeList) ->
          (state & pressedBeforeLens %~ Set.delete keyName) <$ smartTrigger

        -- When pressing of Control already triggered
        | state ^. withCombosFlagLens -> state <$ smartTrigger

        -- `CapsLockKey` or `EnterKey` pressed with combo,
        -- it means it should be interpreted as Control key.
        | otherwise -> do
          noise [qms| {mainKeyName} pressed with combo,
                      triggering {controlKeyName}
                      (X key code: {controlKeyCode})... |]
          pressKey controlKeyCode -- Press Control before current key
          smartTrigger
          return $ state & withCombosFlagLens .~ True

  -- When Caps Lock remapped as Escape key.
  -- Resetting stuff (if it's enabled)
  -- and specific logging (noticing about remapping).
  | keyName == Keys.CapsLockKey && not (O.realCapsLock opts) ->
    if O.resetByEscapeOnCapsLock opts && not isPressed
       then justAsTrigger >> execStateT (runExceptT resetAll) state
       else state <$ justAsTrigger

  -- Usual key handling
  | otherwise -> state <$ smartTrigger

  where

  noise   = Actions.noise         opts ctVars ::  String  -> IO ()
  noise'  = Actions.noise'        opts ctVars :: [String] -> IO ()
  notify  = Actions.notifyXmobar  opts ctVars ::  Actions.XmobarFlag  -> IO ()
  notify' = Actions.notifyXmobar' opts ctVars :: [Actions.XmobarFlag] -> IO ()

  pressKey        = Actions.pressKey        ctVars ::  KeyCode  -> IO ()
  pressKeys       = Actions.pressKeys       ctVars :: [KeyCode] -> IO ()
  releaseKey      = Actions.releaseKey      ctVars ::  KeyCode  -> IO ()
  releaseKeys     = Actions.releaseKeys     ctVars :: [KeyCode] -> IO ()
  pressReleaseKey = Actions.pressReleaseKey ctVars ::  KeyCode  -> IO ()

  getKeyCodeByName :: KeyName -> Maybe KeyCode
  getKeyCodeByName = Keys.getKeyCodeByName keyMap

  getRealKeyCodeByName :: KeyName -> Maybe KeyCode
  getRealKeyCodeByName = Keys.getRealKeyCodeByName keyMap

  isAlternative = Keys.isAlternative keyMap :: KeyName -> Bool
  getAlternative :: KeyName -> Maybe (KeyName, KeyCode)
  getAlternative = Keys.getAlternative keyMap

  isMedia  = Keys.isMedia keyMap  :: KeyName -> Bool
  getMedia = Keys.getMedia keyMap :: KeyName -> Maybe KeyCode

  getAsName    = Keys.getAsName    keyMap :: KeyName -> KeyName
  maybeAsName  = Keys.maybeAsName  keyMap :: KeyName -> KeyName
  getExtraKeys = Keys.getExtraKeys keyMap :: KeyName -> Set KeyName

  toggleCapsLock :: State -> IO State
  toggleCapsLock = CrossThread.toggleCapsLock ctVars noise'

  toggleAlternative :: State -> IO State
  toggleAlternative = CrossThread.toggleAlternative noise' notify'

  handleCapsLockModeChange :: State -> IO State
  handleCapsLockModeChange =
    CrossThread.handleCapsLockModeChange ctVars noise'

  handleAlternativeModeChange :: State -> IO State
  handleAlternativeModeChange =
    CrossThread.handleAlternativeModeChange noise' notify'

  handleResetKbdLayout :: State -> IO State
  handleResetKbdLayout = CrossThread.handleResetKbdLayout ctVars noise'

  resetAll :: ExceptStateT State () IO ()
  resetAll = CrossThread.resetAll opts ctVars noise' notify'

  -- Wait and extract event, make preparations and call handler
  onEv
    :: (POSIXTime -> KeyName -> KeyCode -> Bool -> State -> IO State)
    -> HandledKey
    -> IO ()

  onEv m (HandledKey _ name code isPressed) =
    chain (name, isPressed) $ \state -> do
      !time <- getPOSIXTime
      m time name code isPressed state

  -- Composed prepare actions
  chain :: (KeyName, Bool) -> (State -> IO State) -> IO ()
  chain (keyName, isPressed) handleM = do

    -- Log key user pressed (even if it's ignored or replaced)
    noise [qm| {keyName} is {isPressed ? "pressed" $ "released"} |]

    let mapState :: State -> IO State
        mapState = fmap (either id id) . runExceptT . _chain

        _reset :: State -> State
        _reset = State.comboState' . State.superDoublePressProceeded' .~ False

        _chain :: State -> ExceptT State IO State
        _chain = ignoreDuplicates
                   >=> storeKey
                   >=> lift . handleM
                   >=> lift . handleResetKbdLayout
                   >=> lift . handleCapsLockModeChange
                   >=> lift . handleAlternativeModeChange
                   >=> lift . pure . _reset

     in modifyMVar_ (State.stateMVar ctVars) mapState

    where -- Prevent doing anything when key state is the same
          ignoreDuplicates :: State -> ExceptT State IO State
          ignoreDuplicates state =
            let pressed     = State.pressedKeys state
                isMember    = keyName `Set.member` pressed
                isDuplicate = isPressed == isMember
             in (isDuplicate ? throwE $ pure) state

          -- Store key user pressed in state
          storeKey :: State -> ExceptT State IO State
          storeKey state =
            let action = isPressed ? Set.insert $ Set.delete
             in return $ state & State.pressedKeys' %~ action keyName

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
         in releaseKey keyCode
    return rest

  -- Release alternative keys.
  -- Useful when alternative mode turns off not by both alts
  -- and key could be still pressed.
  -- -- It's commented because it's never used anywhere
  -- releaseAlternative :: Set KeyName -> IO (Set KeyName)
  -- releaseAlternative = abstractRelease
  --
  --   "Releasing alternative keys during turning alternative mode off..."
  --
  --   (\keyName -> [qms| Releasing alternative {keyName}
  --                      during turning alternative mode off... |])
  --
  --   isAlternative
  --   (getAlternative .> fmap snd)

  -- Release apple media keys.
  -- Useful when user released `FNKey` erlier than media key.
  releaseAppleMedia :: Set KeyName -> IO (Set KeyName)
  releaseAppleMedia = abstractRelease

    [qms| Releasing held media keys of apple keyboard
          after {Keys.FNKey} released... |]

    (\keyName -> [qms| Releasing held media {keyName} of apple keyboard
                       after {Keys.FNKey} released... |])

    isMedia
    getMedia

  -- Simple triggering key user pressed to X server
  trigger :: KeyName -> KeyCode -> Bool -> IO ()
  trigger keyName keyCode isPressed = do

    noise [qms| Triggering {isPressed ? "pressing" $ "releasing"}
                of {keyName} (X key code: {keyCode})... |]

    (isPressed ? pressKey $ releaseKey) keyCode

  -- Trigger remapped key.
  -- Difference between `trigger` is that this one
  -- shows in log which key this key remapped to.
  asTrigger :: KeyName -> KeyCode -> Bool -> IO ()
  asTrigger keyName keyCode isPressed = do

    noise [qms| Triggering {isPressed ? "pressing" $ "releasing"}
                of {keyName} as {getAsName keyName}
                (X key code: {keyCode})... |]

    (isPressed ? pressKey $ releaseKey) keyCode

  maybeAsTrigger :: KeyName -> KeyCode -> Bool -> IO ()
  maybeAsTrigger keyName =
    let k = maybeAsName keyName
     in k == keyName ? trigger keyName $ asTrigger keyName

  -- Multiple version of `maybeAsTrigger` that supports alternative keys.
  -- Alternative mode supposed to be enabled when calling this monad.
  alternativeMaybeAsTrigger' :: [KeyName] -> Bool -> IO ()
  alternativeMaybeAsTrigger' keyNames isPressed = do

    let keys :: [(KeyName, KeyName, KeyCode)]
        keys = flip map keyNames $ \k ->

            let mappedAlternative = getAlternative k
                nonAlternative = (maybeAsName k, fromJust $ getKeyCodeByName k)
                (asKeyName, code) = fromMaybe nonAlternative mappedAlternative

             in (k, asKeyName, code)

    noise' $ flip map keys $ \(keyName, asKeyName, code) ->
      let alternativeStr = isAlternative keyName ? "alternative " $ ""
       in [qms| Triggering {isPressed ? "pressing" $ "releasing"}
                of {alternativeStr}{asKeyStr keyName asKeyName}
                (X key code: {code})... |]

    (isPressed ? pressKeys $ releaseKeys) $ map (\(_, _, x) -> x) keys

  -- Triggering both press and release events to X server
  pressRelease :: KeyName -> KeyCode -> IO ()
  pressRelease keyName keyCode = do

    noise [qms| Triggering pressing and releasing of {keyName}
                (X key code: {keyCode})... |]

    pressReleaseKey keyCode

  -- Triggering both press and release events to X server.
  -- Also write to log about to which key this key remapped.
  asPressRelease :: KeyName -> KeyCode -> IO ()
  asPressRelease keyName keyCode = do

    noise [qms| Triggering pressing and releasing
                of {keyName} as {getAsName keyName}
                (X key code: {keyCode})... |]

    pressReleaseKey keyCode

  onlyRealModifiers :: Set KeyName
  onlyRealModifiers = Set.fromList
    [ Keys.ControlLeftKey, Keys.ControlRightKey
    , Keys.ShiftLeftKey,   Keys.ShiftRightKey
    , Keys.AltLeftKey,     Keys.AltRightKey
    ]

  -- Union of all modifiers keys
  -- (including keys that remapped as these modifiers).
  allModifiersKeys :: Set KeyName
  allModifiersKeys = mods `Set.union` remappedMods

    where -- Just Set of modifiers keys
          mods :: Set KeyName
          mods = Set.fromList
            [ Keys.ControlLeftKey, Keys.ControlRightKey
            , Keys.SuperLeftKey,   Keys.SuperRightKey
            , Keys.AltLeftKey,     Keys.AltRightKey
            , Keys.ShiftLeftKey,   Keys.ShiftRightKey
            ]

          -- Other keys that was remapped as modifiers keys,
          -- that means they're modifiers too.
          -- For example `LessKey` is remapped to `ShiftLeftKey`,
          -- so it means that this key is modifier too.
          remappedMods :: Set KeyName
          remappedMods = Set.foldr (Set.union . getExtraKeys) Set.empty mods

  maybeAsKeyStr :: KeyName -> String
  maybeAsKeyStr k | k == maybeAsName k = show k
                  | otherwise          = [qm| {k} (as {maybeAsName k}) |]

  asKeyStr :: KeyName -> KeyName -> String
  asKeyStr keyName asKeyName
    | keyName == asKeyName = show keyName
    | otherwise            = [qm| {keyName} (as {asKeyName}) |]


-- | A wrapper around "EvdevEvent.Key" to add "Ord" instance
--   to make it work with containers such as "Set".
newtype OrderedKey = OrderedKey EvdevEvent.Key deriving (Eq, Show)

instance Ord OrderedKey where
  OrderedKey (EvdevEvent.Key a) `compare` OrderedKey (EvdevEvent.Key b) =
    a `compare` b


-- | Parsed key event obtained from raw keyboard device file descriptor.
data HandledKey
   = HandledKey
   ! OrderedKey
   ! KeyName
   ! KeyCode
   ! Bool -- ^ Indicates whether a key is pressed or released


data SoftwareDebouncer
   = SoftwareDebouncer
   { hwPressedKeysVar :: STM.TVar $ Set OrderedKey
       -- ^ Hardware actual pressed keys set (pressed right now)

   , debouncingKeysVar :: STM.TVar $ Set OrderedKey
       -- ^ Set of keys which events temporarily ignored (debounced)

   -- , debouncedChan :: STM.TChan (OrderedKey, Bool, Int, KeyName, KeyCode)
   , debouncedChan :: STM.TChan (HandledKey, Int)
       -- ^ FIFO to handle event of a key again after some amount of time

   , debouncerTiming :: POSIXTime
       -- ^ Gap between first and next events handling.
       --   See "Options" module for details.
   }


-- | Only a getter (for exporting from module).
getSoftwareDebouncerTiming :: SoftwareDebouncer -> POSIXTime
getSoftwareDebouncerTiming = debouncerTiming


-- | Returns "SoftwareDebouncer" and event handler
--   if debouncer option is set.
--
-- You supposed to start software debouncer event handler in own thread,
-- see "handleNextSoftwareDebouncerEvent" for details.
getSoftwareDebouncer :: Options -> IO $ Maybe SoftwareDebouncer
getSoftwareDebouncer = O.debouncerTiming .> go where
  go = maybe (pure Nothing) $ Just <$. getDebouncer

  getDebouncer debouncerTiming'
    = STM.atomically
    $ SoftwareDebouncer <$> STM.newTVar mempty
                        <*> STM.newTVar mempty
                        <*> STM.newTChan
                        <*> pure debouncerTiming'


-- | Move @HandledKey@ through software debouncer logic.
--
-- It checks whether a key is currently debounced so an event is supposed to be
-- ignored. Otherwise it supposed to be added to debounced keys list.
--
-- If software debouncer feature is turned on you supposed to pass all your
-- @HandledKey@ through this function, like this:
--
-- @
-- let keyEventHandler = handleKeyEvent ctVars opts keyMap
--
-- forkIO $ forever $
--   getNextKeyboardDeviceKeyEvent keyMap fd
--     >>= moveKeyThroughSoftwareDebouncer softwareDebouncer
--     >>= pure () `maybe` keyEventHandler
-- @
--
-- TODO Add logging of events handling.
moveKeyThroughSoftwareDebouncer
  :: SoftwareDebouncer
  -> HandledKey
  -> IO $ Maybe HandledKey
  -- ^ @Maybe@ Indicates whether event should be triggered (or it's ignored)

moveKeyThroughSoftwareDebouncer
  SoftwareDebouncer {..}
  ev@(HandledKey key name code isPressed) = go where

  go = getPOSIXTime
    <&> (+ debouncerTiming) .> posixTimeToMicroseconds
    >>= STM.atomically . stmTransaction

  stmTransaction debouncedTime = do
    -- Always storing last received state of a key (pressed or released)
    STM.modifyTVar' hwPressedKeysVar $ (isPressed ? Set.insert $ Set.delete) key

    -- Indicates that current key isn't debounced right now
    -- so we have to trigger first event immidiately
    -- and mark that key as debounced.
    isKeyEventPassed <- Set.notMember key <$> STM.readTVar debouncingKeysVar

    -- Debounce a key if it wasn't debounced
    when isKeyEventPassed $ do
      -- Temporarily ignoring that key from now
      STM.modifyTVar' debouncingKeysVar $ Set.insert key

      -- Debounce/delay next handle
      STM.writeTChan debouncedChan
        (HandledKey key name code isPressed, debouncedTime)

    pure $ preserve' isKeyEventPassed ev


-- | A handler of a debounced event from queue.
--
-- You're supposed to run this forever again and again in own thread, like that:
--
-- @
-- let keyEventHandler = handleKeyEvent ctVars opts keyMap
--
-- forkIO $ forever $
--   handleNextSoftwareDebouncerEvent softwareDebouncer >>=
--     pure () `maybe` keyEventHandler
-- @
--
-- TODO Add logging of events handling.
handleNextSoftwareDebouncerEvent :: SoftwareDebouncer -> IO $ Maybe HandledKey
handleNextSoftwareDebouncerEvent SoftwareDebouncer {..} = do
  -- Getting next event
  (HandledKey key name code isPressed, debouncedTime) <-
    STM.atomically $ STM.readTChan debouncedChan

  currentTime <- getPOSIXTime

  -- Delayed time threshold - current time
  let diffTime = debouncedTime - posixTimeToMicroseconds currentTime

  -- Waiting for a gap before checking next state of a key (debouncing).
  --
  -- In case there was a lag or next event was very close to previous one
  -- @diffTime@ may have negative value so we don't have to wait at all and act
  -- immidiately, that's why we're checking it is greater than zero).
  --
  -- It's okay to use single waiter for many keys here, it won't block anything
  -- since it waits only for @diffTime@ (not for some fixed delay value).
  -- Next key in order is always supposed to be handled later than current one,
  -- so after current one is handled it will immidiately try to handle next one
  -- and if a gap is shorter it will just wait less amount of time.
  when (diffTime > 0) $ threadDelay diffTime

  -- Checking if after a gap state of a key is changed
  -- (e.g. key was pressed but now, after a gap, it's released or vise versa).
  isItInverted <- STM.atomically $ do
    -- Like right now.
    isPhysicallyPressed <- Set.member key <$> STM.readTVar hwPressedKeysVar

    -- Comparing last known state of a key, before a gap,
    -- with current state, after a gap
    -- (any state changes in between were ignored).
    if isPressed == isPhysicallyPressed

       then -- State of a key isn't changed after a gap,
            -- so just forgetting a key ever have been debounced,
            -- so it could be debounced again now.
            False <$ STM.modifyTVar' debouncingKeysVar (Set.delete key)

       else -- A key was pressed but now, after a gap, it's released,
            -- or vise versa, a key was released but now it's pressed.
            -- Leaving it marked as debounced (see @debouncingKeysVar@),
            -- as it was, and adding new delayed/debounced event for new
            -- inverted state of a key.
            let silenceGap =
                  posixTimeToMicroseconds $ currentTime + debouncerTiming

                debouncedEvent =
                  (HandledKey key name code isPhysicallyPressed, silenceGap)

             in True <$ STM.writeTChan debouncedChan debouncedEvent

  -- So since after a gap state of a key is inverted we have to trigger
  -- new key state right now, to trigger fake key event.
  pure $ preserve' isItInverted $ HandledKey key name code $ not isPressed


-- | Checks whether a key in "KeyEventType" is pressed or released.
--
-- Returns @Nothing@ if it's neither pressed nor released event type
-- (it could be holding a key event).
isKeyPressed :: EvdevEvent.KeyEventType -> Maybe Bool
isKeyPressed EvdevEvent.Depressed = Just True
isKeyPressed EvdevEvent.Released  = Just False
isKeyPressed _                    = Nothing


posixTimeToMicroseconds :: Integral a => POSIXTime -> a
posixTimeToMicroseconds = round . (* 10 ^ (6 :: Word8))
