-- Author: Viacheslav Lotsmanov
-- License: GPLv3 https://raw.githubusercontent.com/unclechu/xlib-keys-hack/master/LICENSE

{-# LANGUAGE NoMonomorphismRestriction, ImpredicativeTypes #-}
{-# LANGUAGE ScopedTypeVariables, FlexibleContexts #-}

module Process.Keyboard.HandlingKeyEventFlow
     ( handleKeyEvent
     ) where

import "base" Data.Tuple (swap)
import "base" Data.Function (fix)
import "data-default" Data.Default (def)
import "base" Data.Maybe (fromMaybe, fromJust, isJust, isNothing)
import "containers" Data.Set (type Set, (\\))
import qualified "containers" Data.Set as Set
import "time" Data.Time.Clock.POSIX (type POSIXTime, getPOSIXTime)
import "qm-interpolated-string" Text.InterpolatedString.QM (qm, qms, qns)
import "safe" Safe (succSafe, predSafe)

import "type-operators" Control.Type.Operator (type ($))
import "base" Control.Applicative ((<|>))
import "base" Control.Concurrent (forkIO)
import "base" Control.Concurrent.MVar (modifyMVar_)
import "base" Control.Monad ((>=>), when, unless, forM_, guard)
import "mtl" Control.Monad.State.Class (type MonadState)
import "transformers" Control.Monad.Trans.Class (lift)
import "transformers" Control.Monad.Trans.State (type StateT, execStateT)

import "transformers" Control.Monad.Trans.Except ( type ExceptT
                                                 , runExceptT, throwE
                                                 )

import "lens" Control.Lens ( (.~), (%~), (^.), (&~), (.=), (%=)
                           , set, over, mapped, view, _1, _2, _3
                           , type Lens'
                           )

import "base" System.IO (type IOMode (WriteMode), withFile)
import qualified "process" System.Process as P

import "X11" Graphics.X11.Types (type KeyCode)

-- local imports

import           Utils.Sugar ( (&), (<&>), (?), (|?|)
                             , applyIf, unnoticed, liftAT2
                             )

import           Utils.Lens ((%=<&~>))
import           Options (type Options)
import qualified Options as O
import qualified Actions
import           State (type State, type CrossThreadVars)
import qualified State
import           Keys (type KeyMap, type KeyName, type AlternativeModeKeyAction)
import qualified Keys
import           Process.Keyboard.Types (type HandledKey (HandledKey))

import           Types ( type AlternativeModeState
                       , type AlternativeModeLevel (..)
                       )

import qualified Process.CrossThread as CrossThread
               ( handleCapsLockModeChange
               , handleAlternativeModeChange
               , handleResetKbdLayout

               , toggleCapsLock
               , toggleAlternative
               , notifyAboutAlternative

               , resetAll
               )


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
      -- | All pressed keys at this time.
      --
      -- Curretly pressed or released key (`keyName`) automatically
      -- added to (or removed from) this Set at the same time.
      pressed = State.pressedKeys state
      -- | All pressed keys at this time excluding currently pressed key
      otherPressed = Set.delete keyName pressed

      -- | Alternative version of currently pressed or released key.
      --
      -- @Maybe@ indicates only that there's some alternative remapping for
      -- current pressed or released key. It doesn't indicate whether
      -- alternative mode is turned on or not. It trying to find alternative
      -- remapping for current alternative mode level, if alternative mode is
      -- turned off it will use first level by default.
      alternativeRemap
        :: Maybe $ Either AlternativeModeKeyAction (KeyName, KeyCode)

      alternativeRemap = getAlternativeRemapByName level keyName where
        level = def `maybe` view _1 $ State.alternative state

      -- | See "alternativeRemap" for details
      alternativeKeyAction :: Maybe AlternativeModeKeyAction
      alternativeKeyAction = alternativeRemap >>= either Just (const Nothing)

      -- | See "alternativeRemap" for details
      alternativeKeyRemap :: Maybe (KeyName, KeyCode)
      alternativeKeyRemap = alternativeRemap >>= either (const Nothing) Just

      additionalControls :: Set KeyName
      additionalControls
        = Set.fromList
        $ [Keys.CapsLockKey, Keys.EnterKey]

        & ((Keys.ergoEnterKey :)
            `applyIf` (O.ergonomicMode opts == O.ErgonomicMode))

      enters :: Set KeyName
      enters
        = Set.fromList
        $ [Keys.EnterKey]

        & ((Keys.ergoEnterKey :)
            `applyIf` (O.ergonomicMode opts == O.ErgonomicMode))

      -- | @False@ when alternative mode is turned off
      hasAlternativeKeyInCurrentLevel :: KeyName -> Bool
      hasAlternativeKeyInCurrentLevel keyName'
        = maybe False (flip hasAlternativeKey keyName' . view _1)
        $ State.alternative state

      onOnlyBothAltsPressed :: Bool
      onOnlyBothAltsPressed =
        O.toggleAlternativeModeByAlts opts &&
        let altsSet = Set.fromList [Keys.AltLeftKey, Keys.AltRightKey]
         in keyName `Set.member` altsSet && pressed == altsSet

      -- | @onOnlyBothAltsPressed@ works when @not isPressed@.
      onBothAltsAreHeldForAlternativeMode :: Bool
      onBothAltsAreHeldForAlternativeMode =
        O.toggleAlternativeModeByAlts opts &&
        O.alternativeModeWithAltMod opts &&
        isPressed &&
        case state ^. State.comboState' . State.heldAltForAlternativeMode' of
             Just (State.AltIsHeldForAlternativeMode altKey) ->
               pressed == Set.singleton (
                 case altKey of
                      State.HeldLeftAltForAlternativeMode  -> Keys.AltRightKey
                      State.HeldRightAltForAlternativeMode -> Keys.AltLeftKey
               )
             _ -> False

      onAppleMediaPressed :: Bool
      onAppleMediaPressed = Set.member Keys.FNKey pressed && isMediaKey keyName

      onOnlyTwoControlsPressed :: Bool
      onOnlyTwoControlsPressed =
        pressed == Set.fromList [Keys.ControlLeftKey, Keys.ControlRightKey] ||
        (
          O.additionalControls opts &&
          -- Either @CapsLockKey@+@EnterKey@ or @CapsLockKey@+@ApostropheKey@
          -- (in case ergonomic mode feature is enabled
          -- and @ApostropheKey@ is ergonomic remap/version of @EnterKey@).
          any (pressed ==)
              (Set.map (\x -> Set.fromList [Keys.CapsLockKey, x]) enters)
        )

      -- | Caps Lock or Enter pressed (current key)
      --   but not Enter with modifiers.
      onAdditionalControlKey :: Bool
      onAdditionalControlKey =
        O.additionalControls opts &&
        keyName `Set.member` additionalControls &&
        not (
          keyName `Set.member` enters &&
          isJust (state ^. State.comboState' . State.isEnterPressedWithMods')
        )

      -- | Caps Lock or Enter pressed (previously pressed)
      --   but ignoring just Enter with modifiers.
      onWithAdditionalControlKey :: Bool
      onWithAdditionalControlKey =
        O.additionalControls opts &&
        any (`Set.member` pressed) additionalControls &&
        not (
          any (`Set.member` pressed) enters &&
          Keys.CapsLockKey `Set.notMember` pressed &&
          isJust (state ^. State.comboState' . State.isEnterPressedWithMods')
        )

      -- | Only for additional controls.
      --
      -- Enter key just pressed after some modifiers (only) was pressed before.
      -- Or Enter key just released after pressed with some modes keys.
      onEnterOnlyWithMods :: Bool
      onEnterOnlyWithMods =
        O.additionalControls opts &&
        keyName `Set.member` enters &&

        let -- | When Enter key just pressed
            --   after some modifiers pressed before.
            pressedCase =
              isPressed &&
              not (Set.null otherPressed) && -- Have some keys pressed
                                             -- along with Enter key.
              -- Enter key was pressed with modifiers only,
              -- not any other key was pressed before.
              Set.null (Set.foldr Set.delete otherPressed allModifiersKeys)

            -- | When Enter key is just released
            --   and before it was pressed only with modifiers.
            releasedCase =
              not isPressed &&
              isJust
                (state ^. State.comboState' . State.isEnterPressedWithMods')

         in pressedCase || releasedCase

      -- | When Enter pressed with only modifiers before and not released yet
      onEnterWithModsOnlyInProgress :: Bool
      onEnterWithModsOnlyInProgress =

        let lens = State.comboState' . State.isEnterPressedWithMods'
            mods = state ^. lens

         in O.additionalControls opts &&
            isJust mods &&
            keyName `Set.notMember` enters &&

            -- Preventing infinite loop, it's already stored in state,
            -- so we're just going to handle it recursively again.
            not (isPressed && keyName `Set.member` fromJust mods)

      intervalLimit :: Rational
      intervalLimit = 0.5

      -- | To turn on alternative mode (real Alt key isn't triggered).
      onHoldAltForAlternativeMode :: Bool
      onHoldAltForAlternativeMode =
        O.alternativeModeWithAltMod opts &&
        isNothing (State.alternative state) &&
        isNothing ( state ^. State.comboState'
                           . State.heldAltForAlternativeMode' ) &&
        isPressed && keyName `elem` [Keys.AltLeftKey, Keys.AltRightKey] &&
        Set.size pressed == 1

      -- | To turn off alternative mode and trigger real Alt key.
      onSpaceWithHoldedAltForAlternativeMode :: Bool
      onSpaceWithHoldedAltForAlternativeMode =
        O.alternativeModeWithAltMod opts &&
        ( case state ^. State.comboState' . State.heldAltForAlternativeMode' of
               Just (State.AltIsHeldForAlternativeMode _) -> True
               _                                          -> False ) &&
        isPressed && keyName == Keys.SpaceKey && Set.size pressed == 1

      -- | When releasing Alt key which triggered alternative mode previously.
      --
      -- This preserves alternative mode is turned on.
      onReleaseAltForAlternativeMode :: Bool
      onReleaseAltForAlternativeMode =
        O.alternativeModeWithAltMod opts &&
        case state ^. State.comboState' . State.heldAltForAlternativeMode' of
             Nothing -> False

             Just (State.AltIsHeldForAlternativeMode altKey) ->
               not isPressed && keyName == (
                 case altKey of
                      State.HeldLeftAltForAlternativeMode  -> Keys.AltLeftKey
                      State.HeldRightAltForAlternativeMode -> Keys.AltRightKey
               )

             Just State.AltIsReleasedBeforeAlternativeKey ->
               all (not . hasAlternativeKeyInCurrentLevel) pressed

      -- | Super-Double-Press feature.
      --
      -- 1st step: first press of Super key.
      onSuperDoubleFirstPress :: Bool
      onSuperDoubleFirstPress =
        O.superDoublePress opts &&
        not (state ^. State.comboState' . State.superDoublePressProceeded') &&
        maybeAsName keyName `elem` [Keys.SuperLeftKey, Keys.SuperRightKey] &&
        isPressed && Set.size pressed == 1 &&
        isNothing (state ^. State.comboState' . State.superDoublePress')

      -- | Super-Double-Press feature.
      --
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

      -- | Super-Double-Press feature.
      --
      -- 3rd step: second press of Super key.
      onSuperDoubleSecondPress :: Bool
      onSuperDoubleSecondPress =
        O.superDoublePress opts &&
        not (state ^. State.comboState' . State.superDoublePressProceeded') &&

        Just True ==
          state ^. State.comboState' . State.superDoublePress' <&> \x ->
            toRational time < toRational (x ^. _3) + intervalLimit &&
            x ^. _2 == State.WaitForSecondPressAgain &&
            keyName == x ^. _1 && isPressed && Set.size pressed == 1

      -- | Super-Double-Press feature.
      --
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

      -- | Super-Double-Press feature.
      --
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
        -- isNothing (State.alternative state) &&

        Just True ==
          state ^. State.comboState' . State.superDoublePress' <&> \x ->
            x ^. _2 == State.WaitForSecondReleaseOrPressAlternativeKey &&
            isPressed && (
              let level = def `maybe` view _1 $ State.alternative state
               in hasAlternativeRemap level keyName
            ) && (
              let
                superAndAlternative = [x ^. _1, keyName]
                onlyModifiers = foldr Set.delete pressed superAndAlternative
                mappedAs = Set.map maybeAsName onlyModifiers
              in
                Set.fromList superAndAlternative `Set.isSubsetOf` pressed &&
                Set.null (mappedAs \\ onlyRealModifiers)
            )

      -- | Super-Double-Press feature.
      --
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
            ( -- Some modifier is pressed.
              x ^. _2 == State.WaitForSecondReleaseOrPressAlternativeKey &&
              isPressed && maybeAsName keyName `Set.member` onlyRealModifiers
            ) ||
            -- Alternative mode on while holding Super key.
            x ^. _2 == State.WaitForSecondReleaseAfterAlternativeKeys

      onAlternativeModeKeyAction :: Bool
      onAlternativeModeKeyAction =
        isJust (State.alternative state) && isJust alternativeKeyAction &&
        isPressed && (
          Set.size pressed == 1 || (
            O.superDoublePress opts && Just True == (
              state ^. State.comboState' . State.superDoublePress' <&> \x ->
                x ^. _2 == State.WaitForSecondReleaseAfterAlternativeKeys &&
                Set.size (Set.delete (x ^. _1) pressed) == 1
            )
          )
        )

      onF24asVerticalBarKey :: Bool
      onF24asVerticalBarKey = O.f24asVerticalBar opts && keyName == Keys.F24Key

      onRealEscapeReset :: Bool
      onRealEscapeReset =
        O.resetByRealEscape opts && keyName == Keys.EscapeKey && not isPressed

      triggerCurrentKey, smartlyTriggerCurrentKey :: IO ()

      -- | Key could be remapped. It ignores alternative mode remapping.
      triggerCurrentKey = trigger keyName keyCode isPressed

      -- | Key could be remapped. If alternative mode is on and a key has
      --   alternative mode remap it triggers that alternative version.
      --
      -- Otherwise it triggers regular key
      -- (which in case may be remapped apart from alternative mode mapping).
      smartlyTriggerCurrentKey
        = maybe triggerCurrentKey (alternativeTrigger . view _2)
        $ liftAT2 (State.alternative state, alternativeKeyRemap)

      alternativeTrigger :: (KeyName, KeyCode) -> IO ()
      alternativeTrigger (keyNameTo, keyCodeTo) = do

        noise [qms| Triggering {isPressed ? "pressing" $ "releasing"}
                    of alternative {keyNameTo}
                    (X key code: {keyCodeTo}) by {keyName}... |]

        (pressKey |?| releaseKey) isPressed keyCodeTo

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

        altModeOnLevel = def :: AlternativeModeLevel
        altModeOnIsPermanent = True

        actionMsg :: String
        actionMsg = go where
          go
            = isNothing cmd
            ? [qm| toggling alternative mode (turning it {altModeAction}) |]
            $ [qm| spawning shell command "{fromJust cmd}" |]

          showPermament = "permanently" |?| "temporarily"

          showLevel FirstAlternativeModeLevel  = "1st level"
          showLevel SecondAlternativeModeLevel = "2nd level"

          altModeAction :: String
          altModeAction =
            case State.alternative state of
                 Nothing ->
                   [qms| on {showPermament altModeOnIsPermanent}
                         on {showLevel altModeOnLevel} |]
                 Just (level, isPermanent) ->
                   [qms| off from {showLevel level} which have been
                         turned on {showPermament isPermanent} |]

        newState = state &~ do
          when (isNothing cmd) $
            State.alternative' %= \case
              Nothing -> Just (altModeOnLevel, altModeOnIsPermanent)
              Just _  -> Nothing

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

    let superKeyCode = fromJust $ getKeyCodeByName superKey

    let ( shouldReleaseSuper   :: Bool                             ,
          whatToDoNextAction   :: DoubleSuperWithAlternativeAction ,
          alternativeModeState :: AlternativeModeState             ,
          asKeyMark            :: String                           ,
          actionMsg            :: String                           )
                                                                   =
          case fromJust alternativeRemap of
               Left km@Keys.AlternativeModeFreeze ->
                 let
                   alternativeModeNewState =
                     (State.alternative state <&> set _2 True) <|>
                     Just (def, True)
                 in
                   ( False
                   , AlternativeModeIsPermanentActLikeKeyIsNotPressed
                   , alternativeModeNewState
                   , show km
                   , [qms| turning alternative mode on permanently,
                           without need to hold {maybeAsKeyStr superKey}
                           to keep alternative mode being turned on,
                           leaving {maybeAsKeyStr superKey} being pressed
                           in case a user need to press something with
                           Super key combo and don't want to release and press
                           it again (it's okay since it doesn't clash with any
                           other key in this case, it doesn't lead to unwanted
                           possible combo with Super) |]
                   )

               Left km@Keys.AlternativeModeLevelUp ->
                 let
                   next = NextStepFlowActLikeKeyIsNotPressed

                   level = maybe (succSafe def) (succSafe . view _1)
                         $ State.alternative state

                   alternativeModeNewState = Just (level, False)

                   msgPrefix :: String
                   msgPrefix =
                     [qm| triggering {maybeAsKeyStr superKey} off and |]

                   msgWhenOn =
                     [qms| {msgPrefix} making alternative mode being no longer
                           permanent (since alternative mode have been turned on
                           previously), so alternative mode is kept turned on
                           until {maybeAsKeyStr superKey} will be released,
                           and shifting alternative mode to {level} |]

                   msgWhenOff =
                     [qms| {msgPrefix} enabling alternative mode with {level} on
                           until {maybeAsKeyStr superKey} will be released |]

                   msg = msgWhenOn |?| msgWhenOff
                       $ isJust $ State.alternative state
                 in
                   (True, next, alternativeModeNewState, show km, msg)

               Left km@Keys.AlternativeModeLevelDown ->
                 let
                   -- | When alternative mode is either
                   --   already turned off or is going to be.
                   isLowerEdge =
                     isNothing (State.alternative state) ||
                     (State.alternative state <&> view _1) == Just minBound

                   -- | Should release super key flag.
                   --
                   -- Release if alternative mode is turned on, and we are just
                   -- shifting alternative mode level one step down.
                   --
                   -- No need for release when either alternative mode is off
                   -- (so we don't have to do anything related to alternative
                   -- mode) or alternative mode is on and its level is minimal
                   -- (so we just turning alternative mode off, and user may
                   -- want to press something with Super after that without need
                   -- to release and press it again, it's okay since it doesn't
                   -- clash with any other key in this case, it doesn't
                   -- lead to unwanted possible combo with Super).
                   releaseSuper = not isLowerEdge

                   (next, alternativeModeNewState)
                     = isLowerEdge
                     ? ( NoNextStepActLikeKeyIsNotPressed, Nothing )
                     $ ( NextStepFlowActLikeKeyIsNotPressed
                       , State.alternative state <&> over _1 predSafe
                                                 <&> set  _2 False
                       )

                   msg
                     | isNothing (State.alternative state) =
                         [qms| alternative mode is already turned off,
                               so just kinda doing nothing, leaving
                               {maybeAsKeyStr superKey} being pressed |]

                     | (State.alternative state <&> view _1) == Just minBound =
                         [qms| alternative mode is turned on with minimal level,
                               so turning alternative mode off, leaving
                               {maybeAsKeyStr superKey} being pressed |]

                     | otherwise = -- not isLowerEdge
                         [qms| shifting alternative mode one level down to
                               {maybe def (view _1) alternativeModeNewState},
                               making alternative mode being no longer
                               permanent (since alternative mode have been
                               turned on previously),
                               triggering {maybeAsKeyStr superKey} off |]
                 in
                   (releaseSuper, next, alternativeModeNewState, show km, msg)

               Right (asKeyName, _) ->
                 let
                   next = NextStepFlowAndTriggerAlternative

                   -- | If alternative mode was previously turned on
                   --   then it's no longer permanent.
                   --
                   -- I'm not sure if we should move it always back to
                   -- @FirstAlternativeModeLevel@ or not... Maybe it will change
                   -- in the future, or would be configurable via some option.
                   alternativeModeNewState = x <|> y where
                     x = State.alternative state <&> set _2 False
                     y = Just (def, False)

                   msgPrefix :: String
                   msgPrefix =
                     [qm| triggering {maybeAsKeyStr superKey} off and |]

                   msgWhenOn =
                     [qms| {msgPrefix} making alternative mode being no longer
                           permanent (since alternative mode have been turned on
                           previously), so alternative mode is kept turned on
                           until {maybeAsKeyStr superKey} will be released |]

                   msgWhenOff =
                     [qms| {msgPrefix} enabling alternative mode on
                           until {maybeAsKeyStr superKey} will be released,
                           also triggering alternative {asKeyName} |]

                   msg = msgWhenOn |?| msgWhenOff
                       $ isJust $ State.alternative state
                 in
                   (True, next, alternativeModeNewState, show asKeyName, msg)

    noise [qms| Pressed alternative {keyName} as {asKeyMark}
                while {maybeAsKeyStr superKey} is pressed
                in context of double press of Super key feature,
                {actionMsg}... |]

    when shouldReleaseSuper $ trigger superKey superKeyCode False
    notify $ Actions.XmobarAlternativeFlag alternativeModeState

    let modifier :: MonadState State m => m ()
        modifier = do
          State.comboState' . State.superDoublePressProceeded' .= True
          State.alternative' .= alternativeModeState

          do
            let waitForSecondReleaseAfterAlternativeKeys =
                  State.comboState' . State.superDoublePress' %=<&~> do
                    _2 .= State.WaitForSecondReleaseAfterAlternativeKeys

            let actLikeKeyIsNotPressed = State.pressedKeys' .= otherPressed

            let noNextStep =
                  State.comboState' . State.superDoublePress' .= Nothing

            case whatToDoNextAction of
                 NextStepFlowAndTriggerAlternative ->
                   waitForSecondReleaseAfterAlternativeKeys
                 NextStepFlowActLikeKeyIsNotPressed -> do
                   waitForSecondReleaseAfterAlternativeKeys
                   actLikeKeyIsNotPressed
                 NoNextStepActLikeKeyIsNotPressed ->
                   noNextStep >> actLikeKeyIsNotPressed
                 AlternativeModeIsPermanentActLikeKeyIsNotPressed ->
                   noNextStep >> actLikeKeyIsNotPressed

    let -- | Rerunning processing flow again if need to trigger a key
        f :: State -> IO State
        f = case whatToDoNextAction of
                 NextStepFlowAndTriggerAlternative                -> reprocess
                 NextStepFlowActLikeKeyIsNotPressed               -> pure
                 NoNextStepActLikeKeyIsNotPressed                 -> pure
                 AlternativeModeIsPermanentActLikeKeyIsNotPressed -> pure

    f $ state &~ modifier

  | onSuperDoubleReleasedAfterAlternative -> do

    let !(!toRelease, !mods) = go where
          go = Set.partition (not . filterMods) pressed

          filterMods x =
            x `Set.member` allModifiersKeys ||
            (O.additionalControls opts && x `Set.member` additionalControls)

    let toReleaseLog
          = Set.null toRelease ? (mempty :: String)
          $ [qms| triggering off events for unreleased keys:
                  {Set.toList toRelease},\ |]

    let modsLog
          = Set.null mods ? (mempty :: String)
          $ [qm| \ (some modifiers left being pressed: {Set.toList mods}) |]

    noise [qms| {maybeAsKeyStr keyName} released after some alternative keys
                had triggered in context of double press of Super key feature,
                {toReleaseLog}turning alternative mode off and
                resetting state of this feature{modsLog}... |]

    let !level =
          maybe (error "alternative mode is supposed to be turned on")
                (view _1)
                (State.alternative state)

    alternativeMultipleTrigger (Just level) toRelease False
    notify $ Actions.XmobarAlternativeFlag Nothing

    pure $ state &~ do
      State.comboState' . State.superDoublePressProceeded' .= True
      State.comboState' . State.superDoublePress'          .= Nothing
      State.pressedKeys'                                   .= mods
      State.alternative'                                   .= Nothing

  | onSuperDoubleElse -> do

    noise [qns| Double press of Super key feature did not match
                required conditions, resetting state of it
                (a reason could be one of these:
                  1. different key is pressed;
                  2. interval limit is exceeded)... |]

    reprocess $ state &~ do
      State.comboState' . State.superDoublePress'          .= Nothing
      State.comboState' . State.superDoublePressProceeded' .= True

  | onAlternativeModeKeyAction -> do

    let !(!level, !isPermanent) = fromJust $ State.alternative state

    let logMsg :: AlternativeModeKeyAction -> String -> String
        logMsg km actionLogMsg =
          [qms| Pressed {keyName} which have been interpreted as {km}
                due to activate alternative mode on {level}, {actionLogMsg},
                removing {keyName} from pressed keys set... |]

    case fromJust alternativeKeyAction of
         km@Keys.AlternativeModeFreeze -> do
           noise $ logMsg km
                 $ isPermanent
                 ? [qm| doing nothing (since it's already permanent) |]
                 $ [qm| making it being permanent |]

           (isPermanent ? pure $ unnoticed notifyAboutAlternative) $
             state &~ do
               State.pressedKeys' .= otherPressed -- Without current key

               unless isPermanent $ do
                 State.alternative' %=<&~> _2 .= True -- Permanent
                 State.comboState' . State.heldAltForAlternativeMode' .= Nothing
                 State.comboState' . State.superDoublePress'          .= Nothing
                 State.comboState' . State.superDoublePressProceeded' .= False

         km@Keys.AlternativeModeLevelUp -> do
           let !isLevelMax = level == maxBound
           let shiftFunc = succSafe

           noise $ logMsg km
                 $ not isLevelMax
                 ? [qm| shifting level up to {shiftFunc level} |]
                 $ [qm| doing nothing (since level is maximum) |]

           (isLevelMax ? pure $ unnoticed notifyAboutAlternative) $
             state &~ do
               State.pressedKeys' .= otherPressed -- Without current key
               State.alternative' %=<&~> _1 %= shiftFunc

         km@Keys.AlternativeModeLevelDown -> do
           let !isLevelMin = level == minBound
           let shiftFunc = predSafe

           noise $ logMsg km
                 $ not isLevelMin
                 ? [qms| shifting level down to {shiftFunc level} |]
                 $ [qms| turning alternative mode off
                         (since level is minimum) |]

           unnoticed notifyAboutAlternative $
             state &~ do
               State.pressedKeys' .= otherPressed -- Without current key

               State.alternative' &
                 (isLevelMin ? (.= Nothing) $ (%=<&~> _1 %= shiftFunc))

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
                 -- to press modifier before triggering Enter.
              in triggerPressRelease Keys.EnterKey enterKeyCode

             -- Triggering releasing of modifier
             -- and flush modifiers for enter list in state.
             state
               & lens .~ Nothing
               & State.pressedKeys' %~ flip (foldr Set.delete) enters
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

  | onHoldAltForAlternativeMode -> do

    noise [qms| Pressed {keyName} alone, real Alt won't be triggered,
                turning alternative mode on... |]

    state
      & State.pressedKeys' .~ otherPressed -- Without Alt key
      & State.comboState' . State.heldAltForAlternativeMode' .~
          Just ( State.AltIsHeldForAlternativeMode
               $ keyName == Keys.AltLeftKey
               ? State.HeldLeftAltForAlternativeMode
               $ State.HeldRightAltForAlternativeMode
               )
      & State.alternative' .~ Just (def, False)
      & unnoticed notifyAboutAlternative

  | onSpaceWithHoldedAltForAlternativeMode -> do

    noise [qns| Pressed Alt+Space, Space after Alt,
                turning alternative mode off and triggering real Alt... |]

    let altKeyName =
          case state ^. State.comboState' . State.heldAltForAlternativeMode' of
               Just ( State.AltIsHeldForAlternativeMode
                      State.HeldLeftAltForAlternativeMode ) -> Keys.AltLeftKey
               _                                            -> Keys.AltRightKey

    getKeyCodeByName altKeyName
      & pure () `maybe` \x -> trigger altKeyName x True

    state
      -- Excluding Space and adding Alt
      & State.pressedKeys' .~ Set.insert altKeyName otherPressed
      & State.comboState' . State.heldAltForAlternativeMode' .~ Nothing
      & State.alternative' .~ Nothing
      & unnoticed notifyAboutAlternative

  | onReleaseAltForAlternativeMode ->

    case state ^. State.comboState' . State.heldAltForAlternativeMode' of
         Nothing -> fail "Impossible case, it supposed to be checked earlier"

         Just (State.AltIsHeldForAlternativeMode heldAltKey) -> do
           let altKey =
                 case heldAltKey of
                      State.HeldLeftAltForAlternativeMode  -> Keys.AltLeftKey
                      State.HeldRightAltForAlternativeMode -> Keys.AltRightKey

           if all (not . hasAlternativeKeyInCurrentLevel) pressed
              then do
                   noise [qms| {altKey} is released,
                               turning alternative mode off... |]

                   state
                     & State.comboState' . State.heldAltForAlternativeMode'
                                         .~ Nothing
                     & State.alternative' .~ Nothing
                     & unnoticed notifyAboutAlternative

              else do
                   noise [qms| {altKey} is released but some alternative keys
                               are still pressed, will turn alternative mode off
                               after all those keys being released. |]
                   pure
                     $ state
                     & State.comboState' . State.heldAltForAlternativeMode' .~
                         Just State.AltIsReleasedBeforeAlternativeKey

         Just State.AltIsReleasedBeforeAlternativeKey -> do
           noise [qns| All alternative keys have been released after delayed
                       turning alternative mode off, now turning alternative
                       mode off... |]

           smartlyTriggerCurrentKey

           state
             & State.comboState' . State.heldAltForAlternativeMode' .~ Nothing
             & State.alternative' .~ Nothing
             & unnoticed notifyAboutAlternative

  -- Alternative mode on/off by Alts handling
  | onOnlyBothAltsPressed -> do

    noise [qns| Two Alts are pressed at the same time,
                it means Alternative mode toggling |]

    let toDelete = [Keys.AltLeftKey, Keys.AltRightKey]
    forM_ toDelete off
    state
      & State.pressedKeys' .~ foldr Set.delete pressed toDelete
      & toggleAlternative

  -- See @onOnlyBothAltsPressed@ for details
  | onBothAltsAreHeldForAlternativeMode -> do

    noise [qns| Two Alts are pressed at the same time,
                keeping Alternative mode turned on
                without need to keep holding it... |]

    unnoticed notifyAboutAlternative $ state &~ do
      State.pressedKeys' .= otherPressed
      State.comboState' . State.heldAltForAlternativeMode' .= Nothing
      State.alternative' %=<&~> _2 .= True -- Now it's permanent

  -- Hadling `FNKey` pressing on apple keyboard
  | keyName == Keys.FNKey ->

    if

    -- Prevent triggering when just pressed
    | isPressed -> pure state

    -- When releasing `FNKey` after some media keys pressed
    | state ^. State.comboState' . State.appleMediaPressed' -> do
      restPressed <- releaseAppleMedia $ State.pressedKeys state
      pure $ state
        & State.comboState' . State.appleMediaPressed' .~ False
        & State.pressedKeys' .~ restPressed

    -- As `InsertKey` (because no media pressed)
    | otherwise -> do
      triggerPressRelease keyName keyCode
      pure state

  -- When held `FNKey` on apple keyboard and press some media key
  | onAppleMediaPressed -> do
    noise [qms| Apple media key pressed, preventing triggering
                {Keys.FNKey} as {Keys.InsertKey}... |]
    smartlyTriggerCurrentKey
    pure $ state & State.comboState' . State.appleMediaPressed' .~ True

  | onOnlyTwoControlsPressed -> do

    noise "Two controls pressed, it means Caps Lock mode toggling"

    off Keys.ControlLeftKey
    off Keys.ControlRightKey

    let toDelete = [Keys.ControlLeftKey, Keys.ControlRightKey]
                     ++ if O.additionalControls opts
                           then Set.toList additionalControls
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
                   set lens Nothing state <$ triggerPressRelease keyName keyCode

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
                 , getDefaultKeyCodeByName Keys.ControlLeftKey
                 )
               k | k `Set.member` enters ->
                 ( State.comboState' . State.isEnterUsedWithCombos'
                 , State.comboState' . State.keysPressedBeforeEnter'
                 , Keys.ControlRightKey
                 , getDefaultKeyCodeByName Keys.ControlRightKey
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
          pure $ state & pressedBeforeLens .~ otherPressed

        -- Trigger Control releasing because when you press
        -- `CapsLockKey` or `EnterKey` with combo (see below)
        -- it triggers Control pressing.
        | state ^. withCombosFlagLens -> do
          noise' [ [qms| {keyName} released after pressed with combos,
                         it means it was interpreted as {controlKeyName} |]
                 , [qms| Triggering releasing of {controlKeyName}
                         (X key code: {controlKeyCode})... |]
                 ]
          releaseKey controlKeyCode
          pure $ state & withCombosFlagLens .~ False

        -- Just triggering default aliased key code
        -- to `CapsLockKey` or `EnterKey`.
        | otherwise ->
          case keyName of
               Keys.CapsLockKey -> do
                 triggerPressRelease keyName keyCode
                 if O.resetByEscapeOnCapsLock opts
                    then execStateT (runExceptT resetAll) state
                    else pure state

               k | k `Set.member` enters ->
                 state <$ triggerPressRelease keyName keyCode

               _ -> pure state

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
             , getDefaultKeyCodeByName Keys.ControlLeftKey
             )
           | any (`Set.member` pressed) enters =
             ( Keys.EnterKey
             , State.comboState' . State.isEnterUsedWithCombos'
             , State.comboState' . State.keysPressedBeforeEnter'
             , Keys.ControlRightKey
             , getDefaultKeyCodeByName Keys.ControlRightKey
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
          let newState = state & pressedBeforeLens %~ Set.delete keyName
           in newState <$ smartlyTriggerCurrentKey

        -- When pressing of Control already triggered
        | state ^. withCombosFlagLens -> state <$ smartlyTriggerCurrentKey

        -- `CapsLockKey` or `EnterKey` pressed with combo,
        -- it means it should be interpreted as Control key.
        | otherwise -> do
          noise [qms| {mainKeyName} pressed with combo,
                      triggering {controlKeyName}
                      (X key code: {controlKeyCode})... |]
          pressKey controlKeyCode -- Press Control before current key
          smartlyTriggerCurrentKey
          pure $ state & withCombosFlagLens .~ True

  -- When Caps Lock remapped as Escape key.
  -- Resetting stuff (if it's enabled)
  -- and specific logging (noticing about remapping).
  | keyName == Keys.CapsLockKey && not (O.realCapsLock opts) ->
    if O.resetByEscapeOnCapsLock opts && not isPressed
       then triggerCurrentKey >> execStateT (runExceptT resetAll) state
       else state <$ triggerCurrentKey

  | onF24asVerticalBarKey -> do

    let (a, b) = go where
          go = ab & if isPressed then id else swap
          ab = (Keys.ShiftLeftKey, Keys.BackslashKey)

    noise [qms|
      {keyName} was {isPressed ? "pressed" $ "released"},
      interpreting it as a vertical bar,
      triggering {isPressed ? "pressing" $ "releasing"} of both {a} and {b}...
    |]

    state <$
      let f k = trigger k (fromJust $ getDefaultKeyCodeByName k) isPressed
       in f a >> f b

  | onRealEscapeReset -> do

    triggerCurrentKey
    execStateT (runExceptT resetAll) state

  -- Usual key handling
  | otherwise -> state <$ smartlyTriggerCurrentKey

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

  getDefaultKeyCodeByName :: KeyName -> Maybe KeyCode
  getDefaultKeyCodeByName = Keys.getDefaultKeyCodeByName keyMap

  hasAlternativeRemap :: AlternativeModeLevel -> KeyName -> Bool
  hasAlternativeRemap = Keys.hasAlternativeRemap keyMap

  hasAlternativeKey :: AlternativeModeLevel -> KeyName -> Bool
  hasAlternativeKey = Keys.hasAlternativeKey keyMap

  getAlternativeRemapByName
    :: AlternativeModeLevel
    -> KeyName
    -> Maybe $ Either AlternativeModeKeyAction (KeyName, KeyCode)

  getAlternativeRemapByName = Keys.getAlternativeRemapByName keyMap

  isMediaKey      = Keys.isMediaKey      keyMap :: KeyName -> Bool
  getMediaKeyCode = Keys.getMediaKeyCode keyMap :: KeyName -> Maybe KeyCode

  getRemapByName = Keys.getRemapByName keyMap :: KeyName -> Maybe KeyName
  getExtraKeys   = Keys.getExtraKeys   keyMap :: KeyName -> Set KeyName

  toggleCapsLock :: State -> IO State
  toggleCapsLock = CrossThread.toggleCapsLock ctVars noise'

  toggleAlternative :: State -> IO State
  toggleAlternative = CrossThread.toggleAlternative noise' notify'

  notifyAboutAlternative :: State -> IO ()
  notifyAboutAlternative = CrossThread.notifyAboutAlternative notify'

  handleCapsLockModeChange :: State -> IO State
  handleCapsLockModeChange =
    CrossThread.handleCapsLockModeChange ctVars noise'

  handleAlternativeModeChange :: State -> IO State
  handleAlternativeModeChange =
    CrossThread.handleAlternativeModeChange noise' notify'

  handleResetKbdLayout :: State -> IO State
  handleResetKbdLayout = CrossThread.handleResetKbdLayout ctVars noise'

  resetAll :: ExceptT () (StateT State IO) ()
  resetAll = CrossThread.resetAll opts ctVars noise' notify'

  -- | Either returns key provided key remapped to or original provided key
  maybeAsName :: KeyName -> KeyName
  maybeAsName keyName = fromMaybe keyName $ getRemapByName keyName

  -- | Wait and extract event, make preparations and call handler
  onEv
    :: (POSIXTime -> KeyName -> KeyCode -> Bool -> State -> IO State)
    -> HandledKey
    -> IO ()

  onEv m (HandledKey _ name code isPressed) =
    chain (name, isPressed) $ \state -> do
      !time <- getPOSIXTime
      m time name code isPressed state

  -- | Composed prepare actions
  chain :: (KeyName, Bool) -> (State -> IO State) -> IO ()
  chain (keyName, isPressed) handleM = go where
    go = do
      -- Log key user pressed (even if it's ignored or replaced)
      noise [qm| {keyName} is {isPressed ? "pressed" $ "released"} |]

      modifyMVar_ (State.stateMVar ctVars) mapState

    mapState :: State -> IO State
    mapState = fmap (either id id) . runExceptT . chain' where

      chain' :: State -> ExceptT State IO State
      chain' = ignoreDuplicates
               >=> storeKey
               >=> lift . handleM
               >=> lift . handleResetKbdLayout
               >=> lift . handleCapsLockModeChange
               >=> lift . handleAlternativeModeChange
               >=> lift . pure . reset

    reset :: State -> State
    reset = State.comboState' . State.superDoublePressProceeded' .~ False

    -- | Store key user pressed in state
    storeKey :: State -> ExceptT State IO State
    storeKey state = x where
      x = pure $ state & State.pressedKeys' %~ action keyName
      action = isPressed ? Set.insert $ Set.delete

    -- | Prevent doing anything when key state is the same.
    ignoreDuplicates :: State -> ExceptT State IO State
    ignoreDuplicates state = (isDuplicate ? throwE $ pure) state where
      pressed     = State.pressedKeys state
      isMember    = keyName `Set.member` pressed

      isDuplicate =
        isPressed == isMember &&
        not isItHeldAltForAlternativeModeReleased

      isItHeldAltForAlternativeModeReleased = isJust $ do
        guard $ O.alternativeModeWithAltMod opts

        heldAltState <-
          state ^. State.comboState' . State.heldAltForAlternativeMode'

        heldAltKey <-
          case heldAltState of
               State.AltIsHeldForAlternativeMode x -> Just x
               _                                   -> Nothing

        let f State.HeldLeftAltForAlternativeMode  = Keys.AltLeftKey
            f State.HeldRightAltForAlternativeMode = Keys.AltRightKey

        guard $ not isPressed && keyName == f heldAltKey

  abstractRelease
    :: String -- ^ @releaseMsg@
    -> (KeyName -> String) -- ^ @releaseItemMsgMask@
    -> (KeyName -> Bool) -- ^ @splitter@
    -> (KeyName -> Maybe KeyCode) -- ^ @getter@
    -> Set KeyName -- ^ @pressed@
    -> IO $ Set KeyName -- ^ Returns rest of @pressed@

  abstractRelease releaseMsg releaseItemMsgMask splitter getter pressed = do
    let (toRelease, rest) = Set.partition splitter pressed
    when (Set.size toRelease > 0) $ do
      noise releaseMsg
      forM_ (Set.toList toRelease) $ \keyName -> do
        noise $ releaseItemMsgMask keyName
        let Just keyCode = getter keyName
         in releaseKey keyCode
    pure rest

  -- -- | Release alternative keys.
  -- --
  -- -- Useful when alternative mode turns off not by both alts
  -- -- and key could be still pressed.
  -- --
  -- -- It's commented because it's never used anywhere
  -- releaseAlternative :: Set KeyName -> IO $ Set KeyName
  -- releaseAlternative = abstractRelease
  --
  --   "Releasing alternative keys during turning alternative mode off..."
  --
  --   (\keyName -> [qms| Releasing alternative {keyName}
  --                      during turning alternative mode off... |])
  --
  --   hasAlternativeRemap
  --   (getAlternativeRemapByName .> fmap snd)

  -- | Release apple media keys.
  --
  -- Useful when user released @FNKey@ erlier than media key.
  releaseAppleMedia :: Set KeyName -> IO $ Set KeyName
  releaseAppleMedia = abstractRelease

    [qms| Releasing held media keys of apple keyboard
          after {Keys.FNKey} released... |]

    (\keyName -> [qms| Releasing held media {keyName} of apple keyboard
                       after {Keys.FNKey} released... |])

    isMediaKey
    getMediaKeyCode

  -- | Triggering of a key user pressed to X server.
  --
  -- It supports optionally remapped key.
  trigger :: KeyName -> KeyCode -> Bool -> IO ()
  trigger keyName keyCode isPressed = do

    let asKeyName = getRemapByName keyName

    noise [qms| Triggering {isPressed ? "pressing" $ "releasing"}
                of {keyName}{maybe mempty ((" as " <>) . show) asKeyName}
                (X key code: {keyCode})... |]

    (pressKey |?| releaseKey) isPressed keyCode

  -- | Multiple version of "trigger" (supports alternative remapping)
  alternativeMultipleTrigger
    :: Maybe AlternativeModeLevel -> Set KeyName -> Bool -> IO ()

  alternativeMultipleTrigger level keyNames isPressed = go where
    go = do
      noise' $ flip fmap keys $ \(keyName, asKeyName, code) ->
        [qms| Triggering {isPressed ? "pressing" $ "releasing"}
              of {keyStr keyName asKeyName} (X key code: {code})... |]

      (pressKeys |?| releaseKeys) isPressed $ view _3 <$> keys

    keyStr keyName (Left Nothing) = show keyName
    keyStr keyName (Left (Just asKeyName)) = [qm| {keyName} (as {asKeyName}) |]
    keyStr keyName (Right asKeyName) =
      [qm| alternative {keyName} (as {asKeyName}) |]

    keys :: [(KeyName, Either (Maybe KeyName) KeyName, KeyCode)]
    keys = foldl reducer [] keyNames where
      reducer acc k = maybe acc (: acc) $
        case level >>= flip getAlternativeRemapByName k of
             Just (Left _) -> Nothing

             Just (Right (asKeyName, keyCode)) ->
               Just (k, Right asKeyName, keyCode)

             Nothing ->
               getKeyCodeByName k <&> \x -> (k, Left $ getRemapByName k, x)

  -- | Triggering both press and release events to X server.
  --
  -- It supports optionally remapped key.
  triggerPressRelease :: KeyName -> KeyCode -> IO ()
  triggerPressRelease keyName keyCode = do

    noise [qms| Triggering pressing and releasing of {keyName}\
                {maybe mempty ((" as " <>) . show) $ getRemapByName keyName}
                (X key code: {keyCode})... |]

    pressReleaseKey keyCode

  onlyRealModifiers :: Set KeyName
  onlyRealModifiers = Set.fromList
    [ Keys.ControlLeftKey, Keys.ControlRightKey
    , Keys.ShiftLeftKey,   Keys.ShiftRightKey
    , Keys.AltLeftKey,     Keys.AltRightKey
    ]

  -- | Union of all modifiers keys.
  --
  -- Including keys that are remapped as those modifiers.
  allModifiersKeys :: Set KeyName
  allModifiersKeys = mods `Set.union` remappedMods where
    -- | Just Set of modifiers keys
    mods :: Set KeyName
    mods = Set.fromList
      [ Keys.ControlLeftKey, Keys.ControlRightKey
      , Keys.SuperLeftKey,   Keys.SuperRightKey
      , Keys.AltLeftKey,     Keys.AltRightKey
      , Keys.ShiftLeftKey,   Keys.ShiftRightKey
      ]

    -- | Other keys that was remapped as modifiers keys,
    --   that means they're modifiers too.
    --
    -- For example @LessKey@ is remapped to @ShiftLeftKey@,
    -- so it means that this key is a modifier too.
    remappedMods :: Set KeyName
    remappedMods = Set.foldr (Set.union . getExtraKeys) Set.empty mods

  -- | A helper for logging (to also show remapped key).
  maybeAsKeyStr :: KeyName -> String
  maybeAsKeyStr keyName
    = getRemapByName keyName
    & show keyName `maybe` \asKeyName -> [qm| {keyName} (as {asKeyName}) |]


data DoubleSuperWithAlternativeAction
   = NextStepFlowAndTriggerAlternative
   | NextStepFlowActLikeKeyIsNotPressed
   | NoNextStepActLikeKeyIsNotPressed
   | AlternativeModeIsPermanentActLikeKeyIsNotPressed
     deriving (Show, Eq)
