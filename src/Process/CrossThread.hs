-- Author: Viacheslav Lotsmanov
-- License: GPLv3 https://raw.githubusercontent.com/unclechu/xlib-keys-hack/master/LICENSE

{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Process.CrossThread
  ( handleCapsLockModeChange
  , handleAlternativeModeChange

  , toggleCapsLock
  , toggleAlternative

  , turnCapsLockMode
  , turnAlternativeMode

  , justTurnCapsLockMode

  , notifyAboutAlternative

  , handleResetKbdLayout
  , resetKbdLayout

  , resetAll
  ) where

import "X11" Graphics.X11.Xlib (Display)

import "lens" Control.Lens ((.~), (^.), view, Lens')
import "transformers" Control.Monad.Trans.State (execStateT)
import "transformers" Control.Monad.Trans.Except (runExceptT, throwE)
import qualified "mtl" Control.Monad.State.Class as St (MonadState)
import "base" Control.Monad.IO.Class (MonadIO, liftIO)

import qualified "containers" Data.Set as Set (null)
import "base" Data.Maybe (fromJust, isJust)
import "qm-interpolated-string" Text.InterpolatedString.QM (qm, qms, qns)

-- local imports

import           Utils.StateMonad (modifyState, modifyStateM)
import           Utils.BreakableMonad (continueIf)
import           Utils.Sugar ((?), (|?|), (.>))
import           Bindings.XTest (fakeKeyCodeEvent)
import           Bindings.MoreXlib (getLeds)
import           Options (type Options)
import qualified Actions
import           State (type State, type CrossThreadVars)
import qualified State
import           Keys (type KeyMap)
import qualified Keys



type Noiser         = [String] -> IO ()
type Notifier       = [Actions.XmobarFlag] -> IO ()
type ModeChangeLens = Lens' State (Maybe Bool)



-- Abstraction for handling delayed actions connected to keys
-- (Caps Lock mode or Alternative mode).
handleModeChange :: ModeChangeLens -- Lens for delayed mode change
                                   -- state that we handle.
                 -> (String, String) -- Info messages to write it to log
                 -> (State -> IO State) -- Monad that do something to handle it.
                                        -- You can change the state here
                                        -- and it will be stored!
                                        -- It can reset mode by simulating
                                        -- some keys events for example.
                 -> Bool -- Flag that indicates current state of mode
                         -- that we handle.
                 -> Noiser -> State -> IO State
handleModeChange mcLens (doingItMsg, alreadyMsg) doHandle isNowOn
                 noise' state =

  flip execStateT state . runExceptT $ do

  -- Remove delayed task if it's already done
  if hasDelayed && isAlreadyDone -- Nothing to do, already done
     then liftIO (noise' [alreadyMsg])
            >> modifyState (mcLens .~ Nothing)
            >> throwE ()
     else pure () -- Go further

  -- Do nothing if Caps Lock mode changing is not requested
  -- or if all another keys isn't released yet.
  continueIf $ hasDelayed && everyKeyIsReleased

  -- Handling it!
  liftIO $ noise' [doingItMsg]
  modifyStateM $ liftIO . doHandle -- State can be modified there
  modifyState  $ mcLens .~ Nothing -- Reset delayed mode change

  where hasDelayed         = (isJust   $ state ^. mcLens)         :: Bool
        toOn               = (fromJust $ state ^. mcLens)         :: Bool
        isAlreadyDone      = (toOn == isNowOn)                    :: Bool
        everyKeyIsReleased = (Set.null $ State.pressedKeys state) :: Bool


-- Handle delayed Caps Lock mode change
handleCapsLockModeChange :: CrossThreadVars -> Noiser -> State -> IO State
handleCapsLockModeChange ctVars noise' state =

  handleModeChange mcLens (doingItMsg, alreadyMsg) handler isNowOn
                   noise' state

  where doingItMsg = [qms| Delayed Caps Lock mode turning
                           {toOn ? "on" $ "off"}
                           after all other keys release
                           (by pressing and releasing {keyName})... |]

        alreadyMsg = [qms| Delayed Caps Lock mode turning
                           {toOn ? "on" $ "off"}
                           after all other keys release was skipped
                           because it's already done now |]

        mcLens :: ModeChangeLens
        mcLens = State.comboState' . State.capsLockModeChange'

        toOn    = (fromJust $ state ^. mcLens)                :: Bool
        isNowOn = (state ^. State.leds' . State.capsLockLed') :: Bool

        keyName = Keys.RealCapsLockKey

        handler :: State -> IO State
        handler s = s <$ Actions.turnCapsLock ctVars toOn


-- Handle delayed Alternative mode change
handleAlternativeModeChange :: Noiser -> Notifier -> State -> IO State
handleAlternativeModeChange noise' notify' state =

  handleModeChange mcLens (doingItMsg, alreadyMsg) handler isNowOn
                   noise' state

  where doingItMsg = [qms| Delayed Alternative mode turning
                           {toOn ? "on" $ "off"}
                           after all other keys release... |]

        alreadyMsg = [qms| Delayed Alternative mode turning
                           {toOn ? "on" $ "off"}
                           after all other keys release was skipped
                           because it's already done now |]

        mcLens :: ModeChangeLens
        mcLens = State.comboState' . State.alternativeModeChange'

        toOn    = (fromJust $ state ^. mcLens)  :: Bool
        isNowOn = (state ^. State.alternative') :: Bool

        handler :: State -> IO State
        handler = changeAlternativeMode toOn
                   .> (\s -> s <$ notifyAboutAlternative notify' s)



-- Abstraction for turning mode (caps lock/alternative) on/off
turnMode :: ModeChangeLens -- Lens for delayed mode change
                           -- to toggle it later if it's
                           -- bad time for that now.
         -> ([String], [String]) -- Info messages to log
         -> Maybe (Bool, [String]) -- Previous state and message
                                   -- if it's already done.
         -> (State -> IO State) -- Handler to call if it's possible right now.
                                -- It's possible to change state there
                                -- and it will be stored.
         -> Bool -- State to turn in ON or OFF
         -> Noiser -> State -> IO State
turnMode mcLens (immediatelyMsgs, laterMsgs) already nowHandle toOn
         noise' state =

  flip execStateT state . runExceptT $ do

  -- It's already done
  if isJust already && let Just (isNowOn, _) = already
                        in toOn == isNowOn
     then let (_, alreadyMsgs) = fromJust already
           in liftIO (noise' alreadyMsgs)
                >> modifyState (mcLens .~ Nothing) -- Clear possible previous
                                                   -- delayed action.
                >> throwE ()
     else pure () -- Go further

  -- Doing it right now
  if Set.null (State.pressedKeys state)
     then liftIO (noise' immediatelyMsgs)
            >> modifyStateM (liftIO . nowHandle) -- State can be modified there
            >> modifyState  (mcLens .~ Nothing)  -- Clear possible previous
                                                 -- delayed action.
            >> throwE ()
     else pure () -- Or not, go further

  -- Let's do it later
  liftIO $ noise' laterMsgs
  modifyState $ mcLens .~ Just toOn



toggleCapsLock :: CrossThreadVars -> Noiser -> State -> IO State
toggleCapsLock ctVars noise' state =

  turnMode mcLens ([immediatelyMsg], laterMsgs) Nothing handler toOn
           noise' state

  where immediatelyMsg =
          [qms| Toggling Caps Lock mode (turning it {onOrOff toOn}
                by pressing and releasing {keyName})... |]

        laterMsgs = [ [qms| Attempt to toggle Caps Lock mode
                            (to turn it {onOrOff toOn}
                            by pressing and releasing {keyName})
                            while pressed some another keys |]

                    , [qms| Storing in state request to turn Caps Lock mode
                            {onOrOff toOn} after all another keys release... |]
                    ]

        mcLens :: ModeChangeLens
        mcLens  = State.comboState' . State.capsLockModeChange'

        toOn    = (not $ state ^. State.leds' . State.capsLockLed') :: Bool

        keyName = Keys.RealCapsLockKey

        handler :: State -> IO State
        handler s = s <$ Actions.turnCapsLock ctVars toOn


toggleAlternative :: Noiser -> Notifier -> State -> IO State
toggleAlternative noise' notify' state =

  turnMode mcLens ([immediatelyMsg], laterMsgs) Nothing handler toOn
           noise' state

  where immediatelyMsg = [qms| Toggling Alternative mode
                               (turning it {onOrOff toOn})... |]

        laterMsgs = [ [qms| Attempt to toggle Alternative mode
                            (to turn it {onOrOff toOn})
                            while pressed some another keys |]

                    , [qms| Storing in state request to turn Alternative mode
                            {onOrOff toOn} after all another keys release... |]
                    ]

        mcLens :: ModeChangeLens
        mcLens  = State.comboState' . State.alternativeModeChange'

        toOn    = (not $ state ^. State.alternative') :: Bool

        handler :: State -> IO State
        handler = changeAlternativeMode toOn
                   .> (\s -> s <$ notifyAboutAlternative notify' s)



turnCapsLockMode :: CrossThreadVars -> Noiser -> State -> Bool -> IO State
turnCapsLockMode ctVars noise' state toOn =

  turnMode mcLens ([immediatelyMsg], laterMsgs) already handler toOn
           noise' state

  where immediatelyMsg = [qms| Turning Caps Lock mode {onOrOff toOn}
                               (by pressing and releasing {keyName})... |]

        laterMsgs = [ [qms| Attempt to turn Caps Lock mode {onOrOff toOn}
                            (by pressing and releasing {keyName})
                            while pressed some another keys |]

                    , [qms| Storing in state request to turn Caps Lock mode
                            {onOrOff toOn} after all another keys release... |]
                    ]

        alreadyMsg = [qms| Skipping attempt to turn Caps Lock mode
                           {onOrOff toOn}, because it's already done... |]

        mcLens :: ModeChangeLens
        mcLens  = State.comboState' . State.capsLockModeChange'

        isNowOn = (state ^. State.leds' . State.capsLockLed') :: Bool
        already = Just (isNowOn, [alreadyMsg]) :: Maybe (Bool, [String])

        keyName = Keys.RealCapsLockKey

        handler :: State -> IO State
        handler s = s <$ Actions.turnCapsLock ctVars toOn


turnAlternativeMode :: Noiser -> Notifier -> State -> Bool -> IO State
turnAlternativeMode noise' notify' state toOn =

  turnMode mcLens ([immediatelyMsg], laterMsgs) already handler toOn
           noise' state

  where immediatelyMsg = [qm| Turning Alternative mode {onOrOff toOn}... |]

        laterMsgs = [ [qms| Attempt to turn Alternative mode {onOrOff toOn}
                            while pressed some another keys |]

                    , [qms| Storing in state request to turn Alternative mode
                            {onOrOff toOn} after all another keys release... |]
                    ]

        alreadyMsg = [qms| Skipping attempt to turn Alternative mode
                           {onOrOff toOn}, because it's already done... |]

        mcLens :: ModeChangeLens
        mcLens  = State.comboState' . State.alternativeModeChange'

        isNowOn = (state ^. State.alternative') :: Bool
        already = Just (isNowOn, [alreadyMsg])  :: Maybe (Bool, [String])

        handler :: State -> IO State
        handler = changeAlternativeMode toOn
                   .> (\s -> s <$ notifyAboutAlternative notify' s)



handleResetKbdLayout :: CrossThreadVars -> Noiser -> State -> IO State
handleResetKbdLayout ctVars noise' state =

  flip execStateT state . runExceptT $ do

  -- Break if we don't have to do anything
  continueIf hasDelayed

  -- Skip if it's already done
  if State.kbdLayout state == 0
     then let msg = [qns| Delayed reset keyboard layout to default
                          after all other keys release was skipped
                          because it's already done now |]
           in liftIO (noise' [msg])
                >> modifyState (mcLens .~ False)
                >> throwE ()
     else pure ()

  -- Do nothing right now if we have some not released keys yet
  continueIf everyKeyIsReleased

  -- Perfect time to do it!
  liftIO $ noise' [ [qns| Delayed resetting keyboard layout to default
                          after all other keys release... |] ]
  liftIO handle
  modifyState $ mcLens .~ False

  where mcLens             = State.comboState' . State.resetKbdLayout'
        handle             = Actions.resetKeyboardLayout ctVars
        hasDelayed         = (state ^. mcLens) :: Bool
        everyKeyIsReleased = (Set.null $ State.pressedKeys state) :: Bool

resetKbdLayout :: CrossThreadVars -> Noiser -> State -> IO State
resetKbdLayout ctVars noise' state = flip execStateT state . runExceptT $ do

  -- Skip if it's already done
  if State.kbdLayout state == 0
     then let msg = [qns| Skipping attempt to reset keyboard layout
                          to default because it's already done |]
           in liftIO (noise' [msg])
                >> modifyState (mcLens .~ False)
                >> throwE ()
     else pure ()

  -- Doing it right now
  if Set.null (State.pressedKeys state)
     then liftIO (noise' ["Resetting keyboard layout to default..."])
            >> liftIO handle
            >> modifyState (mcLens .~ False) -- Clear possible previous
                                             -- delayed action.
            >> throwE ()
     else pure () -- Or not, go further

  -- Let's do it later
  liftIO $ noise' [ [qns| Attempt to reset keyboard layout to default
                          while pressed some another keys |]
                  , [qns| Storing in state request to reset keyboard layout
                          to default after all another keys release... |]
                  ]
  modifyState $ mcLens .~ True

  where mcLens = State.comboState' . State.resetKbdLayout'
        handle = Actions.resetKeyboardLayout ctVars



resetAll :: (St.MonadState State m, MonadIO m)
         => Options -> CrossThreadVars -> Noiser -> Notifier -> m ()
resetAll _ ctVars noise' notify' = do

  liftIO $ noise' ["Resetting keyboard layout..."]
  modifyStateM $ liftIO . _resetKbdLayout

  liftIO $ noise' ["Resetting Caps Lock mode..."]
  modifyStateM $ liftIO . turnCapsLockModeOff

  liftIO $ noise' ["Resetting Alternative mode..."]
  modifyStateM $ liftIO . turnAlternativeModeOff

  where _resetKbdLayout = Process.CrossThread.resetKbdLayout ctVars noise'
        turnCapsLockModeOff = flip (turnCapsLockMode ctVars noise') False
        turnAlternativeModeOff = flip (turnAlternativeMode noise' notify') False



-- Turns Caps Lock mode on/off without checking pressed keys
-- but checks for led state.
justTurnCapsLockMode :: Display -> (String -> IO ()) -> KeyMap -> Bool -> IO ()
justTurnCapsLockMode dpy noise keyMap isOn =

  let logIt = noise [qms| Turning Caps Lock mode {onOrOff isOn}
                          (by pressing and releasing {keyName})... |]

      f = fakeKeyCodeEvent dpy keyCode
      toggle = f True >> f False

      -- Sometimes for some reason Caps Lock mode led returns True
      -- at initialization step even if Caps Lock mode is disabled,
      -- let's bang Caps Lock key until it is really disabled.
      recur = do
        toggle
        (view State.capsLockLed' -> isReallyOn) <- getLeds dpy
        isReallyOn /= isOn ? recur $ return ()

   in logIt >> recur

  `orIfAlreadyOn`

  noise [qms| Attempt to turn Caps Lock mode {onOrOff isOn},
              it's already done, skipping... |]

  where keyName = Keys.RealCapsLockKey
        keyCode = fromJust $ Keys.getRealKeyCodeByName keyMap keyName

        orIfAlreadyOn :: IO () -> IO () -> IO ()
        a `orIfAlreadyOn` b = do
          (view State.capsLockLed' -> isOnAlready) <- getLeds dpy
          isOn /= isOnAlready ? a $ b



-- Alternative mode change bare handler
changeAlternativeMode :: Bool -> State -> State
changeAlternativeMode toOn = State.alternative' .~ toOn


-- Notify xmobar about Alternative mode state
notifyAboutAlternative :: Notifier -> State -> IO ()
notifyAboutAlternative notify' state =
  notify' [Actions.XmobarAlternativeFlag $ State.alternative state]


onOrOff :: Bool -> String
onOrOff = "on" |?| "off"
