-- Author: Viacheslav Lotsmanov
-- License: GPLv3 https://raw.githubusercontent.com/unclechu/xlib-keys-hack/master/LICENSE

{-# LANGUAGE NoMonomorphismRestriction, RankNTypes, FlexibleContexts #-}
{-# LANGUAGE DataKinds, MultiParamTypeClasses, TupleSections #-}

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

import "base" Data.Proxy (Proxy (Proxy))
import "data-default" Data.Default (def)
import "base" Data.Maybe (isJust)
import qualified "containers" Data.Set as Set (null)
import "qm-interpolated-string" Text.InterpolatedString.QM (qm, qms, qns)

import "base" Control.Monad (when, unless, join)
import "transformers" Control.Monad.Trans.State (execStateT)
import "transformers" Control.Monad.Trans.Except (runExceptT, throwE)
import qualified "mtl" Control.Monad.State.Class as St
                     ( MonadState (get, put), modify
                     )
import "base" Control.Monad.IO.Class (MonadIO (liftIO))
import "lens" Control.Lens ((.~), (^.), view, set, Lens')

import "X11" Graphics.X11.Types (type KeyCode)
import "X11" Graphics.X11.Xlib.Types (type Display)

-- local imports

import           Utils.Sugar ((?), (|?|), (.>), unnoticed)
import           Bindings.XTest (fakeKeyCodeEvent)
import           Bindings.MoreXlib (getLeds)
import           Options (type Options, defaultKeyboardLayout)
import qualified Actions
import           State (type State, type CrossThreadVars)
import qualified State
import qualified Keys
import           Types (type AlternativeModeState, AlternativeModeLevel (..))



type Noiser           = [String] -> IO ()
type Notifier         = [Actions.XmobarFlag] -> IO ()
type ModeChangeLens a = Lens' State (Maybe a)



-- | Abstraction for handling delayed actions connected to keys
--   (Caps Lock mode or Alternative mode).
handleModeChange
  :: Eq a
  => ModeChangeLens a -- ^ Lens for delayed mode change state that we handle
  -> (String, String) -- ^ Info messages to write it to log
  -> (State -> IO State)
  -- ^ Monad that do something to handle it.
  --   You can change the state here and it will be stored!
  --   It can reset mode by simulating some keys events for example.
  -> a -- ^ Current state of mode that we handle
  -> Noiser -> State -> IO State

handleModeChange mcLens (doingItMsg, alreadyMsg) doHandle currentModeState
                 noise' state = runExceptT go `execStateT` state where
  go = do
    -- Remove delayed task if it's already done
    when (maybe False (== currentModeState) delayedModeState) $ do
      liftIO $ noise' [alreadyMsg]
      St.modify $ mcLens .~ Nothing
      throwE () -- Nothing to do, already done

    -- Do nothing if Caps Lock mode changing is not requested
    -- or if all another keys isn't released yet.
    unless (isJust delayedModeState && everyKeyIsReleased) $ throwE ()

    -- Handling it now!
    handleRightNow

  handleRightNow = do
    liftIO $ noise' [doingItMsg]
    St.get >>= liftIO . doHandle >>= St.put
    St.modify $ mcLens .~ Nothing -- Reset delayed mode change

  everyKeyIsReleased = Set.null $ State.pressedKeys state :: Bool
  delayedModeState = state ^. mcLens


-- | Handle delayed Caps Lock mode change.
--
-- See "handleModeChange", it's generic implementation of this particular case.
handleCapsLockModeChange :: CrossThreadVars -> Noiser -> State -> IO State
handleCapsLockModeChange ctVars noise' state = go where

  go = handleModeChange mcLens (doingItMsg, alreadyMsg) handler currentModeState
                        noise' state

  doingItMsg = [qms| Delayed Caps Lock mode turning
                     {maybe mempty onOrOff delayedModeState}
                     after all other keys release
                     (by pressing and releasing {keyName})... |]

  alreadyMsg = [qms| Delayed Caps Lock mode turning
                     {maybe mempty onOrOff delayedModeState}
                     after all other keys release was skipped
                     because it's already done now |]

  mcLens :: ModeChangeLens Bool
  mcLens = State.comboState' . State.capsLockModeChange'

  delayedModeState = state ^. mcLens
  currentModeState = state ^. State.leds' . State.capsLockLed' :: Bool

  keyName = Keys.CapsLockKey

  handler :: State -> IO State
  handler = (<$ maybe (pure ()) (Actions.turnCapsLock ctVars) delayedModeState)


-- | Handle delayed Alternative mode change.
--
-- See "handleModeChange", it's generic implementation of this particular case.
handleAlternativeModeChange :: Noiser -> Notifier -> State -> IO State
handleAlternativeModeChange noise' notify' state = go where

  go = handleModeChange mcLens (doingItMsg, alreadyMsg) handler currentModeState
                        noise' state

  shownAction
    = showAlternativeModeStateChangeAction currentModeState
    $ join delayedModeState

  doingItMsg = [qms| Delayed Alternative mode {shownAction}
                     after all other keys release... |]

  alreadyMsg = [qms| Delayed Alternative mode {shownAction}
                     after all other keys release was skipped
                     because it's already done now |]

  mcLens :: ModeChangeLens AlternativeModeState
  mcLens = State.comboState' . State.alternativeModeChange'

  delayedModeState = view mcLens state
  currentModeState = State.alternative state

  handler :: State -> IO State
  handler
    = maybe id changeAlternativeMode delayedModeState
    .> unnoticed (notifyAboutAlternative notify')



-- | Abstraction for turning mode (caps lock/alternative) on/off
turnMode
  :: Eq a
  => ModeChangeLens a
  -- ^ Lens for delayed mode change if it's bad time to do that right now
  -> ([String], [String]) -- ^ Info messages to log
  -> Maybe (a, [String])
  -- ^ Previous mode state and message if it's already done
  -> (State -> IO State)
  -- ^ Handler to call if it's possible right now.
  --   It's possible to change state there and it will be stored.
  -> a -- ^ New mode state
  -> Noiser -> State -> IO State

turnMode mcLens (immediatelyMsgs, laterMsgs) already nowHandle newModeState
         noise' state = runExceptT go `execStateT` state where
  go = do
    -- In case it's already done
    pure () `maybe` alreadyCase $ already

    -- Doing it right now
    when (Set.null $ State.pressedKeys state) $ do
      liftIO $ noise' immediatelyMsgs
      St.get >>= liftIO . nowHandle >>= St.put -- State can be modified there
      St.modify $ mcLens .~ Nothing -- Clear possible previous delayed action.
      throwE ()

    -- Let's do it later
    liftIO $ noise' laterMsgs
    St.modify $ mcLens .~ Just newModeState

  alreadyCase (currentModeState, alreadyMsgs) =
    when (newModeState == currentModeState) $ do
      liftIO $ noise' alreadyMsgs
      St.modify $ mcLens .~ Nothing -- Clear possible previous delayed action
      throwE ()



-- | See "turnMode" generic implementation.
toggleCapsLock :: CrossThreadVars -> Noiser -> State -> IO State
toggleCapsLock ctVars noise' state = go where
  go =
    turnMode mcLens ([immediatelyMsg], laterMsgs) Nothing handler newModeState
             noise' state

  immediatelyMsg =
    [qms| Toggling Caps Lock mode (turning it {onOrOff newModeState}
          by pressing and releasing {keyName})... |]

  laterMsgs = [ [qms| Attempt to toggle Caps Lock mode
                      (to turn it {onOrOff newModeState}
                      by pressing and releasing {keyName})
                      while pressed some another keys |]

              , [qms| Storing in state request to turn Caps Lock mode
                      {onOrOff newModeState} after all another keys release... |]
              ]

  mcLens :: ModeChangeLens Bool
  mcLens = State.comboState' . State.capsLockModeChange'

  newModeState = not $ state ^. State.leds' . State.capsLockLed' :: Bool

  keyName = Keys.CapsLockKey

  handler :: State -> IO State
  handler = (<$ Actions.turnCapsLock ctVars newModeState)


-- | See "turnMode" generic implementation.
--
-- When alternative mode is off it switches to permanent first level.
toggleAlternative :: Noiser -> Notifier -> State -> IO State
toggleAlternative noise' notify' state = go where
  go =
    turnMode mcLens ([immediatelyMsg], laterMsgs) Nothing handler newModeState
             noise' state

  immediatelyMsg = [qms| Toggling Alternative mode
                         (turning it {onOrOff $ isJust newModeState})... |]

  laterMsgs = [ [qms| Attempt to toggle Alternative mode
                      (to turn it {onOrOff $ isJust newModeState})
                      while pressed some another keys |]

              , [qms| Storing in state request to turn Alternative mode
                      {onOrOff $ isJust newModeState}
                      after all another keys release... |]
              ]

  mcLens :: ModeChangeLens AlternativeModeState
  mcLens = State.comboState' . State.alternativeModeChange'

  newModeState :: AlternativeModeState
  newModeState = case State.alternative state of
    Just _  -> Nothing
    Nothing -> Just (def, True)

  handler :: State -> IO State
  handler
    = changeAlternativeMode newModeState
    .> unnoticed (notifyAboutAlternative notify')



-- | See "turnMode" generic implementation.
turnCapsLockMode :: CrossThreadVars -> Noiser -> State -> Bool -> IO State
turnCapsLockMode ctVars noise' state newModeState = go where
  go =
    turnMode mcLens ([immediatelyMsg], laterMsgs) already handler newModeState
             noise' state

  immediatelyMsg = [qms| Turning Caps Lock mode {onOrOff newModeState}
                         (by pressing and releasing {keyName})... |]

  laterMsgs = [ [qms| Attempt to turn Caps Lock mode {onOrOff newModeState}
                      (by pressing and releasing {keyName})
                      while pressed some another keys |]

              , [qms| Storing in state request to turn Caps Lock mode
                      {onOrOff newModeState}
                      after all another keys release... |]
              ]

  alreadyMsg = [qms| Skipping attempt to turn Caps Lock mode
                     {onOrOff newModeState}, because it's already done... |]

  mcLens :: ModeChangeLens Bool
  mcLens  = State.comboState' . State.capsLockModeChange'

  isNowOn = state ^. State.leds' . State.capsLockLed' :: Bool
  already = Just (isNowOn, [alreadyMsg]) :: Maybe (Bool, [String])

  keyName = Keys.CapsLockKey

  handler :: State -> IO State
  handler = (<$ Actions.turnCapsLock ctVars newModeState)


-- | See "turnMode" generic implementation.
turnAlternativeMode
  :: Noiser -> Notifier -> State -> AlternativeModeState -> IO State

turnAlternativeMode noise' notify' state newModeState = go where
  go =
    turnMode mcLens ([immediatelyMsg], laterMsgs) already handler newModeState
             noise' state

  shownAction =
    showAlternativeModeStateChangeAction currentModeState newModeState

  immediatelyMsg = [qm| Alternative mode is {shownAction}... |]

  laterMsgs = [ [qms| Alternative mode, attempt of {shownAction}
                      while pressed some another keys |]

              , [qms| Storing in state a request of Alternative mode
                      {shownAction} after all another keys release... |]
              ]

  alreadyMsg = [qms| Alternative mode, skipping attempt of {shownAction},
                     because it's already done... |]

  mcLens :: ModeChangeLens AlternativeModeState
  mcLens = State.comboState' . State.alternativeModeChange'

  currentModeState = State.alternative state
  already = Just (currentModeState, [alreadyMsg])

  handler :: State -> IO State
  handler
    = changeAlternativeMode newModeState
    .> unnoticed (notifyAboutAlternative notify')



handleResetKbdLayout
  :: Options -> CrossThreadVars -> Noiser -> State -> IO State

handleResetKbdLayout opts ctVars noise' state = go where
  go = runExceptT go' `execStateT` state

  go' = do
    -- Break if we don't have to do anything
    unless hasDelayed $ throwE ()

    -- Skip if it's already done
    when (State.kbdLayout state == defaultKeyboardLayout opts) $ do
      liftIO $ noise' $ pure
        [qns| Delayed reset keyboard layout to default after all other keys
              release was skipped because it's already done now |]

      St.modify $ mcLens .~ False
      throwE ()

    -- Do nothing right now if we have some not released keys yet
    unless everyKeyIsReleased $ throwE ()

    -- Perfect time to do it!
    liftIO $ noise' [ [qns| Delayed resetting keyboard layout to default
                            after all other keys release... |] ]
    liftIO handle
    St.modify $ mcLens .~ False

  mcLens             = State.comboState' . State.resetKbdLayout'
  handle             = Actions.resetKeyboardLayout ctVars
  hasDelayed         = state ^. mcLens :: Bool
  everyKeyIsReleased = Set.null $ State.pressedKeys state :: Bool

resetKbdLayout :: Options -> CrossThreadVars -> Noiser -> State -> IO State
resetKbdLayout opts ctVars noise' state = runExceptT go `execStateT` state where
  go = do
    -- Skip if it's already done
    when (State.kbdLayout state == defaultKeyboardLayout opts) $ do
      liftIO $ noise' $ pure
        [qns| Skipping attempt to reset keyboard layout to default
              because it's already done |]

      St.modify $ mcLens .~ False
      throwE ()

    -- Doing it right now
    when (Set.null $ State.pressedKeys state) $ do
      liftIO $ noise' ["Resetting keyboard layout to default..."]
      liftIO handle
      St.modify $ mcLens .~ False -- Clear possible previous delayed action
      throwE ()

    -- Let's do it later
    liftIO $ noise' [ [qns| Attempt to reset keyboard layout to default
                            while pressed some another keys |]
                    , [qns| Storing in state request to reset keyboard layout
                            to default after all another keys release... |]
                    ]

    St.modify $ mcLens .~ True

  mcLens = State.comboState' . State.resetKbdLayout'
  handle = Actions.resetKeyboardLayout ctVars



resetAll
  :: (St.MonadState State m, MonadIO m)
  => Options -> CrossThreadVars -> Noiser -> Notifier -> m ()

resetAll opts ctVars noise' notify' = go where
  go = do
    liftIO $ noise' ["Resetting keyboard layout..."]
    St.get >>= liftIO . _resetKbdLayout >>= St.put

    liftIO $ noise' ["Resetting Caps Lock mode..."]
    St.get >>= liftIO . turnCapsLockModeOff >>= St.put

    liftIO $ noise' ["Resetting Alternative mode..."]
    St.get >>= liftIO . turnAlternativeModeOff >>= St.put

  _resetKbdLayout = Process.CrossThread.resetKbdLayout opts ctVars noise'
  turnCapsLockModeOff = flip (turnCapsLockMode ctVars noise') False
  turnAlternativeModeOff = flip (turnAlternativeMode noise' notify') Nothing



-- Turns Caps Lock mode on/off without checking pressed keys
-- but checks for led state.
justTurnCapsLockMode
  :: Display
  -> (String -> IO ())
  -> (Proxy 'Keys.CapsLockKey, KeyCode)
  -- ^ "KeyCode" of real Caps Lock key (it must be not remapped!)
  -> Bool
  -> IO ()

justTurnCapsLockMode dpy noise (Proxy, capsLockKeyCode) isOn = go where
  go =
    (
      let
        logIt = noise [qms| Turning Caps Lock mode {onOrOff isOn}
                            (by pressing and releasing {Keys.CapsLockKey})... |]

        f = fakeKeyCodeEvent dpy capsLockKeyCode
        toggle = f True >> f False

        -- Sometimes for some reason Caps Lock mode led returns True
        -- at initialization step even if Caps Lock mode is disabled,
        -- let's bang Caps Lock key until it is really disabled.
        recur = do
          toggle
          (view State.capsLockLed' -> isReallyOn) <- getLeds dpy
          isReallyOn /= isOn ? recur $ pure ()
      in
        logIt >> recur
    )

    `orIfAlreadyOn`

    noise [qms| Attempt to turn Caps Lock mode {onOrOff isOn},
                it's already done, skipping... |]

  orIfAlreadyOn :: IO () -> IO () -> IO ()
  a `orIfAlreadyOn` b = do
    (view State.capsLockLed' -> isOnAlready) <- getLeds dpy
    isOn /= isOnAlready ? a $ b



showAlternativeModeStateChangeAction
  :: AlternativeModeState -> AlternativeModeState -> String

showAlternativeModeStateChangeAction currentModeState newModeState = go where
  go =
    case (currentModeState, newModeState) of

         (Nothing, Just (level, isPermanent)) ->
           [qms| turning on with {showLevel level}
                 {showPermanentFlag isPermanent} |]

         (Just (curLevel, curIsPermanent), Just (level, isPermanent)) ->
           [qms| switching from
                 {showLevel curLevel} {showPermanentFlag curIsPermanent}
                 to {showLevel level} {showPermanentFlag isPermanent} |]

         (Just (curLevel, curIsPermanent), Nothing) ->
           [qms| turning off from {showLevel curLevel}
                 {showPermanentFlag curIsPermanent} |]

         (Nothing, Nothing) -> "turning off"

  showLevel FirstAlternativeModeLevel  = "1st level"
  showLevel SecondAlternativeModeLevel = "2nd level"

  showPermanentFlag = "permanently" |?| "temporarily"


-- | Alternative mode change bare handler
changeAlternativeMode :: AlternativeModeState -> State -> State
changeAlternativeMode = set State.alternative'


-- | Notify xmobar about Alternative mode state
notifyAboutAlternative :: Notifier -> State -> IO ()
notifyAboutAlternative notify' state =
  notify' [Actions.XmobarAlternativeFlag $ State.alternative state]


onOrOff :: Bool -> String
onOrOff = "on" |?| "off"
