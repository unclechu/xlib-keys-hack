-- Author: Viacheslav Lotsmanov
-- License: GPLv3 https://raw.githubusercontent.com/unclechu/xlib-keys-hack/master/LICENSE

{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE PackageImports #-}
{-# LANGUAGE QuasiQuotes #-}

module Process.CrossThread
  ( toggleCapsLock
  , turnCapsLockMode
  , handleCapsLockModeChange
  , justTurnCapsLockMode
  ) where

import "X11" Graphics.X11.Xlib.Types (Display)

import "transformers" Control.Monad.Trans.Class (lift)
import "lens" Control.Lens ((.~), (%~), (^.), set, over, view, Lens')
import "either" Control.Monad.Trans.Either (EitherT, runEitherT, left, right)

import qualified "containers" Data.Set as Set (null)
import "base" Data.Maybe (fromJust, isJust)

-- local imports

import Utils ((?), (<||>), (&), (.>))
import Utils.String (qm)
import Bindings.XTest (fakeKeyCodeEvent)
import Bindings.MoreXlib (getLeds)
import qualified State
import qualified Keys


type State  = State.State
type Noiser = [String] -> IO ()
type KeyMap = Keys.KeyMap


-- Handle delayed Caps Lock mode change
handleCapsLockModeChange :: Display -> Noiser -> KeyMap -> State -> IO State
handleCapsLockModeChange dpy noise' keyMap state =
  fmap (either id id) $ runEitherT $ do

  let log = lift $ noise [qm| Delayed Caps Lock mode turning {onOrOff isToOn}
                            \ after all other keys release is skipped
                            \ because it's already done now |]
   in if hasDelayed && isAlreadyDone
         then log >> left (clearDelayed state) -- Nothing to do
         else right ()

  -- Do nothing if Caps Lock mode changing is not requested
  -- or if all another keys isn't released yet.
  (not hasDelayed || not allIsReleased ? left $ right) state

  let log = noise [qm| Delayed Caps Lock mode turning {onOrOff isToOn}
                     \ after all other keys release
                     \ (by pressing and releasing {show keyName})... |]
      f = fakeKeyCodeEvent dpy keyCode
   in lift $ log >> f True >> f False

  return $ clearDelayed state

  where hasDelayed    = isJust   $ state ^. delayedLens    :: Bool
        isToOn        = fromJust $ state ^. delayedLens    :: Bool
        isCurOn       = state ^. capsLedLens               :: Bool
        isAlreadyDone = isToOn == isCurOn                  :: Bool
        allIsReleased = Set.null $ State.pressedKeys state :: Bool

        noise = (: []) .> noise' :: String -> IO ()

        keyName = Keys.RealCapsLockKey
        keyCode = fromJust $ Keys.getRealKeyCodeByName keyMap keyName


toggleCapsLock :: Display -> Noiser -> KeyMap -> State -> IO State
toggleCapsLock dpy noise' keyMap state =

  do
    noise [qm| Toggling Caps Lock mode (turning it {onOrOff isOn}
             \ by pressing and releasing {keyName})... |]

    let f = fakeKeyCodeEvent dpy keyCode
     in f True >> f False

    return $ clearDelayed state

  `or`

  do
    noise' [ [qm| Attempt to toggle Caps Lock mode
                \ (to turn it {onOrOff isOn}
                \ by pressing and releasing {keyName})
                \ while pressed some another keys |]

           , [qm| Storing in state request to turn Caps Lock mode
                \ {onOrOff isOn} after all another keys release... |]
           ]

    return (state & delayedLens .~ Just isOn)

  where or    = orIfSomeKeysPressed state  :: IO State -> IO State -> IO State
        isOn  = not $ state ^. capsLedLens :: Bool
        noise = (: []) .> noise'           :: String -> IO ()

        keyName = Keys.RealCapsLockKey
        keyCode = fromJust $ Keys.getRealKeyCodeByName keyMap keyName


turnCapsLockMode :: Display -> Noiser -> KeyMap -> State -> Bool -> IO State
turnCapsLockMode dpy noise' keyMap state isOn = unlessAlready $

  do
    noise [qm| Turning Caps Lock mode {onOrOff isOn}
             \ (by pressing and releasing {keyName})... |]

    let f = fakeKeyCodeEvent dpy keyCode
     in f True >> f False

    return $ clearDelayed state

  `or`

  do
    noise' [ [qm| Attempt to turn Caps Lock mode {onOrOff isOn}
                \ (by pressing and releasing {keyName})
                \ while pressed some another keys |]

           , [qm| Storing in state request to turn Caps Lock mode
                \ {onOrOff isOn} after all another keys release... |]
           ]

    return (state & delayedLens .~ Just isOn)

  where unlessAlready :: IO State -> IO State
        unlessAlready m =
          let prev    = state ^. capsLedLens
              unlessM = attemptNoise >> return (clearDelayed state)
           in prev /= isOn ? m $ unlessM

        attemptNoise :: IO ()
        attemptNoise =
          noise [qm| Attempt to turn Caps Lock mode {onOrOff isOn},
                   \ it's already done, skipping... |]

        keyName = Keys.RealCapsLockKey
        keyCode = fromJust $ Keys.getRealKeyCodeByName keyMap keyName

        or    = orIfSomeKeysPressed state :: IO State -> IO State -> IO State
        noise = (: []) .> noise'          :: String -> IO ()


-- Turns Caps Lock mode on/off without checking pressed keys
-- but checks for led state.
justTurnCapsLockMode :: Display -> (String -> IO ()) -> KeyMap -> Bool -> IO ()
justTurnCapsLockMode dpy noise keyMap isOn =

  let log = noise [qm| Turning Caps Lock mode {onOrOff isOn}
                     \ (by pressing and releasing {keyName})... |]
      f = fakeKeyCodeEvent dpy keyCode
      toggle = f True >> f False
      -- Sometimes for some reason Caps Lock mode led returns True
      -- at initialization step even if Caps Lock mode is disabled,
      -- let's bang Caps Lock key until it is really disabled.
      recur = do
        toggle
        (view State.capsLockLed' -> isReallyOn) <- getLeds dpy
        isReallyOn /= isOn ? recur $ return ()
   in log >> recur

  `or`

  noise [qm| Attempt to turn Caps Lock mode {onOrOff isOn},
           \ it's already done, skipping... |]

  where keyName = Keys.RealCapsLockKey
        keyCode = fromJust $ Keys.getRealKeyCodeByName keyMap keyName

        or :: IO () -> IO () -> IO ()
        a `or` b = do
          (view State.capsLockLed' -> isOnAlready) <- getLeds dpy
          isOn /= isOnAlready ? a $ b


orIfSomeKeysPressed :: State -> IO State -> IO State -> IO State
orIfSomeKeysPressed state = (Set.null (State.pressedKeys state) ?)

clearDelayed :: State -> State
clearDelayed = delayedLens .~ Nothing

delayedLens :: Lens' State (Maybe Bool)
delayedLens = State.comboState' . State.capsLockModeChange'

capsLedLens :: Lens' State Bool
capsLedLens = State.leds' . State.capsLockLed'

onOrOff :: Bool -> String
onOrOff = "on" <||> "off"
