-- Author: Viacheslav Lotsmanov
-- License: GPLv3 https://raw.githubusercontent.com/unclechu/xlib-keys-hack/master/LICENSE

{-# LANGUAGE ScopedTypeVariables, DataKinds #-}

module Process.KeysActions
  ( processKeysActions
  ) where

import "base" Data.Proxy (Proxy (Proxy))

import "base" Control.Concurrent.Chan (readChan)
import "base" Control.Monad (unless, forever)

import "X11" Graphics.X11.Types (type KeyCode)
import "X11" Graphics.X11.Xlib (type Display)

-- local imports

import Utils (dieWith)
import Utils.Sugar ((.>))
import Bindings.MoreXlib (getLeds)
import Bindings.XTest (fakeKeyCodeEvent)
import Actions ( ActionType(Single, Sequence)
               , KeyAction ( KeyCodePress
                           , KeyCodeRelease
                           , TurnCapsLock
                           , ResetKeyboardLayout
                           )
               , seqHead
               )
import Bindings.Xkb (xkbSetGroup)
import State (type CrossThreadVars, keysActionsChan)
import qualified State
import Keys (KeyName (CapsLockKey))
import Options (type Options, defaultKeyboardLayout)


processKeysActions
  :: CrossThreadVars
  -> Options
  -> (Proxy 'CapsLockKey, KeyCode)
  -- ^ "KeyCode" of real Caps Lock key (it must be not remapped!)
  -> Display
  -> IO ()

processKeysActions ctVars opts (Proxy, capsLockKeyCode) dpy = forever $ do

  (action :: ActionType KeyAction) <- readChan $ keysActionsChan ctVars

  flip f action $ \case
    KeyCodePress   keyCode -> press   keyCode
    KeyCodeRelease keyCode -> release keyCode

    ResetKeyboardLayout ->
      xkbSetGroup dpy (fromIntegral $ defaultKeyboardLayout opts) >>=
        (`unless` dieWith "xkbSetGroup error")

    TurnCapsLock x ->
      getLeds dpy >>= State.capsLockLed .> (== x) .> (`unless` toggleCapsLock)

  where f :: (KeyAction -> IO ()) -> ActionType KeyAction -> IO ()
        f m (Actions.Single a) = m a
        f _ (Actions.Sequence []) = return () -- TODO NonEmpty
        f m (Actions.seqHead -> (x, xs)) = m x >> f m xs

        press   keyCode = fakeKeyCodeEvent dpy keyCode True
        release keyCode = fakeKeyCodeEvent dpy keyCode False

        toggleCapsLock = press capsLockKeyCode >> release capsLockKeyCode
