-- Author: Viacheslav Lotsmanov
-- License: GPLv3 https://raw.githubusercontent.com/unclechu/xlib-keys-hack/master/LICENSE

{-# LANGUAGE PackageImports #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE LambdaCase #-}

module Process.KeysActions
  ( processKeysActions
  ) where

import "base" Control.Concurrent.Chan (readChan)
import "base" Control.Monad (when, unless, forever)

import "base" Data.Maybe (fromJust)

import "X11" Graphics.X11.Xlib (Display, Window)

-- local imports

import Utils (dieWith)
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
import Options (Options)
import Keys (KeyMap)
import qualified Keys
import Bindings.Xkb (xkbSetGroup, xkbGetCurrentLayout)
import State (CrossThreadVars, keysActionsChan)
import qualified State


processKeysActions :: CrossThreadVars -> Options -> KeyMap -> Display -> IO ()
processKeysActions ctVars _ keyMap dpy = forever $ do

  (action :: ActionType KeyAction) <- readChan $ keysActionsChan ctVars

  flip f action $ \case
    KeyCodePress   keyCode -> press   keyCode
    KeyCodeRelease keyCode -> release keyCode
    ResetKeyboardLayout    -> xkbSetGroup dpy 0
                                >>= flip unless (dieWith "xkbSetGroup error")
    TurnCapsLock x -> (== x) . State.capsLockLed <$> getLeds dpy
                        >>= flip unless toggleCapsLock

  where f :: (KeyAction -> IO ()) -> ActionType KeyAction -> IO ()
        f m (Actions.Single a) = m a
        f m (Actions.Sequence []) = return ()
        f m (Actions.seqHead -> (x, xs)) = m x >> f m xs

        press   keyCode = fakeKeyCodeEvent dpy keyCode True
        release keyCode = fakeKeyCodeEvent dpy keyCode False

        toggleCapsLock = press keyCode >> release keyCode
          where keyCode = fromJust $
                  Keys.getRealKeyCodeByName keyMap Keys.RealCapsLockKey
