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

import "X11" Graphics.X11.Xlib (Display, Window)

-- local imports

import Bindings.XTest (fakeKeyCodeEvent)
import Actions ( ActionType(Single, Sequence)
               , KeyAction(KeyCodePress, KeyCodeRelease)
               , seqHead
               )
import Options (Options)
import Keys (KeyMap)
import State (CrossThreadVars, keysActionsChan)
import qualified State


processKeysActions :: CrossThreadVars -> Options -> KeyMap -> Display -> Window
                   -> IO ()
processKeysActions ctVars _ _ dpy _ = do

  (action :: ActionType KeyAction) <- readChan $ keysActionsChan ctVars

  flip f action $ \case
    KeyCodePress   keyCode -> fakeKeyCodeEvent dpy keyCode True
    KeyCodeRelease keyCode -> fakeKeyCodeEvent dpy keyCode False

  where f :: (KeyAction -> IO ()) -> ActionType KeyAction -> IO ()
        f m (Actions.Single a) = m a
        f m (Actions.Sequence []) = return ()
        f m (Actions.seqHead -> (x, xs)) = m x >> f m xs
