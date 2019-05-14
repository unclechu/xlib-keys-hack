-- Author: Viacheslav Lotsmanov
-- License: GPLv3 https://raw.githubusercontent.com/unclechu/xlib-keys-hack/master/LICENSE

module Process.Keyboard.RawDeviceHandling
     ( getNextKeyboardDeviceKeyEvent
     ) where

import "base" GHC.IO.Handle (type Handle)

import "base" Data.Function (fix)

import "linux-evdev" System.Linux.Input.Event
                   ( Key (Key)
                   , KeyEventType (Depressed, Released)
                   , Event (KeyEvent, evKeyCode, evKeyEventType)
                   , hReadEvent
                   )

-- local imports

import Keys (type KeyMap, getAliasByKeyDevNum)
import Process.Keyboard.Types (toOrderedKey, HandledKey (HandledKey))


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
getNextKeyboardDeviceKeyEvent keyMap fd =
  let unwrapKey (Key x) = x in fix $ \again ->

  hReadEvent fd >>= \case
    Just KeyEvent
           { evKeyCode =
               k@( unwrapKey
                -> getAliasByKeyDevNum keyMap
                -> Just (!name, _, !code)
                 )
           , evKeyEventType =
               (isKeyPressed -> Just !isPressed)
           }
             -> pure $! HandledKey (toOrderedKey k) name code isPressed

    _ -> again



-- | Checks whether a key in "KeyEventType" is pressed or released.
--
-- Returns @Nothing@ if it's neither pressed nor released event type
-- (it could be holding a key event).
isKeyPressed :: KeyEventType -> Maybe Bool
isKeyPressed Depressed = Just True
isKeyPressed Released  = Just False
isKeyPressed _         = Nothing
