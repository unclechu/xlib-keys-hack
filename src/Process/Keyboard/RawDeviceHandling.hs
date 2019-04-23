-- Author: Viacheslav Lotsmanov
-- License: GPLv3 https://raw.githubusercontent.com/unclechu/xlib-keys-hack/master/LICENSE

module Process.Keyboard.RawDeviceHandling
     ( getNextKeyboardDeviceKeyEvent
     ) where

import "base" GHC.IO.Handle (type Handle)

import "base" Data.Function (fix)

import qualified "linux-evdev" System.Linux.Input.Event as EvdevEvent

-- local imports

import           Keys (type KeyMap)
import qualified Keys
import           Process.Keyboard.Types (toOrderedKey, HandledKey (HandledKey))


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
             -> let !ev = HandledKey (toOrderedKey k) name code isPressed
                 in pure ev
    _ -> again



-- | Checks whether a key in "KeyEventType" is pressed or released.
--
-- Returns @Nothing@ if it's neither pressed nor released event type
-- (it could be holding a key event).
isKeyPressed :: EvdevEvent.KeyEventType -> Maybe Bool
isKeyPressed EvdevEvent.Depressed = Just True
isKeyPressed EvdevEvent.Released  = Just False
isKeyPressed _                    = Nothing
