-- Author: Viacheslav Lotsmanov
-- License: GPLv3 https://raw.githubusercontent.com/unclechu/xlib-keys-hack/master/LICENSE

module Process.Keyboard.Types
     ( OrderedKey
     , toOrderedKey
     , HandledKey (HandledKey)
     ) where

import "linux-evdev" System.Linux.Input.Event (Key (Key))
import "X11" Graphics.X11.Types (type KeyCode)

-- local imports

import Keys (type KeyName)


-- | A wrapper around "Key" to add "Ord" instance
--   to make it work with containers such as "Set".
newtype OrderedKey = OrderedKey Key deriving (Eq, Show)

instance Ord OrderedKey where
  OrderedKey (Key a) `compare` OrderedKey (Key b) = a `compare` b

-- | Write-only constructor of "OrderedKey".
toOrderedKey :: Key -> OrderedKey
toOrderedKey = OrderedKey


-- | Parsed key event obtained from raw keyboard device file descriptor.
data HandledKey
   = HandledKey
   ! OrderedKey
   ! KeyName
   ! KeyCode
   ! Bool -- ^ Indicates whether a key is pressed or released
