-- Author: Viacheslav Lotsmanov
-- License: GPLv3 https://raw.githubusercontent.com/unclechu/xlib-keys-hack/master/LICENSE

{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Bindings.MoreXlib
  ( getLeds
  , initThreads
  ) where


import Foreign
import Foreign.Storable as Storable
import qualified Foreign.C.Types as CTypes
import qualified Foreign.Marshal.Alloc as MAlloc

import Graphics.X11.Xlib.Types (Display(Display))

import Control.Monad (when, unless)

import Data.Bits ((.&.))

import Utils ((&), (.>), dieWith)
import State (LedModes(..))


#include <X11/Xlib.h>
#define  __XKEYBOARDSTATE_AUTO_REPEATS_S  sizeof(((XKeyboardState*)0)->auto_repeats)


data XKeyboardState =
  XKeyboardState { key_click_percent  :: CTypes.CInt
                 , bell_percent       :: CTypes.CInt
                 , bell_pitch         :: CTypes.CUInt
                 , bell_duration      :: CTypes.CUInt
                 , led_mask           :: CTypes.CULong
                 , global_auto_repeat :: CTypes.CInt

                 -- char auto_repeats[32];
                 , auto_repeats       :: [CTypes.CChar]
                 } deriving Show

instance Storable.Storable XKeyboardState where
  sizeOf _ = (#size XKeyboardState)
  alignment = sizeOf
  peek ptr = do
    v_key_click_percent  <- (#peek XKeyboardState, key_click_percent) ptr
    v_bell_percent       <- (#peek XKeyboardState, bell_percent) ptr
    v_bell_pitch         <- (#peek XKeyboardState, bell_pitch) ptr
    v_bell_duration      <- (#peek XKeyboardState, bell_duration) ptr
    v_led_mask           <- (#peek XKeyboardState, led_mask) ptr
    v_global_auto_repeat <- (#peek XKeyboardState, global_auto_repeat) ptr

    v_auto_repeats       <- peekArray (#const __XKEYBOARDSTATE_AUTO_REPEATS_S) $
                                      (#ptr XKeyboardState, auto_repeats) ptr
    return $ XKeyboardState
             v_key_click_percent
             v_bell_percent
             v_bell_pitch
             v_bell_duration
             v_led_mask
             v_global_auto_repeat
             v_auto_repeats
  poke ptr ( XKeyboardState
             v_key_click_percent
             v_bell_percent
             v_bell_pitch
             v_bell_duration
             v_led_mask
             v_global_auto_repeat
             v_auto_repeats
           ) = do
    (#poke XKeyboardState, key_click_percent)  ptr $ v_key_click_percent
    (#poke XKeyboardState, bell_percent)       ptr $ v_bell_percent
    (#poke XKeyboardState, bell_pitch)         ptr $ v_bell_pitch
    (#poke XKeyboardState, bell_duration)      ptr $ v_bell_duration
    (#poke XKeyboardState, led_mask)           ptr $ v_led_mask
    (#poke XKeyboardState, global_auto_repeat) ptr $ v_global_auto_repeat

    pokeArray ((#ptr XKeyboardState, auto_repeats) ptr)
              (take (#const __XKEYBOARDSTATE_AUTO_REPEATS_S)
                    (v_auto_repeats ++ repeat 0))


-- native
foreign import ccall unsafe "X11/Xlib.h XGetKeyboardControl"
  getKeyboardControl :: Display -> Ptr XKeyboardState -> IO ()



getLeds :: Display -> IO LedModes
getLeds dpy = do

  (statePtr :: Ptr XKeyboardState) <- MAlloc.mallocBytes (#size XKeyboardState)
  getKeyboardControl dpy statePtr
  (state :: XKeyboardState) <- peek statePtr
  MAlloc.free statePtr

  let isOn n = led_mask state .&. n & (/= 0)
      in return LedModes { capsLockLed = isOn 1
                         , numLockLed  = isOn 2
                         }


-- native
foreign import ccall unsafe "X11/Xlib.h XInitThreads"
  xInitThreads :: IO CTypes.CInt


initThreads :: IO ()
initThreads =
  xInitThreads
    >>= return . (/= 0)
    >>= flip unless (dieWith "Xlib: init concurrent threads error")
