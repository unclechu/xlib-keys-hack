-- Author: Viacheslav Lotsmanov
-- License: GPLv3 https://raw.githubusercontent.com/unclechu/xlib-keys-hack/master/LICENSE

{-# LANGUAGE TemplateHaskell #-}

module Keys
  ( KeyCodes(..)
  , HasKeyCodes(..)
  , RealKeyCodes(..)
  , HasRealKeyCodes(..)
  , VirtualKeyCodes(..)
  , HasVirtualKeyCodes(..)
  , getKeyCodes
  , getRealKeyCodes
  ) where

import Graphics.X11.Types (KeyCode)

import Utils (makeApoClassy)


getRealKeyCodes :: RealKeyCodes
getRealKeyCodes =
  RealKeyCodes { capsEscKeyCode    = 66
               , enterCtrlKeyCode  = 36
               , escapeKeyCode     = 9

               , leftAltKeyCode    = 64
               , rightAltKeyCode   = 108

               , leftCtrlKeyCode   = 37
               , rightCtrlKeyCode  = 105

               , leftShiftKeyCode  = 50
               , rightShiftKeyCode = 62

               , leftSuperKeyCode  = 133
               , rightSuperKeyCode = 134
               }


getKeyCodes :: VirtualKeyCodes -> KeyCodes
getKeyCodes x =
  KeyCodes { realKeyCodes    = getRealKeyCodes
           , virtualKeyCodes = x
           }


data KeyCodes =
  KeyCodes { realKeyCodes    :: RealKeyCodes
           , virtualKeyCodes :: VirtualKeyCodes
           }


data RealKeyCodes =
  RealKeyCodes { capsEscKeyCode    :: KeyCode -- caps lock that remapped to escape
               , enterCtrlKeyCode  :: KeyCode -- enter that remapped to right ctrl
               , escapeKeyCode     :: KeyCode

               , leftAltKeyCode    :: KeyCode
               , rightAltKeyCode   :: KeyCode

               , leftCtrlKeyCode   :: KeyCode
               , rightCtrlKeyCode  :: KeyCode

               , leftShiftKeyCode  :: KeyCode
               , rightShiftKeyCode :: KeyCode

               , leftSuperKeyCode  :: KeyCode
               , rightSuperKeyCode :: KeyCode
               }


data VirtualKeyCodes =
  VirtualKeyCodes { -- level3KeyCode   :: KeyCode
                  -- , capsLockKeyCode :: KeyCode
                  }


makeApoClassy ''KeyCodes
makeApoClassy ''RealKeyCodes
makeApoClassy ''VirtualKeyCodes
