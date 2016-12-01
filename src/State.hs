-- Author: Viacheslav Lotsmanov
-- License: GPLv3 https://raw.githubusercontent.com/unclechu/xlib-keys-hack/master/LICENSE

{-# LANGUAGE TemplateHaskell #-}

module State
  ( State(..),       HasState(..)
  , PressedKeys(..), HasPressedKeys(..)
  , LedModes(..),    HasLedModes(..)

  , CrossThreadVars(..), HasCrossThreadVars(..)

  , initState
  ) where

import Control.Concurrent.MVar (MVar)
import Control.Concurrent.Chan (Chan)

import Graphics.X11.Types (Window)

import Utils (makeApoClassy)
import Actions.Types (ActionType)


data State =
  State { lastWindow  :: Window
        , pressedKeys :: PressedKeys
        , leds        :: LedModes
        }
  deriving (Show, Eq)


-- Real keys on keyboard even if some of them rebound to another key.
data PressedKeys =
  PressedKeys { caps   :: Bool
              , enter  :: Bool
              , lCtrl  :: Bool
              , rCtrl  :: Bool
              , lAlt   :: Bool
              , rAlt   :: Bool
              , lShift :: Bool
              , rShift :: Bool
              }
  deriving (Show, Eq)


data LedModes =
  LedModes { capsLockLed :: Bool
           , numLockLed  :: Bool
           }
  deriving (Show, Eq)


initState :: Window -> State
initState wnd =
  State { lastWindow  = wnd
        , pressedKeys = defaultPressedKeys
        , leds        = defaultLedModes
        }

defaultPressedKeys :: PressedKeys
defaultPressedKeys =
  PressedKeys { caps   = False
              , enter  = False
              , lCtrl  = False
              , rCtrl  = False
              , lAlt   = False
              , rAlt   = False
              , lShift = False
              , rShift = False
              }

defaultLedModes :: LedModes
defaultLedModes = LedModes { capsLockLed = False
                           , numLockLed  = False
                           }


data CrossThreadVars =
  CrossThreadVars { stateMVar   :: MVar State
                  , actionsChan :: Chan ActionType
                  }

instance Show CrossThreadVars where
  show _ = "CrossThreadVars"


makeApoClassy ''State
makeApoClassy ''PressedKeys
makeApoClassy ''LedModes
makeApoClassy ''CrossThreadVars
