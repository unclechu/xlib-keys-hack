-- Author: Viacheslav Lotsmanov
-- License: GPLv3 https://raw.githubusercontent.com/unclechu/xlib-keys-hack/master/LICENSE

{-# LANGUAGE TemplateHaskell #-}

module State
  ( State(..),    HasState(..)
  , LedModes(..), HasLedModes(..)

  , CrossThreadVars(..), HasCrossThreadVars(..)

  , initState
  ) where

import Graphics.X11.Types (Window)

import Control.Concurrent.MVar (MVar)
import Control.Concurrent.Chan (Chan)
import qualified Data.Set as Set

import Utils (makeApoClassy)
import Actions.Types (ActionType)
import Keys (KeyName)


data State =
  State { lastWindow  :: Window
        , pressedKeys :: Set.Set KeyName
        , leds        :: LedModes
        , alternative :: Bool
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
        , pressedKeys = Set.empty
        , leds        = defaultLedModes
        , alternative = False
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
makeApoClassy ''LedModes
makeApoClassy ''CrossThreadVars
