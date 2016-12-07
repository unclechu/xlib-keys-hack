-- Author: Viacheslav Lotsmanov
-- License: GPLv3 https://raw.githubusercontent.com/unclechu/xlib-keys-hack/master/LICENSE

{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveGeneric #-}

module State
  ( State(..),      HasState(..)
  , LedModes(..),   HasLedModes(..)
  , ComboState(..), HasComboState(..)

  , CrossThreadVars(..), HasCrossThreadVars(..)

  , initState
  ) where

import GHC.Generics (Generic)
import Graphics.X11.Types (Window)

import Control.DeepSeq (NFData, rnf, deepseq)
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
        , alternative :: Bool -- Alternative mode on/off
        , comboState  :: ComboState
        }
  deriving (Show, Eq, Generic)

instance NFData State where
  rnf x =
    lastWindow  x `seq`
    pressedKeys x `deepseq`
    leds        x `deepseq`
    alternative x `seq`
    comboState  x `deepseq`
      ()


data LedModes =
  LedModes { capsLockLed :: Bool
           , numLockLed  :: Bool
           }
  deriving (Show, Eq, Generic)

instance NFData LedModes where
  rnf x =
    capsLockLed x `seq`
    numLockLed  x `seq`
      ()


data ComboState =
  ComboState { appleMediaPressed :: Bool
             }
  deriving (Show, Eq, Generic)

instance NFData ComboState where
  rnf x =
    appleMediaPressed x `seq`
      ()


initState :: Window -> State
initState wnd =
  State { lastWindow  = wnd
        , pressedKeys = Set.empty
        , leds        = defaultLedModes
        , alternative = False
        , comboState  = defaultComboState
        }

defaultLedModes :: LedModes
defaultLedModes = LedModes { capsLockLed = False
                           , numLockLed  = False
                           }

defaultComboState :: ComboState
defaultComboState =
  ComboState { appleMediaPressed = False
             }


data CrossThreadVars =
  CrossThreadVars { stateMVar   :: MVar State
                  , actionsChan :: Chan ActionType
                  }
  deriving (Generic)

instance Show CrossThreadVars where
  show _ = "CrossThreadVars"

instance NFData CrossThreadVars where
  rnf ctVars =
    stateMVar   ctVars `seq`
    actionsChan ctVars `seq`
      ()


makeApoClassy ''State
makeApoClassy ''LedModes
makeApoClassy ''ComboState
makeApoClassy ''CrossThreadVars
