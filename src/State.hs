-- Author: Viacheslav Lotsmanov
-- License: GPLv3 https://raw.githubusercontent.com/unclechu/xlib-keys-hack/master/LICENSE

{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveGeneric #-}

module State
  ( State(..),    HasState(..)
  , LedModes(..), HasLedModes(..)

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
        }
  deriving (Show, Eq, Generic)

instance NFData State where
  rnf x =
    lastWindow  x `seq`
    pressedKeys x `deepseq`
    leds        x `deepseq`
    alternative x `seq`
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
  ComboState {
             }
  deriving (Show, Eq, Generic)

instance NFData ComboState


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
makeApoClassy ''CrossThreadVars
