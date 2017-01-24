-- Author: Viacheslav Lotsmanov
-- License: GPLv3 https://raw.githubusercontent.com/unclechu/xlib-keys-hack/master/LICENSE

{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE PackageImports #-}

module State
  ( State(..),      HasState(..)
  , LedModes(..),   HasLedModes(..)
  , ComboState(..), HasComboState(..)

  , CrossThreadVars(..), HasCrossThreadVars(..)
  ) where

import "base" GHC.Generics (Generic)
import "X11" Graphics.X11.Types (Window)

import "deepseq" Control.DeepSeq (NFData, rnf, deepseq)
import "base" Control.Concurrent.MVar (MVar)
import "base" Control.Concurrent.Chan (Chan)

import "data-default" Data.Default (Default, def)
import qualified "containers" Data.Set as Set

-- local imports

import Utils (makeApoClassy)
import Actions.Types (ActionType, Action, KeyAction)
import Keys (KeyName)


data State =
  State { pressedKeys   :: Set.Set KeyName
        , leds          :: LedModes
        , kbdLayout     :: Int
        , alternative   :: Bool -- Alternative mode on/off
        , comboState    :: ComboState
        , isTerminating :: Bool
        }
  deriving (Show, Eq, Generic)

instance NFData State where
  rnf x =
    pressedKeys   x `deepseq`
    leds          x `deepseq`
    kbdLayout     x `deepseq`
    alternative   x `deepseq`
    comboState    x `deepseq`
    isTerminating x `deepseq`
      ()

instance Default State where
  def = State
    { pressedKeys   = Set.empty
    , leds          = def
    , kbdLayout     = 0
    , alternative   = False
    , comboState    = def
    , isTerminating = False
    }


data LedModes =
  LedModes { capsLockLed :: Bool
           , numLockLed  :: Bool
           }
  deriving (Show, Eq, Generic)

instance NFData LedModes where
  rnf x =
    capsLockLed x `deepseq`
    numLockLed  x `deepseq`
      ()

instance Default LedModes where
  def = LedModes
    { capsLockLed = False
    , numLockLed  = False
    }

data ComboState =
  ComboState { appleMediaPressed :: Bool

             -- When Caps Lock works as additional Control
             , isCapsLockUsedWithCombos  :: Bool
             -- For fast-typing cases
             , keysPressedBeforeCapsLock :: Set.Set KeyName

             -- When Enter works as additional Control
             , isEnterUsedWithCombos  :: Bool
             -- For fast-typing cases
             , keysPressedBeforeEnter :: Set.Set KeyName

             -- When Enter wors as additional Control
             -- and pressed with modifiers like for example
             -- Shift+Enter or Alt+Enter.
             , isEnterPressedWithMods :: Bool

             -- Modes that will be changed to specified state
             -- after all currently pressed keys will be released.
             , capsLockModeChange    :: Maybe Bool
             , alternativeModeChange :: Maybe Bool

             -- Is keyboard layout reset deleyed til
             -- all currently pressed keys will be released.
             , resetKbdLayout :: Bool
             }
  deriving (Show, Eq, Generic)

instance NFData ComboState where
  rnf x =
    appleMediaPressed         x `deepseq`

    isCapsLockUsedWithCombos  x `deepseq`
    keysPressedBeforeCapsLock x `deepseq`

    isEnterUsedWithCombos     x `deepseq`
    keysPressedBeforeEnter    x `deepseq`

    isEnterPressedWithMods    x `deepseq`

    capsLockModeChange        x `deepseq`
    alternativeModeChange     x `deepseq`
    resetKbdLayout            x `deepseq`
      ()

instance Default ComboState where
  def = ComboState
    { appleMediaPressed         = False

    , isCapsLockUsedWithCombos  = False
    , keysPressedBeforeCapsLock = Set.empty

    , isEnterUsedWithCombos     = False
    , keysPressedBeforeEnter    = Set.empty

    , isEnterPressedWithMods    = False

    , capsLockModeChange        = Nothing
    , alternativeModeChange     = Nothing
    , resetKbdLayout            = False
    }


data CrossThreadVars =
  CrossThreadVars { stateMVar       :: MVar State
                  , actionsChan     :: Chan (ActionType Action)
                  , keysActionsChan :: Chan (ActionType KeyAction)
                  }
  deriving (Generic)

instance Show CrossThreadVars where
  show _ = "CrossThreadVars"

instance NFData CrossThreadVars where
  rnf ctVars =
    stateMVar       ctVars `seq`
    actionsChan     ctVars `seq`
    keysActionsChan ctVars `seq`
      ()


makeApoClassy ''State
makeApoClassy ''LedModes
makeApoClassy ''ComboState
makeApoClassy ''CrossThreadVars
