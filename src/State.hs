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

  , initState
  ) where

import "base" GHC.Generics (Generic)
import "X11" Graphics.X11.Types (Window)

import "deepseq" Control.DeepSeq (NFData, rnf, deepseq)
import "base" Control.Concurrent.MVar (MVar)
import "base" Control.Concurrent.Chan (Chan)

import "base" Data.Maybe (Maybe(Just, Nothing))
import "data-default" Data.Default (Default, def)
import qualified "containers" Data.Set as Set

-- local imports

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

instance Default State where
  def = State
    { lastWindow  = undefined -- Must be overwritten
    , pressedKeys = Set.empty
    , leds        = def
    , alternative = False
    , comboState  = def
    }


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

instance Default LedModes where
  def = LedModes
    { capsLockLed = False
    , numLockLed  = False
    }

data ComboState =
  ComboState { appleMediaPressed :: Bool

             -- When Caps Lock works as additional Control
             , isCapsLockUsedWithCombos :: Bool

             -- When Enter works as additional Control
             , isEnterUsedWithCombos :: Bool

             -- When Enter wors as additional Control
             -- and pressed with modifiers like for example
             -- Shift+Enter or Alt+Enter.
             , isEnterPressedWithMods :: Bool

             -- CapsLock will be changed to specified state
             -- after all currently pressed keys will be released.
             , capsLockModeChange :: Maybe Bool
             }
  deriving (Show, Eq, Generic)

instance NFData ComboState where
  rnf x =
    appleMediaPressed        x `seq`
    isCapsLockUsedWithCombos x `seq`
    isEnterUsedWithCombos    x `seq`
    isEnterPressedWithMods   x `seq`
    capsLockModeChange       x `deepseq`
      ()

instance Default ComboState where
  def = ComboState
    { appleMediaPressed        = False
    , isCapsLockUsedWithCombos = False
    , isEnterUsedWithCombos    = False
    , isEnterPressedWithMods   = False
    , capsLockModeChange       = Nothing
    }


initState :: Window -> State
initState wnd = def { lastWindow = wnd }


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
