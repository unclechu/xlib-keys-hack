-- Author: Viacheslav Lotsmanov
-- License: GPLv3 https://raw.githubusercontent.com/unclechu/xlib-keys-hack/master/LICENSE

{-# LANGUAGE DeriveGeneric, DeriveAnyClass, TemplateHaskell #-}

module State
  ( State (..),            HasState (..)
  , LedModes (..),         HasLedModes (..)
  , ComboState (..),       HasComboState (..)

  , HeldAltForAlternativeModeState (..)
  , HeldAltForAlternativeMode (..)
  , SuperDoublePress (..)

  , CrossThreadVars (..),  HasCrossThreadVars (..)
  ) where

import "base" GHC.Generics (Generic)
import "process" System.Process (ProcessHandle)
import "base" System.IO (Handle)

import "deepseq" Control.DeepSeq (NFData, rnf)
import "base" Control.Concurrent.MVar (MVar)
import "base" Control.Concurrent.Chan (Chan)
import "type-operators" Control.Type.Operator (type ($))

import "base" Data.Word (Word8)
import "data-default" Data.Default (Default, def)
import qualified "containers" Data.Set as Set
import "time" Data.Time.Clock.POSIX (POSIXTime)

-- local imports

import Utils.Lens (makeApoClassy)
import Utils.Instances ()
import Actions.Types (ActionType, Action, KeyAction)
import Keys (KeyName)


data State
   = State
   { pressedKeys     :: Set.Set KeyName
   , leds            :: LedModes
   , kbdLayout       :: Word8
   , alternative     :: Bool -- Alternative mode on/off
   , comboState      :: ComboState
   , isTerminating   :: Bool
   , windowFocusProc :: Maybe (FilePath, ProcessHandle, Handle)
   } deriving (Show, Generic, NFData)

instance Default State where
  def
    = State
    { pressedKeys     = Set.empty
    , leds            = def
    , kbdLayout       = 0
    , alternative     = False
    , comboState      = def
    , isTerminating   = False
    , windowFocusProc = Nothing
    }


data LedModes
   = LedModes
   { capsLockLed :: Bool
   , numLockLed  :: Bool
   } deriving (Show, Eq, Generic, NFData)

instance Default LedModes where
  def
    = LedModes
    { capsLockLed = False
    , numLockLed  = False
    }

data ComboState
   = ComboState
   { appleMediaPressed :: Bool

   -- When Caps Lock works as additional Control
   , isCapsLockUsedWithCombos  :: Bool
   -- For fast-typing cases
   -- (keys pressed before Caps Lock wont go to a combo).
   , keysPressedBeforeCapsLock :: Set.Set KeyName

   -- When Enter works as additional Control
   , isEnterUsedWithCombos  :: Bool
   -- For fast-typing cases
   -- (keys pressed before Enter wont go to a combo).
   , keysPressedBeforeEnter :: Set.Set KeyName

   -- What Just or Nothing indicates:
   --   If Enter key works as additional Control
   --   and it pressed with modifiers like for example
   --   Shift+Enter or Alt+Enter.
   -- About value inside Just:
   --   Modifiers keys Set that was pressed before Enter key
   --   to check if for example modifier key was released
   --   before Enter key and it means that we need to trigger
   --   Enter key before release this modifier.
   , isEnterPressedWithMods :: Maybe $ Set.Set KeyName

   -- Modes that will be changed to specified state
   -- after all currently pressed keys will be released.
   , capsLockModeChange    :: Maybe Bool
   , alternativeModeChange :: Maybe Bool

   -- Is keyboard layout reset deleyed til
   -- all currently pressed keys will be released.
   , resetKbdLayout :: Bool

   , heldAltForAlternativeMode :: Maybe HeldAltForAlternativeModeState
   -- ^ In case @alternativeModeWithAltMod@ option is enabled.
   --
   -- @Just@ when alternative mode have been turned on by pressing Alt key
   -- but that Alt isn't released yet.

   -- TODO add description
   , superDoublePress :: Maybe (KeyName, SuperDoublePress, POSIXTime)
   -- Using it to prevent infinite recursion.
   , superDoublePressProceeded :: Bool

   } deriving (Show, Eq, Generic, NFData)

instance Default ComboState where
  def
    = ComboState
    { appleMediaPressed         = False

    , isCapsLockUsedWithCombos  = False
    , keysPressedBeforeCapsLock = Set.empty

    , isEnterUsedWithCombos     = False
    , keysPressedBeforeEnter    = Set.empty

    , isEnterPressedWithMods    = Nothing

    , capsLockModeChange        = Nothing
    , alternativeModeChange     = Nothing
    , resetKbdLayout            = False
    , heldAltForAlternativeMode = Nothing
    , superDoublePress          = Nothing
    , superDoublePressProceeded = False
    }


data HeldAltForAlternativeModeState
   = AltIsHeldForAlternativeMode HeldAltForAlternativeMode
   | AltIsReleasedBeforeAlternativeKey
-- ^ Waiting for release all alternative keys before turn alternative mode off
     deriving (Show, Eq, Generic, NFData)

data HeldAltForAlternativeMode
   = HeldLeftAltForAlternativeMode
   | HeldRightAltForAlternativeMode
     deriving (Show, Eq, Generic, NFData)


data SuperDoublePress
   = WaitForFirstRelease
   | WaitForSecondPressAgain
   | WaitForSecondReleaseOrPressAlternativeKey
   | WaitForSecondReleaseAfterAlternativeKeys
   | WaitForReleaseToDisableAlternativeMode
     deriving (Show, Eq, Generic, NFData)


data CrossThreadVars
   = CrossThreadVars
   { stateMVar       :: MVar State
   , actionsChan     :: Chan $ ActionType Action
   , keysActionsChan :: Chan $ ActionType KeyAction
   } deriving Generic

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
