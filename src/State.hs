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
import Types (type AlternativeModeState)
import Actions.Types (ActionType, Action, KeyAction)
import Keys (KeyName)


data State
   = State
   { pressedKeys     :: Set.Set KeyName
   , leds            :: LedModes
   , kbdLayout       :: Word8
   , alternative     :: AlternativeModeState
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
    , alternative     = Nothing
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

   -- What Just or Nothing indicates:
   --   If Enter key works as additional Control
   --   and it is pressed with modifiers like for example
   --   Shift+Enter or Alt+Enter.
   -- About value inside Just:
   --   Modifiers keys Set that was pressed before Enter key
   --   to check if for example modifier key was released
   --   before Enter key and it means that we need to trigger
   --   Enter key before release this modifier.
   , isEnterPressedWithMods :: Maybe $ Set.Set KeyName

   , additonalControlState :: Maybe (KeyName, Bool, Set.Set KeyName)
   -- ^ When “additional controls” feature is turned on
   --   and when you hold an “additional control” key.
   --
   -- "Maybe" indicates that an additional control is pressed and held at the
   -- moment.
   --
   -- First value ("KeyName" which is an additional control key) indicates that
   -- such key is currently pressed and is held.
   --
   -- Second "Bool" value indicates that such additional control key is pressed
   -- with combos so it is interpreted as a Control key. It’s always starts with
   -- @False@ when you just press it and only after some further key press (a
   -- combo) it becomes @True@.
   --
   -- Third "Set.Set" of "KeyName"s value indicates a set of keys held before a
   -- press of an additional control (usually it’s a fast-typing accident).
   -- Those keys should not be treated as combos pressed with additional
   -- controls but instead they should act like regular keys, like no additional
   -- controls were pressed with them.
   --
   -- Well, it doesn’t mean that a control key shouldn’t be triggered if you
   -- press some another key as a combo, but it means that it should behave kind
   -- of as on a regular keyboard. So basically it means that if you’re still
   -- holding some key before you press an additional control it should not set
   -- "Bool" value to @True@.
   --
   -- A bit more of clarification about this:
   -- In a fast-typing scenario you might not release some keys in time before
   -- you press an additional control key. Those keys should not trigger
   -- additional control combos. It’s okay to release them at any time. Even if
   -- you release them after you press and release a combo with additional
   -- control key.
   --
   -- If you release an additional control then the whole value becomes
   -- @Nothing@. And it’s okay to forget that "Set.Set" of "KeyName"s even if
   -- you didn’t release yet all those keys because any further releases of them
   -- will be handled as usual after anyway, no special handling required in
   -- this case. The point of that "Set.Set" is that while you’re holding an
   -- additional control those keys should not be associated with additional
   -- control combos (as parts of those combos).

   -- Modes that will be changed to specified state
   -- after all currently pressed keys will be released.
   , capsLockModeChange    :: Maybe Bool
   , alternativeModeChange :: Maybe AlternativeModeState

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

    , isEnterPressedWithMods    = Nothing
    , additonalControlState     = Nothing

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
     -- ^ Waiting for release all alternative keys
     --   before turn alternative mode off.
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
