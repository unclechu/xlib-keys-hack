-- Author: Viacheslav Lotsmanov
-- License: GPLv3 https://raw.githubusercontent.com/unclechu/xlib-keys-hack/master/LICENSE

{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE PackageImports #-}

module Actions.Types
  ( ActionType(..)
  , Action(..),     HasAction(..)
  , XmobarFlag(..), HasXmobarFlag(..)
  , KeyAction(..),  HasKeyAction(..)
  ) where

import "X11" Graphics.X11.Xlib (KeyCode)

-- local imports

import Utils.Lens (makeApoClassy)


data ActionType a = Single    a
                  | Sequence [a]
                    deriving (Show, Eq)


data Action = Noise        String
            | PanicNoise   String
            | NotifyXmobar XmobarFlag

            -- Parts of application termination process
            | InitTerminate
            | ThreadIsDead Int
            | JustDie -- Ask main thread to die
              deriving (Show, Eq)


data XmobarFlag = XmobarNumLockFlag     Bool
                | XmobarCapsLockFlag    Bool
                | XmobarAlternativeFlag Bool
                  deriving (Show, Eq)


data KeyAction = KeyCodePress   KeyCode
               | KeyCodeRelease KeyCode
               | TurnCapsLock   Bool
               | ResetKeyboardLayout
                 deriving (Show, Eq)


makeApoClassy ''Action
makeApoClassy ''KeyAction
makeApoClassy ''XmobarFlag
