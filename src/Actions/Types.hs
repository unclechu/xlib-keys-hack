-- Author: Viacheslav Lotsmanov
-- License: GPLv3 https://raw.githubusercontent.com/unclechu/xlib-keys-hack/master/LICENSE

{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE PackageImports #-}

module Actions.Types
  ( ActionType(..), HasActionType(..)
  , Action(..),     HasAction(..)
  ) where

import Utils (makeApoClassy)


data Action = Noise        String
            | PanicNoise   String
            | NotifyXmobar String

            -- Parts of application termination process
            | InitTerminate
            | ThreadIsDead Int
            | JustDie -- Ask main thread to die
              deriving (Show, Eq)


data ActionType = Single    Action
                | Sequence [Action]
                  deriving (Show, Eq)


makeApoClassy ''Action
makeApoClassy ''ActionType
