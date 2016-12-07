-- Author: Viacheslav Lotsmanov
-- License: GPLv3 https://raw.githubusercontent.com/unclechu/xlib-keys-hack/master/LICENSE

{-# LANGUAGE TemplateHaskell #-}

module Actions.Types
  ( ActionType(..), HasActionType(..)
  , Action(..),     HasAction(..)
  ) where

import Utils (makeApoClassy)


data Action = Noise        String
            | NotifyXmobar String
              deriving (Show, Eq)


data ActionType = Single    Action
                | Sequence [Action]
                  deriving (Show, Eq)


makeApoClassy ''Action
makeApoClassy ''ActionType
