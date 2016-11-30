-- Author: Viacheslav Lotsmanov
-- License: GPLv3 https://raw.githubusercontent.com/unclechu/xlib-keys-hack/master/LICENSE

{-# LANGUAGE TemplateHaskell #-}

module Actions
  ( ActionType(..), HasActionType(..)
  , Action(..),     HasAction(..)

  , seqHead
  , noise
  ) where

import Control.Monad (when, unless)
import Control.Concurrent.Chan (writeChan)

import Utils (makeApoClassy)
import qualified Options as O
import qualified State

import Actions.Types ( ActionType(..), HasActionType(..)
                     , Action(..),     HasAction(..)
                     )


-- Takes head from actions sequence and returns it with wrapped tail.
seqHead :: ActionType -> (Action, ActionType)
seqHead (Sequence (x:xs)) = (x, Sequence xs)

noise :: O.Options -> State.CrossThreadVars -> String -> IO ()
noise opts ctVars msg = when (O.verboseMode opts) $
  writeChan (State.actionsChan ctVars) $ Single $ Noise msg
