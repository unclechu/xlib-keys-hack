-- Author: Viacheslav Lotsmanov
-- License: GPLv3 https://raw.githubusercontent.com/unclechu/xlib-keys-hack/master/LICENSE

{-# LANGUAGE PackageImports #-}

module Actions
  ( ActionType(..), HasActionType(..)
  , Action(..),     HasAction(..)

  , seqHead
  , noise,        noise'
  , notifyXmobar, notifyXmobar'
  ) where

import "base" Control.Monad (when, unless)
import "base" Control.Concurrent.Chan (writeChan)
import "lens" Control.Lens ((.~), (%~), (^.), set, over, view)

import "base" Data.Maybe (isJust)

-- local imports

import Utils (makeApoClassy)
import qualified Options as O
import qualified State

import Actions.Types ( ActionType(..), HasActionType(..)
                     , Action(..),     HasAction(..)
                     )


-- Takes head from actions sequence and returns it with wrapped tail.
seqHead :: ActionType -> (Action, ActionType)
seqHead (Sequence (x:xs)) = (x, Sequence xs)


-- Checks if verbose mode is enabled and only then adds actions to queue.
noise :: O.Options -> State.CrossThreadVars -> String -> IO ()
noise opts ctVars msg = when (O.verboseMode opts) $
  writeChan (State.actionsChan ctVars) $ Single $ Noise msg

-- Multiple version of `noise`.
noise' :: O.Options -> State.CrossThreadVars -> [String] -> IO ()
noise' opts ctVars msgs = when (O.verboseMode opts) $
  writeChan (State.actionsChan ctVars) $ Sequence $ map Noise msgs


-- Checks if we have xmobar pipe file descriptor
-- and only then adds actions to queue.
notifyXmobar :: O.Options -> State.CrossThreadVars -> String -> IO ()
notifyXmobar opts ctVars msg = when (isJust $ opts ^. O.xmobarPipeFd') $
  writeChan (State.actionsChan ctVars) $ Single $ NotifyXmobar msg

-- Multiple version of `notifyXmobar`.
notifyXmobar' :: O.Options -> State.CrossThreadVars -> [String] -> IO ()
notifyXmobar' opts ctVars msgs = when (isJust $ opts ^. O.xmobarPipeFd') $
  writeChan (State.actionsChan ctVars) $ Sequence $ map NotifyXmobar msgs
