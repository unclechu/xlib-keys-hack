-- Author: Viacheslav Lotsmanov
-- License: GPLv3 https://raw.githubusercontent.com/unclechu/xlib-keys-hack/master/LICENSE

{-# LANGUAGE PackageImports #-}

module Actions
  ( ActionType(..)
  , Action(..),    HasAction(..)
  , KeyAction(..), HasKeyAction(..)

  , seqHead
  , noise,         noise'
  , panicNoise,    panicNoise'
  , notifyXmobar,  notifyXmobar'
  , initTerminate, threadIsDeath, overthrow
  , pressKey,      releaseKey,    pressReleaseKey
  , resetKeyboardLayout
  , turnCapsLock
  ) where

import "X11" Graphics.X11.Xlib (KeyCode)

import "base" Control.Monad (when, unless)
import "base" Control.Concurrent.Chan (writeChan)
import "lens" Control.Lens ((.~), (%~), (^.), set, over, view)

import "base" Data.Maybe (isJust)

-- local imports

import Utils (makeApoClassy)
import qualified Options as O
import qualified State

import Actions.Types ( ActionType(..)
                     , Action(..),    HasAction(..)
                     , KeyAction(..), HasKeyAction(..)
                     )


-- Takes head from actions sequence and returns it with wrapped tail.
seqHead :: ActionType a -> (a, ActionType a)
seqHead (Sequence (x:xs)) = (x, Sequence xs)


-- Checks if verbose mode is enabled and only then adds actions to queue.
noise :: O.Options -> State.CrossThreadVars -> String -> IO ()
noise opts ctVars = when (O.verboseMode opts) .
  writeChan (State.actionsChan ctVars) . Single . Noise

panicNoise :: State.CrossThreadVars -> String -> IO ()
panicNoise ctVars =
  writeChan (State.actionsChan ctVars) . Single . PanicNoise

-- Multiple version of `noise`.
noise' :: O.Options -> State.CrossThreadVars -> [String] -> IO ()
noise' opts ctVars = when (O.verboseMode opts) .
  writeChan (State.actionsChan ctVars) . Sequence . map Noise

panicNoise' :: State.CrossThreadVars -> [String] -> IO ()
panicNoise' ctVars =
  writeChan (State.actionsChan ctVars) . Sequence . map PanicNoise


-- Checks if we have xmobar pipe file descriptor
-- and only then adds actions to queue.
notifyXmobar :: O.Options -> State.CrossThreadVars -> String -> IO ()
notifyXmobar opts ctVars msg = when (isJust $ opts ^. O.xmobarPipeFd') $
  writeChan (State.actionsChan ctVars) $ Single $ NotifyXmobar msg

-- Multiple version of `notifyXmobar`.
notifyXmobar' :: O.Options -> State.CrossThreadVars -> [String] -> IO ()
notifyXmobar' opts ctVars msgs = when (isJust $ opts ^. O.xmobarPipeFd') $
  writeChan (State.actionsChan ctVars) $ Sequence $ map NotifyXmobar msgs


-- Initiates termination process of whole application
initTerminate :: State.CrossThreadVars -> IO ()
initTerminate ctVars =
  writeChan (State.actionsChan ctVars) $ Single InitTerminate

-- Notifies about thread's death
threadIsDeath :: State.CrossThreadVars -> Int -> IO ()
threadIsDeath ctVars =
  writeChan (State.actionsChan ctVars) . Single . ThreadIsDead

-- Kills main thread
overthrow :: State.CrossThreadVars -> IO ()
overthrow ctVars =
  writeChan (State.actionsChan ctVars) $ Single JustDie



-- Keys actions

pressKey :: State.CrossThreadVars -> KeyCode -> IO ()
pressKey ctVars =
  writeChan (State.keysActionsChan ctVars) . Single . KeyCodePress

releaseKey :: State.CrossThreadVars -> KeyCode -> IO ()
releaseKey ctVars =
  writeChan (State.keysActionsChan ctVars) . Single . KeyCodeRelease

pressReleaseKey :: State.CrossThreadVars -> KeyCode -> IO ()
pressReleaseKey ctVars keyCode =
  writeChan (State.keysActionsChan ctVars) $
    Sequence [KeyCodePress keyCode, KeyCodeRelease keyCode]

resetKeyboardLayout :: State.CrossThreadVars -> IO ()
resetKeyboardLayout ctVars =
  writeChan (State.keysActionsChan ctVars) $ Single ResetKeyboardLayout

turnCapsLock :: State.CrossThreadVars -> Bool -> IO ()
turnCapsLock ctVars =
  writeChan (State.keysActionsChan ctVars) . Single . TurnCapsLock
