-- Author: Viacheslav Lotsmanov
-- License: GPLv3 https://raw.githubusercontent.com/unclechu/xlib-keys-hack/master/LICENSE

{-# LANGUAGE MultiParamTypeClasses #-}

module Utils
  ( (&), (.>), (<&>) -- pipes
  , (<||>)
  , (?)

  , nextEvent'

  , errPutStrLn
  , errPutStr
  , dieWith

  , makeApoLenses
  , makeApoClassy

  , updateState
  , updateState'
  , updateStateM
  , updateStateM'

  , writeToFd

  , BreakableT
  , breakT, breakTWith
  , breakTOn, breakTOnWith
  , continueT, continueTWith
  , runBreakableT, runFromBreakableT
  , fromBreackableT
  ) where

import Graphics.X11.Xlib (pending)
import Graphics.X11.Xlib.Types (Display)
import qualified Graphics.X11.Xlib.Event as XEvent
import Graphics.X11.Xlib.Display (connectionNumber)

import Control.Concurrent (threadWaitRead)
import qualified Control.Monad.State as St (get, put)
import Control.Monad.State.Class (MonadState)
import Control.Monad.Trans.Either (EitherT, runEitherT, left)

import System.Posix.Types (Fd(Fd))
import System.IO (hPutStrLn, hPutStr, stderr)
import qualified GHC.IO.Handle as IOHandle

import qualified Language.Haskell.TH as TH

import Control.Lens ((.~))
import qualified Control.Lens.TH as LTH
import Control.Applicative ((<$>))

import Data.Either (Either(Left, Right), either)
import Data.Maybe (Maybe(Nothing, Just))
import Data.Char (toLower)


-- Pipe operator.
-- Left-to-right call instead of right-to-left.
-- Actually it's part of `Data.Function` from `base` package
-- but only since 4.8 version.
-- http://hackage.haskell.org/package/base-4.9.0.0/docs/Data-Function.html#v:-38-
(&) :: a -> (a -> b) -> b
(&) = flip ($)
infixl 0 &


-- Pipe composition operator.
-- Left-to-right composition instead of right-to-left.
-- Just like bind operator (>>=) for monads but for simple functions.
(.>) :: (a -> b) -> (b -> c) -> a -> c
(.>) = flip (.)
infixl 9 .>


-- Pipe version of `fmap` operator.
(<&>) :: Functor f => f a -> (a -> b) -> f b
(<&>) = flip (<$>)
infixr 4 <&>


-- Makes function from then-else values that takes an expression.
-- Example:
--   let foo = "Yes" <||> "No"
--    in [foo (2+2 == 4), foo (2+2 == 5)] -- returns: ["Yes", "No"]
(<||>) :: a -> a -> (Bool -> a)
a <||> b = \x -> if x then a else b
infixl 2 <||>


-- If-then-else chain operator.
-- See more: https://wiki.haskell.org/Case
-- Example:
--   isFoo ? "foo" $
--   isBar ? "bar" $
--    "default"
(?) :: Bool -> a -> a -> a
(?) True  x _ = x
(?) False _ y = y
infixl 1 ?


-- https://wiki.haskell.org/X_window_programming_in_Haskell
-- A version of nextEvent that does not block in foreign calls.
nextEvent' :: Display -> XEvent.XEventPtr -> IO ()
nextEvent' dpy evPtr =
  pending dpy
    >>= return . (/= 0)
    >>= XEvent.nextEvent dpy evPtr <||>
        (threadWaitRead (Fd fd) >> nextEvent' dpy evPtr)
  where fd = connectionNumber dpy


errPutStrLn :: String -> IO ()
errPutStrLn = hPutStrLn stderr

errPutStr :: String -> IO ()
errPutStr = hPutStr stderr

dieWith :: String -> IO a
dieWith = ioError . userError


lensNamer :: LTH.FieldNamer
lensNamer = LTH.mappingNamer $ (++ "'") .> (:[])


-- Customization of 'makeLenses'.
-- Original "foo" transforms to "foo'"
-- instead of "_foo" transforms "foo".
makeApoLenses :: TH.Name -> TH.DecsQ
makeApoLenses = LTH.makeLensesWith rules
  where rules :: LTH.LensRules
        rules = LTH.lensRules & LTH.lensField .~ lensNamer


-- Same way as 'makeApoLenses'
-- customization of 'makeClassy'.
makeApoClassy :: TH.Name -> TH.DecsQ
makeApoClassy = LTH.makeLensesWith rules
  where rules :: LTH.LensRules
        rules = LTH.classyRules
              & LTH.lensField .~ lensNamer
              & LTH.lensClass .~ classNamer
        classNamer :: TH.Name -> Maybe (TH.Name, TH.Name)
        classNamer name = let base = TH.nameBase name in
          Just ( TH.mkName $ "Has" ++ base
               , TH.mkName $ [toLower $ head base] ++ tail base ++ "'c"
               )


-- Updates a state and gets value back.
updateState :: (MonadState s m) => ((s, a) -> s) -> a -> m a
updateState f x = St.get >>= (\s -> St.put $ f (s, x)) >> return x

-- Alternative version of `updateState` that call `f` function
-- with two arguments instead of touple.
updateState' :: (MonadState s m) => (s -> a -> s) -> a -> m a
updateState' f x = St.get >>= (\s -> St.put $ f s x) >> return x

-- Monadic version of `updateState`.
updateStateM :: (MonadState s m) => ((s, a) -> m s) -> a -> m a
updateStateM fm x = St.get >>= (\s -> fm (s, x)) >>= St.put >> return x

-- Monadic version of `updateState'`.
updateStateM' :: (MonadState s m) => (s -> a -> m s) -> a -> m a
updateStateM' fm x = St.get >>= (\s -> fm s x) >>= St.put >> return x


writeToFd :: IOHandle.Handle -> String -> IO ()
writeToFd fd chunk = do
  isWritable <- IOHandle.hIsWritable fd
  if isWritable
     then IOHandle.hPutStr fd chunk >> IOHandle.hFlushAll fd
     else writeToFd fd chunk -- try again



-- Breakable
-- TODO separate it to own module

type BreakableT m a = EitherT a m a

-- Transofmer
runBreakableT :: (Monad m) => BreakableT m a -> m (Either a a)
runBreakableT = runEitherT

-- If you don't need to know was it braked or not,
-- use this instead of `runBreakableT`.
runFromBreakableT :: (Monad m) => BreakableT m a -> m a
runFromBreakableT m = runEitherT m >>= fromBreackableT

-- Extracts value from Either
fromBreackableT :: (Monad m) => Either a a -> m a
fromBreackableT = return . either id id

-- Break chain returning specified value
breakTWith :: (Monad m) => a -> BreakableT m a
breakTWith = left

-- Break chain when value is void
breakT :: (Monad m) => BreakableT m ()
breakT = left ()

-- Break by condition returning specified value
breakTOnWith :: (Monad m) => Bool -> a -> BreakableT m a
breakTOnWith True  = breakTWith
breakTOnWith False = continueTWith

-- Break by condition when value is void
breakTOn :: (Monad m) => Bool -> BreakableT m ()
breakTOn True  = breakT
breakTOn False = continueTWith ()

-- Continue chain (for break-conditions)
continueTWith :: (Monad m) => a -> BreakableT m a
continueTWith = return

-- Continue chain (for break-conditions when value doesn't make any sense).
-- WARNING! Fails when you use it at the end of the do-notation
-- (use `continueTWith` instead to specify value),
-- use it only to keep going to next monad in do-notation.
continueT :: (Monad m) => BreakableT m a
continueT = return undefined
