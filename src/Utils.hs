-- Author: Viacheslav Lotsmanov
-- License: GPLv3 https://raw.githubusercontent.com/unclechu/xlib-keys-hack/master/LICENSE

{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PackageImports #-}

module Utils
  ( (&), (.>), (<&>) -- pipes
  , (<||>)
  , (?)

  , ifMaybe

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
  , modifyState
  , modifyStateM
  , EitherStateT
  , continueIf
  , continueUnless

  , writeToFd
  ) where

import "X11" Graphics.X11.Xlib (pending)
import "X11" Graphics.X11.Xlib.Types (Display)
import qualified "X11" Graphics.X11.Xlib.Event as XEvent
import "X11" Graphics.X11.Xlib.Display (connectionNumber)

import "base" Control.Concurrent (threadWaitRead)
import qualified "mtl" Control.Monad.State.Class as St (MonadState(get, put, state))
import "either" Control.Monad.Trans.Either (EitherT, left, right)
import "transformers" Control.Monad.Trans.State (StateT)

import "base" System.Posix.Types (Fd(Fd))
import "base" System.IO (hPutStrLn, hPutStr, stderr)
import qualified "base" GHC.IO.Handle as IOHandle

import qualified "template-haskell" Language.Haskell.TH as TH

import "lens" Control.Lens ((.~))
import qualified "lens" Control.Lens.TH as LTH

import "base" Data.Char (toLower)


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


-- Same as 'partial' from 'Control-Monad-Plus'
ifMaybe :: (a -> Bool) -> a -> Maybe a
ifMaybe f x = if f x then Just x else Nothing


-- https://wiki.haskell.org/X_window_programming_in_Haskell
-- A version of nextEvent that does not block in foreign calls.
nextEvent' :: Display -> XEvent.XEventPtr -> IO ()
nextEvent' dpy evPtr =
  pending dpy <&> (/= 0)
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
updateState :: (St.MonadState s m) => ((s, a) -> s) -> a -> m a
updateState f x = St.state $ \s -> (x, f (s, x))

-- Alternative version of `updateState` that call `f` function
-- with two arguments instead of touple.
updateState' :: (St.MonadState s m) => (s -> a -> s) -> a -> m a
updateState' f x = St.state $ \s -> (x, f s x)

-- Monadic version of `updateState`.
updateStateM :: (St.MonadState s m) => ((s, a) -> m s) -> a -> m a
updateStateM fm x = St.get >>= (\s -> fm (s, x)) >>= St.put >> return x

-- Monadic version of `updateState'`.
updateStateM' :: (St.MonadState s m) => (s -> a -> m s) -> a -> m a
updateStateM' fm x = St.get >>= (\s -> fm s x) >>= St.put >> return x

-- Deal just with State and return void
modifyState :: (St.MonadState s m) => (s -> s) -> m ()
modifyState f = St.state $ \s -> ((), f s)

-- Monadic version of `modifyState`
modifyStateM :: (St.MonadState s m) => (s -> m s) -> m ()
modifyStateM fm = St.get >>= fm >>= St.put

-- Simplified alias for combined `EitherT` and `StateT`
type EitherStateT s l m r = EitherT l (StateT s m) r

continueIf :: Monad m => Bool -> EitherT () m ()
continueIf cond = if cond then right () else left ()

continueUnless :: Monad m => Bool -> EitherT () m ()
continueUnless cond = if cond then left () else right ()


writeToFd :: IOHandle.Handle -> String -> IO ()
writeToFd fd chunk = do
  isWritable <- IOHandle.hIsWritable fd
  if isWritable
     then IOHandle.hPutStr fd chunk >> IOHandle.hFlushAll fd
     else writeToFd fd chunk -- try again
