-- Author: Viacheslav Lotsmanov
-- License: GPLv3 https://raw.githubusercontent.com/unclechu/xlib-keys-hack/master/LICENSE

{-# LANGUAGE PackageImports #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}

module Utils.BreakableMonad
  ( BreakableMonad(continueIf, continueUnless)
  ) where

import "either" Control.Monad.Trans.Either (EitherT, left, right)
import "transformers" Control.Monad.Trans.Maybe (MaybeT)

-- local imports

import Utils.Sugar ((?))


class Monad m => BreakableMonad m where
  continueIf     :: Bool -> m ()
  continueUnless :: Bool -> m ()

instance a ~ () => BreakableMonad (Either a) where
  continueIf     cond = cond ? Right () $ Left  ()
  continueUnless cond = cond ? Left  () $ Right ()
-- Type equality to void type constraint
-- helps ghc with deducing void from monad (when it's ambiguous).
instance (Monad t, a ~ ()) => BreakableMonad (EitherT a t) where
  continueIf     cond = cond ? right () $ left  ()
  continueUnless cond = cond ? left  () $ right ()

instance BreakableMonad Maybe where
  continueIf     cond = cond ? Just () $ Nothing
  continueUnless cond = cond ? Nothing $ Just ()
instance Monad t => BreakableMonad (MaybeT t) where
  continueIf     cond = cond ? return () $ fail undefined
  continueUnless cond = cond ? return () $ fail undefined
