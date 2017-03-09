-- Author: Viacheslav Lotsmanov
-- License: GPLv3 https://raw.githubusercontent.com/unclechu/xlib-keys-hack/master/LICENSE

{-# LANGUAGE PackageImports #-}

module Utils.Sugar
  ( (&), (.>), (<&>) -- pipes
  , (?), (|?|)       -- conditions helpers

  , ifMaybe
  , applyIf
  , applyUnless
  , dupe
  ) where

import "base" Data.Bool (bool)


-- Pipe operator.
-- Left-to-right call instead of right-to-left.
-- Actually it's part of `Data.Function` from `base` package
-- but only since 4.8 version.
-- http://hackage.haskell.org/package/base-4.9.0.0/docs/Data-Function.html#v:-38-
(&) :: a -> (a -> b) -> b
(&) = flip ($)
infixl 1 &


-- Pipe composition operator.
-- Left-to-right composition instead of right-to-left.
-- Just like bind operator (>>=) for monads but for simple functions.
(.>) :: (a -> b) -> (b -> c) -> a -> c
(.>) = flip (.)
infixl 9 .>


-- Pipe version of `fmap` operator.
(<&>) :: Functor f => f a -> (a -> b) -> f b
(<&>) = flip (<$>)
infixr 5 <&>


-- Makes function from then-else values that takes an expression.
-- Flipped version of `Data.Bool.bool`.
-- Example:
--   let foo = "Yes" |?| "No"
--    in [foo (2+2 == 4), foo (2+2 == 5)] -- returns: ["Yes", "No"]
(|?|) :: a -> a -> (Bool -> a)
a |?| b = bool b a
infixl 2 |?|


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


-- Same as 'partial' from 'Control-Monad-Plus'.
-- Returns given value inside Just only if it passes a predicate.
ifMaybe :: (a -> Bool) -> a -> Maybe a
ifMaybe f x = f x ? Just x $ Nothing


applyIf :: (a -> a) -> Bool -> a -> a
applyIf = (|?| id)

applyUnless :: (a -> a) -> Bool -> a -> a
applyUnless = (id |?|)


dupe :: a -> (a, a)
dupe x = (x, x)
