-- Author: Viacheslav Lotsmanov
-- License: GPLv3 https://raw.githubusercontent.com/unclechu/xlib-keys-hack/master/LICENSE

module Utils.Sugar
  ( (&), (<&>), (.>) -- pipes
  , (?), (|?|)       -- conditions helpers
  , module Data.Maybe.Preserve
  , applyIf, applyUnless
  , dupe
  ) where

import "base" Data.Bool (bool)

import qualified "lens" Control.Lens.Operators as Operators ((&), (<&>))

-- local imports

import Data.Maybe.Preserve
     ( preserve,  preserve'
     , preserveF, preserveF', lazyPreserveF'
     , preserveM, preserveM'
     )


(&) :: a -> (a -> b) -> b
(&) = (Operators.&)
{-# INLINE (&) #-}
infixl 1 &

(<&>) :: Functor f => f a -> (a -> b) -> f b
(<&>) = (Operators.<&>)
{-# INLINE (<&>) #-}
infixr 5 <&>


-- Pipe composition operator.
-- Left-to-right composition instead of right-to-left.
-- Just like bind operator (>>=) for monads but for simple functions.
(.>) :: (a -> b) -> (b -> c) -> a -> c
(.>) = flip (.)
{-# INLINE (.>) #-}
infixl 9 .>


-- Makes function from then-else values that takes an expression.
-- Flipped version of `Data.Bool.bool`.
-- Example:
--   let foo = "Yes" |?| "No"
--    in [foo (2+2 == 4), foo (2+2 == 5)] -- returns: ["Yes", "No"]
(|?|) :: a -> a -> (Bool -> a)
a |?| b = bool b a
{-# INLINE (|?|) #-}
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
{-# INLINE (?) #-}
infixl 1 ?


applyIf :: (a -> a) -> Bool -> a -> a
applyIf = (|?| id)
{-# INLINE applyIf #-}

applyUnless :: (a -> a) -> Bool -> a -> a
applyUnless = (id |?|)
{-# INLINE applyUnless #-}


dupe :: a -> (a, a)
dupe x = (x, x)
{-# INLINE dupe #-}
