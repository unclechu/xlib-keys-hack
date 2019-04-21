-- Author: Viacheslav Lotsmanov
-- License: GPLv3 https://raw.githubusercontent.com/unclechu/xlib-keys-hack/master/LICENSE

module Utils.Sugar
  ( (&), (<&>), (.&>), (<$.), (.>)
  , (?), (|?|) -- condition helpers
  , module Data.Maybe.Preserve
  , applyIf, applyUnless
  ) where

import "base" Data.Bool (bool)
import qualified "base" Data.Function as Operators ((&))
import qualified "base" Data.Functor as Operators ((<&>))

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

-- Point-free fmap (left-to-right version).
-- Mix of (.>) and (<&>).
(.&>) :: Functor f => (a -> f b) -> (b -> c) -> a -> f c
f .&> g = f .> fmap g
{-# INLINE (.&>) #-}
infixl 9 .&> -- Precedence of (.>)

-- Point-free fmap.
-- Mix of (<$>) and (.).
(<$.) :: Functor f => (b -> c) -> (a -> f b) -> a -> f c
f <$. g = fmap f . g
{-# INLINE (<$.) #-}
infixr 9 <$. -- Precedence of (.)


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
(?) c x y = if c then x else y
{-# INLINE (?) #-}
infixl 1 ?


applyIf :: (a -> a) -> Bool -> a -> a
applyIf = (|?| id)
{-# INLINE applyIf #-}

applyUnless :: (a -> a) -> Bool -> a -> a
applyUnless = (id |?|)
{-# INLINE applyUnless #-}
