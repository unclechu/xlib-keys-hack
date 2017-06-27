-- Author: Viacheslav Lotsmanov
-- License: GPLv3 https://raw.githubusercontent.com/unclechu/xlib-keys-hack/master/LICENSE

module Utils.Sugar
  ( (&), (<&>), (.>) -- pipes
  , (?), (|?|)       -- conditions helpers

  , ifMaybe, ifMaybeM, ifMaybeM'
  , applyIf, applyUnless
  , dupe
  ) where

import "base" Data.Bool (bool)

import qualified "lens" Control.Lens.Operators as Operators ((&), (<&>))


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


-- Same as 'partial' from 'Control-Monad-Plus'.
-- Returns given value inside Just only if it passes a predicate.
ifMaybe :: (a -> Bool) -> a -> Maybe a
ifMaybe f x = f x ? Just x $ Nothing
{-# INLINE ifMaybe #-}

-- Monadic version of `ifMaybe`
ifMaybeM :: Monad m => (a -> Bool) -> m a -> m (Maybe a)
ifMaybeM f m = m >>= \x -> return $ f x ? Just x $ Nothing
{-# INLINE ifMaybeM #-}

-- Like `ifMaybeM` but instead of predicate uses just Bool,
-- also doesn't executes monad if Bool is False
ifMaybeM' :: Monad m => Bool -> m a -> m (Maybe a)
ifMaybeM' condition m = condition ? (Just <$> m) $ return Nothing
{-# INLINE ifMaybeM' #-}


applyIf :: (a -> a) -> Bool -> a -> a
applyIf = (|?| id)
{-# INLINE applyIf #-}

applyUnless :: (a -> a) -> Bool -> a -> a
applyUnless = (id |?|)
{-# INLINE applyUnless #-}


dupe :: a -> (a, a)
dupe x = (x, x)
{-# INLINE dupe #-}
