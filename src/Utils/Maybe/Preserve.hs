-- Author: Viacheslav Lotsmanov
-- License: GPLv3 https://raw.githubusercontent.com/unclechu/xlib-keys-hack/master/LICENSE

{-# LANGUAGE UnicodeSyntax #-}

module Utils.Maybe.Preserve
     ( preserve,  preserve'
     , preserveF, preserveF', lazyPreserveF'
     , preserveM, preserveM'
     ) where


-- Kinda like a `guard`.
-- Consider this example: `\x → x <$ guard (x ≠ "")`
-- it could be replaced with: `preserve (≠ "")`.
-- Protects value with some condition and returns either value wrapped by `Just`
-- or `Nothing` if value isn't satisfies condition.
preserve ∷ (a → Bool) → a → Maybe a
preserve f x = if f x then Just x else Nothing
{-# INLINE preserve #-}

-- Like `preserve` but instead of predicate function just takes `Bool`.
-- So `preserve (const True)` could be replaced with `preserve' True`.
preserve' ∷ Bool → a → Maybe a
preserve' condition x = if condition then Just x else Nothing
{-# INLINE preserve' #-}

-- Functor version of `preserve`.
-- Consider this example: `preserve (≠ "") <$> getLine`
-- it could be replaced with: `preserveF (≠ "") getLine`.
-- Guards value inside a functor, returns wrapped `Maybe` value.
preserveF ∷ Functor f ⇒ (a → Bool) → f a → f (Maybe a)
preserveF predicate = fmap $ \x → if predicate x then Just x else Nothing
{-# INLINE preserveF #-}

-- Like `preserveF` but instead of predicate function just takes `Bool`.
-- So `preserveF (const True) getLine` could be replaced
-- with `preserveF' True getLine`.
preserveF' ∷ Functor f ⇒ Bool → f a → f (Maybe a)
preserveF' condition = fmap $ if condition then Just else const Nothing
{-# INLINE preserveF' #-}

-- Lazy version of `preserveF'`, so if `condition` is `False` and `f` is
-- a monad it wouldn't be touched at all, so side-effects won't happen.
-- `Applicative` is required here to wrap `Nothing` to `f` with hands off of
-- functor from arguments.
lazyPreserveF' ∷ (Applicative f, Functor f) ⇒ Bool → f a → f (Maybe a)
lazyPreserveF' condition f = if condition then Just <$> f else pure Nothing
{-# INLINE lazyPreserveF' #-}

-- Like `preserve` but guards a value already wrapped with `Maybe`, guards
-- a value inside and fails (returns `Nothing`) if predicate gets `False`.
preserveM ∷ (a → Bool) → Maybe a → Maybe a
preserveM f m = m >>= \x → if f x then Just x else Nothing
{-# INLINE preserveM #-}

-- Like `preserveM` but instead of predicate function just takes `Bool`.
-- So `preserveM (const True)` could be replaced with `preserveM' True`.
preserveM' ∷ Bool → Maybe a → Maybe a
preserveM' condition m = if condition then m else Nothing
{-# INLINE preserveM' #-}
