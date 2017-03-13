-- Author: Viacheslav Lotsmanov
-- License: GPLv3 https://raw.githubusercontent.com/unclechu/xlib-keys-hack/master/LICENSE

module Utils.StateMonad
  ( EitherStateT
  , updateState
  , updateState'
  , updateStateM
  , updateStateM'
  , modifyState
  , modifyStateM
  ) where

import "either" Control.Monad.Trans.Either (EitherT)
import "transformers" Control.Monad.Trans.State (StateT)
import qualified "mtl" Control.Monad.State.Class as St
  (MonadState(get, put, state))


-- Simplified alias for combined `EitherT` and `StateT`
type EitherStateT s l m r = EitherT l (StateT s m) r


-- Updates a state and gets value back.
updateState :: (St.MonadState s m) => ((s, a) -> s) -> a -> m a
updateState f x = St.state $ \s -> (x, f (s, x))

-- Alternative version of `updateState` that call `f` function
-- with two arguments instead of tuple.
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
