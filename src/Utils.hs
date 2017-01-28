-- Author: Viacheslav Lotsmanov
-- License: GPLv3 https://raw.githubusercontent.com/unclechu/xlib-keys-hack/master/LICENSE

{-# LANGUAGE PackageImports #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Utils
  ( errPutStrLn
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

import qualified "mtl" Control.Monad.State.Class as St
  (MonadState(get, put, state))
import "either" Control.Monad.Trans.Either (EitherT, left, right)
import "transformers" Control.Monad.Trans.State (StateT)
import "transformers" Control.Monad.Trans.Maybe (MaybeT(MaybeT))

import "base" GHC.IO.Handle (hFlushAll)
import "base" System.IO
  (Handle, stderr, hPutStrLn, hPutStr, hIsWritable, hPutStr)

import qualified "template-haskell" Language.Haskell.TH as TH

import "lens" Control.Lens ((.~))
import qualified "lens" Control.Lens.TH as LTH

import "base" Data.Char (toLower)

-- local imports

import Utils.Sugar ((&), (.>), (|?|), (?))


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

class Monad m => BreakableMonad m where
  continueIf     :: Bool -> m ()
  continueUnless :: Bool -> m ()

instance BreakableMonad (Either ()) where
  continueIf     cond = cond ? Right () $ Left  ()
  continueUnless cond = cond ? Left  () $ Right ()
instance Monad t => BreakableMonad (EitherT () t) where
  continueIf     cond = cond ? right () $ left  ()
  continueUnless cond = cond ? left  () $ right ()

instance BreakableMonad Maybe where
  continueIf     cond = cond ? Just () $ Nothing
  continueUnless cond = cond ? Nothing $ Just ()
instance Monad t => BreakableMonad (MaybeT t) where
  continueIf     cond = cond ? return () $ fail undefined
  continueUnless cond = cond ? return () $ fail undefined


writeToFd :: Handle -> String -> IO ()
writeToFd fd chunk = hIsWritable fd >>= write |?| tryAgain
  where write = hPutStr fd chunk >> hFlushAll fd
        tryAgain = writeToFd fd chunk
