-- Author: Viacheslav Lotsmanov
-- License: GPLv3 https://raw.githubusercontent.com/unclechu/xlib-keys-hack/master/LICENSE

{-# LANGUAGE PackageImports #-}

module Utils.BreakableMonad (spec) where

import "base" Data.Functor.Identity (Identity, runIdentity)

import "either" Control.Monad.Trans.Either (EitherT, runEitherT)
import "transformers" Control.Monad.Trans.Maybe (MaybeT, runMaybeT)

import "hspec" Test.Hspec (Spec, describe, it, shouldBe)

-- local imports

import "xlib-keys-hack" Utils.BreakableMonad
  (BreakableMonad(continueIf, continueUnless))


spec :: Spec
spec = do

  -- It's supposed to fail only at compilation time
  it "Deducing void type equality" $
    let
      m :: Identity ()
      m = do
        either (const ()) (const ()) <$> runEitherT (continueIf True)
        either (const ()) (const ()) <$> runEitherT (continueIf False)
        either (const ()) (const ()) <$> runEitherT (continueUnless True)
        either (const ()) (const ()) <$> runEitherT (continueUnless False)
    in m `shouldBe` return ()

  describe "Maybe" $ do

    it "Correctly breaks" $
      let
        m1, m2 :: Maybe Int
        m1 = do continueIf     False ; return 10
        m2 = do continueUnless True  ; return 20
      in do
        m1 `shouldBe` Nothing
        m2 `shouldBe` Nothing

    it "Correctly continues" $
      let
        m1, m2 :: Maybe Int
        m1 = do continueIf     True  ; return 10
        m2 = do continueUnless False ; return 20
      in do
        m1 `shouldBe` Just 10
        m2 `shouldBe` Just 20

  describe "MaybeT" $ do

    it "Correctly breaks" $
      let
        m1, m2 :: MaybeT Identity Int
        m1 = do continueIf     False ; return 10
        m2 = do continueUnless True  ; return 20
      in do
        runIdentity (runMaybeT m1) `shouldBe` Nothing
        runIdentity (runMaybeT m2) `shouldBe` Nothing

    it "Correctly continues" $
      let
        m1, m2 :: MaybeT Identity Int
        m1 = do continueIf     True  ; return 10
        m2 = do continueUnless False ; return 20
      in do
        runIdentity (runMaybeT m1) `shouldBe` Just 10
        runIdentity (runMaybeT m2) `shouldBe` Just 20

  describe "Either" $ do

    it "Correctly breaks" $
      let
        m1, m2 :: Either () ()
        m1 = continueIf     False
        m2 = continueUnless True
      in do
        m1 `shouldBe` Left ()
        m2 `shouldBe` Left ()

    it "Correctly continues" $
      let
        m1, m2 :: Either () ()
        m1 = do continueIf     True  ; continueUnless False
        m2 = do continueUnless False ; continueIf True
      in do
        m1 `shouldBe` Right ()
        m2 `shouldBe` Right ()

  describe "EitherT" $ do

    it "Correctly breaks" $
      let
        m1 :: EitherT () Identity ()
        m1 = continueIf     False
        m2 = continueUnless True
      in do
        runIdentity (runEitherT m1) `shouldBe` Left ()
        runIdentity (runEitherT m2) `shouldBe` Left ()

    it "Correctly continues" $
      let
        m1 :: EitherT () Identity ()
        m1 = continueIf     True
        m2 = continueUnless False
      in do
        runIdentity (runEitherT m1) `shouldBe` Right ()
        runIdentity (runEitherT m2) `shouldBe` Right ()
