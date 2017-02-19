-- Author: Viacheslav Lotsmanov
-- License: GPLv3 https://raw.githubusercontent.com/unclechu/xlib-keys-hack/master/LICENSE

{-# LANGUAGE PackageImports #-}

module Utils.StateMonad (spec) where

import "base" Data.Maybe (fromJust)
import "mtl" Control.Monad.State (execState)
import "transformers" Control.Monad.Trans.State (execStateT, put)
import "transformers" Control.Monad.Trans.Class (lift)

import "hspec" Test.Hspec (Spec, describe, it, shouldBe)

-- local imports

import "xlib-keys-hack" Utils.StateMonad ( modifyState, modifyStateM
                                         )


spec :: Spec
spec = do

  describe "modifyState & modifyStateM" $ do

    it "modifyState" $
      let
        v :: Int
        v = flip execState (10 :: Int) $
          do put 20 ; modifyState (+10) ; modifyState (+5)
      in
        v `shouldBe` 35

    it "modifyStateM (monadic version of 'modifyState')" $
      let
        v :: Int
        v = fromJust $ flip execStateT (10 :: Int) $
          do put 20 ; modifyStateM (lift . m) ; modifyStateM (lift . m)

        m :: Int -> Maybe Int
        m x = do _ <- Just True ; _ <- Just False ; Just (x + 2)
      in
        v `shouldBe` 24

    it "modifyState & modifyStateM together" $
      let
        v :: Int
        v = fromJust $ flip execStateT (10 :: Int) $
          do put 20 ; modifyState (+10) ; modifyStateM (lift . m)

        m :: Int -> Maybe Int
        m x = do _ <- Just True ; _ <- Just False ; Just (x + 3)
      in
        v `shouldBe` 33

  -- describe "updateState (for chaining using (>>=) bind operator)" $ do

  --   it "updateState" $
  --     -- TODO
  --     True `shouldBe` True
