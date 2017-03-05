-- Author: Viacheslav Lotsmanov
-- License: GPLv3 https://raw.githubusercontent.com/unclechu/xlib-keys-hack/master/LICENSE

{-# LANGUAGE PackageImports #-}

module Utils.StateMonad (spec) where

import "base" Data.Maybe (fromJust)

import "base" Control.Monad ((>=>))
import "mtl" Control.Monad.State (execState)
import "transformers" Control.Monad.Trans.State (execStateT, put, get)
import "transformers" Control.Monad.Trans.Class (lift)

import "hspec" Test.Hspec (Spec, describe, it, shouldBe)

-- local imports

import "xlib-keys-hack" Utils.StateMonad ( modifyState,  modifyStateM
                                         , updateState,  updateState'
                                         , updateStateM, updateStateM'
                                         )

mShouldBe :: (Eq a, Show a) => a -> a -> IO a
mShouldBe x a = a <$ shouldBe a x

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

  describe "updateState (for chaining using (>>=) bind operator)" $ do

    -- Check value equals specific value before and after 'm' monad
    let mShouldKeepValue x m = lift . mShouldBe x >=> m >=> lift . mShouldBe x

    it "updateState" $ (>>= (`shouldBe` 20)) $ flip execStateT (10 :: Int) $
      return "foo"
        >>= mShouldKeepValue "foo" (updateState $ (+10) . fst)
        >>  (get >>= lift . mShouldBe 20)

    it "updateState (modification based on value and state both)" $
      (>>= (`shouldBe` 120)) $ flip execStateT (10 :: Int) $
        return 100
          >>= mShouldKeepValue 100 (updateState $ \(s, a) -> s + a + 10)
          >>  (get >>= lift . mShouldBe 120)

    it "updateState'" $ (>>= (`shouldBe` 20)) $ flip execStateT (10 :: Int) $
      return "foo"
        >>= mShouldKeepValue "foo" (updateState' $ \s _ -> s + 10)
        >>  (get >>= lift . mShouldBe 20)

    it "updateState' (modification based on value and state both)" $
      (>>= (`shouldBe` 120)) $ flip execStateT (10 :: Int) $
        return 100
          >>= mShouldKeepValue 100 (updateState' $ \s a -> s + a + 10)
          >>  (get >>= lift . mShouldBe 120)

    it "updateState & updateState' together" $
      (>>= (`shouldBe` 340)) $ flip execStateT (10 :: Int) $
        return 100
          >>= mShouldKeepValue 100 (updateState $ \(s, a) -> s + a + 10)
          >>= (\x -> x <$ (get >>= lift . mShouldBe 120))
          >>= return . (+ 100)
          >>= mShouldKeepValue 200 (updateState' $ \s a -> s + a + 20)
          >>  (get >>= lift . mShouldBe 340)

    it "updateStateM" $ (>>= (`shouldBe` 260)) $ flip execStateT (10 :: Int) $
      let
        fm (s, a) = do a `shouldBe` 100 ; s `shouldBe` 10
                       let x = s + a + 10 in x `mShouldBe` 120

        fm2 (s, a) = do a `shouldBe` 120 ; s `shouldBe` 120
                        let x = s + a + 20 in x `mShouldBe` 260
      in
        return 100
          >>= mShouldKeepValue 100 (updateStateM $ lift . fm)
          >>  (get >>= lift . mShouldBe 120)
          >>= mShouldKeepValue 120 (updateStateM $ lift . fm2)
          >>  (get >>= lift . mShouldBe 260)


    it "updateStateM'" $ (>>= (`shouldBe` 260)) $ flip execStateT (10 :: Int) $
      let
        fm s a = do lift (a `shouldBe` 100) ; lift (s `shouldBe` 10)
                    let x = s + a + 10 in lift (x `mShouldBe` 120)

        fm2 s a = do lift (a `shouldBe` 120) ; lift (s `shouldBe` 120)
                     let x = s + a + 20 in lift (x `mShouldBe` 260)
      in
        return 100
          >>= mShouldKeepValue 100 (updateStateM' fm)
          >>  (get >>= lift . mShouldBe 120)
          >>= mShouldKeepValue 120 (updateStateM' fm2)
          >>  (get >>= lift . mShouldBe 260)


    it "updateState & updateState' & updateStateM & updateStateM' together" $
      (>>= (`shouldBe` 1080)) $ flip execStateT (10 :: Int) $
        return (120 :: Int)
          >>= mShouldKeepValue 120 (updateState $ \(s, a) -> subtract 2 $ s + a)
          >>  (get >>= lift . mShouldBe 128)
          >>= return . (+ 11)
          >>= mShouldKeepValue 139 (updateState' $ \s a -> subtract 3 $ s + a)
          >>  (get >>= lift . mShouldBe 264)
          >>= return . (+ 12)
          >>= mShouldKeepValue 276
                (updateStateM $ \(s, a) ->
                  let x = subtract 4 (s + a) in lift (x `mShouldBe` 536))
          >>  (get >>= lift . mShouldBe 536)
          >>= return . (+ 13)
          >>= mShouldKeepValue 549
                (updateStateM' $ \s a ->
                  let x = subtract 5 (s + a) in lift (x `mShouldBe` 1080))
          >>  (get >>= lift . mShouldBe 1080)
