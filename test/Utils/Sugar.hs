-- Author: Viacheslav Lotsmanov
-- License: GPLv3 https://raw.githubusercontent.com/unclechu/xlib-keys-hack/master/LICENSE

module Utils.Sugar (spec) where

import "base" Control.Concurrent.MVar (newEmptyMVar, putMVar, tryTakeMVar)

import "hspec" Test.Hspec (Spec, describe, it, shouldBe)

-- local imports

import "xlib-keys-hack" Utils.Sugar ( (.>), (|?|), (?)
                                    , dupe, applyIf, applyUnless
                                    , ifMaybe, ifMaybeM, ifMaybeM'
                                    )

spec :: Spec
spec = do

  describe "Piping operators" $ do

    it "(.>) flipped version of composition operator" $ do
      (subtract 6 .  (*2) .  subtract 2) 15 `shouldBe` (20 :: Int)
      (subtract 2 .> (*2) .> subtract 6) 15 `shouldBe` (20 :: Int)

  describe "Boolean operators" $ do

    it "(|?|) flipped `bool` function (A value if True or B value if False)" $ do
      (10 |?| 20) True `shouldBe` (10 :: Int)
      (10 |?| 20) False `shouldBe` (20 :: Int)

    it "(?) if-condition operator" $ do
      (True ? 10 $ 20) `shouldBe` (10 :: Int)
      (False ? 10 $ 20) `shouldBe` (20 :: Int)

    it "(?) if-condition operator chaining" $
      (False ? 10 $ False ? 20 $ True ? 30 $ 40) `shouldBe` (30 :: Int)

  describe "Tuples" $

    it "`dupe`" $ do
      dupe (10 :: Int) `shouldBe` (10, 10)
      dupe (20 :: Int) `shouldBe` (20, 20)
      dupe (30 :: Int, 40 :: Int) `shouldBe` ((30, 40), (30, 40))

  describe "'Maybe' helpers" $ do

    it "`ifMaybe` that returns Just if value passes a predicate" $ do
      ifMaybe (== 10) 10 `shouldBe` (Just 10 :: Maybe Int)
      ifMaybe (== 10) 20 `shouldBe` (Nothing :: Maybe Int)
      ifMaybe (== 20) 20 `shouldBe` (Just 20 :: Maybe Int)

    it "`ifMaybeM` monadic version of `ifMaybe`" $ do
      ifMaybeM (== 10) (return 10 :: IO Int) >>= (`shouldBe` Just 10)
      ifMaybeM (== 10) (return 20 :: IO Int) >>= (`shouldBe` Nothing)
      ifMaybeM (== 20) (return 20 :: IO Int) >>= (`shouldBe` Just 20)

    it "`ifMaybeM'` alternative version of `ifMaybe`\
       \ (just Bool instead of predicate)" $ do
      ifMaybeM' True  (return 10 :: IO Int) >>= (`shouldBe` Just 10)
      ifMaybeM' False (return 20 :: IO Int) >>= (`shouldBe` Nothing)
      ifMaybeM' True  (return 20 :: IO Int) >>= (`shouldBe` Just 20)

    it "`ifMaybeM` executes monad even if predicate is constantly falsy" $ do
      mvar <- newEmptyMVar
      let m = putMVar mvar () >> return (10 :: Int)
      ifMaybeM (const True) m >>= (`shouldBe` Just 10)
      tryTakeMVar mvar >>= (`shouldBe` Just ())
      tryTakeMVar mvar >>= (`shouldBe` Nothing)
      ifMaybeM (const False) m >>= (`shouldBe` Nothing)
      tryTakeMVar mvar >>= (`shouldBe` Just ())
      tryTakeMVar mvar >>= (`shouldBe` Nothing)

    it "`ifMaybeM'` executes monad only if condition is constantly truly" $ do
      mvar <- newEmptyMVar
      let m = putMVar mvar () >> return (10 :: Int)
      ifMaybeM' True m >>= (`shouldBe` Just 10)
      tryTakeMVar mvar >>= (`shouldBe` Just ())
      tryTakeMVar mvar >>= (`shouldBe` Nothing)
      ifMaybeM' False m >>= (`shouldBe` Nothing)
      tryTakeMVar mvar >>= (`shouldBe` Nothing)

  describe "Function applying helpers" $ do

    it "`applyIf` applies given function if value passes predicate\
       \ or returns original value" $ do
      applyIf (+5) True 10 `shouldBe` (15 :: Int)
      applyIf (+5) False 10 `shouldBe` (10 :: Int)

    it "`applyUnless` applies given function if value doesn't pass predicate\
       \ or returns original value" $ do
      applyUnless (+5) False 10 `shouldBe` (15 :: Int)
      applyUnless (+5) True 10 `shouldBe` (10 :: Int)
