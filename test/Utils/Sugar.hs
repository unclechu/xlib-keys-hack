-- Author: Viacheslav Lotsmanov
-- License: GPLv3 https://raw.githubusercontent.com/unclechu/xlib-keys-hack/master/LICENSE

module Utils.Sugar (spec) where

import "base" Control.Concurrent.MVar (newEmptyMVar, putMVar, tryTakeMVar)

import "hspec" Test.Hspec (Spec, describe, it, shouldBe)

-- local imports

import "xlib-keys-hack" Utils.Sugar ( (.>), (|?|), (?)
                                    , dupe, applyIf, applyUnless
                                    , preserve,  preserve'
                                    , preserveF, preserveF', lazyPreserveF'
                                    , preserveM, preserveM'
                                    )

spec :: Spec
spec = do

  describe "Piping operators" $

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

    it "`preserve` that returns Just if value passes a predicate" $ do
      preserve (== 10) 10 `shouldBe` (Just 10 :: Maybe Int)
      preserve (== 10) 20 `shouldBe` (Nothing :: Maybe Int)
      preserve (== 20) 20 `shouldBe` (Just 20 :: Maybe Int)

    it "`preserve'` alternative version of `preserve`\
       \ (just Bool instead of predicate)" $ do
      preserve' True  10 `shouldBe` (Just 10 :: Maybe Int)
      preserve' False 20 `shouldBe` (Nothing :: Maybe Int)
      preserve' True  20 `shouldBe` (Just 20 :: Maybe Int)

    it "`preserveF` monadic version of `preserveF`" $ do
      preserveF (== 10) (return 10 :: IO Int) >>= (`shouldBe` Just 10)
      preserveF (== 10) (return 20 :: IO Int) >>= (`shouldBe` Nothing)
      preserveF (== 20) (return 20 :: IO Int) >>= (`shouldBe` Just 20)

    it "`preserveF'` alternative version of `preserveF`\
       \ (just Bool instead of predicate)" $ do
      preserveF' True  (return 10 :: IO Int) >>= (`shouldBe` Just 10)
      preserveF' False (return 20 :: IO Int) >>= (`shouldBe` Nothing)
      preserveF' True  (return 20 :: IO Int) >>= (`shouldBe` Just 20)

    it "`lazyPreserveF'` lazy version of `preserveF'`" $ do
      lazyPreserveF' True  (return 10 :: IO Int) >>= (`shouldBe` Just 10)
      lazyPreserveF' False (return 20 :: IO Int) >>= (`shouldBe` Nothing)
      lazyPreserveF' True  (return 20 :: IO Int) >>= (`shouldBe` Just 20)

    it "`preserveF` executes monad even if predicate is constantly falsy" $ do
      mvar <- newEmptyMVar
      let m = putMVar mvar () >> return (10 :: Int)
      preserveF (const True) m >>= (`shouldBe` Just 10)
      tryTakeMVar mvar >>= (`shouldBe` Just ())
      tryTakeMVar mvar >>= (`shouldBe` Nothing)
      preserveF (const False) m >>= (`shouldBe` Nothing)
      tryTakeMVar mvar >>= (`shouldBe` Just ())
      tryTakeMVar mvar >>= (`shouldBe` Nothing)

    it "`preserveF'` executes monad even if condition is False" $ do
      mvar <- newEmptyMVar
      let m = putMVar mvar () >> return (10 :: Int)
      preserveF' True m >>= (`shouldBe` Just 10)
      tryTakeMVar mvar >>= (`shouldBe` Just ())
      tryTakeMVar mvar >>= (`shouldBe` Nothing)
      preserveF' False m >>= (`shouldBe` Nothing)
      tryTakeMVar mvar >>= (`shouldBe` Just ()) -- Monad was executed

    it "`lazyPreserveF'` executes monad only if condition is True" $ do
      mvar <- newEmptyMVar
      let m = putMVar mvar () >> return (10 :: Int)
      lazyPreserveF' True m >>= (`shouldBe` Just 10)
      tryTakeMVar mvar >>= (`shouldBe` Just ())
      tryTakeMVar mvar >>= (`shouldBe` Nothing)
      lazyPreserveF' False m >>= (`shouldBe` Nothing)
      tryTakeMVar mvar >>= (`shouldBe` Nothing) -- Monad was NOT executed

    it "`preserveM` Maybe to Maybe version of `preserve`" $ do
      preserveM (== 10) (Just 10 :: Maybe Int) `shouldBe` Just 10
      preserveM (== 10) (Just 20 :: Maybe Int) `shouldBe` Nothing
      preserveM (== 20) (Just 20 :: Maybe Int) `shouldBe` Just 20

    it "`preserveM'` alternative version of `preserveM`\
       \ (just Bool instead of predicate)" $ do
      preserveM' True  (Just 10 :: Maybe Int) `shouldBe` Just 10
      preserveM' False (Just 20 :: Maybe Int) `shouldBe` Nothing
      preserveM' True  (Just 20 :: Maybe Int) `shouldBe` Just 20

  describe "Function applying helpers" $ do

    it "`applyIf` applies given function if value passes predicate\
       \ or returns original value" $ do
      applyIf (+5) True 10 `shouldBe` (15 :: Int)
      applyIf (+5) False 10 `shouldBe` (10 :: Int)

    it "`applyUnless` applies given function if value doesn't pass predicate\
       \ or returns original value" $ do
      applyUnless (+5) False 10 `shouldBe` (15 :: Int)
      applyUnless (+5) True 10 `shouldBe` (10 :: Int)
