-- Author: Viacheslav Lotsmanov
-- License: GPLv3 https://raw.githubusercontent.com/unclechu/xlib-keys-hack/master/LICENSE

module Utils.Sugar (spec) where

import "hspec" Test.Hspec (Spec, describe, it, shouldBe)

-- local imports

import "xlib-keys-hack" Utils.Sugar
     ( (.>), (|?|), (?)
     , applyIf, applyUnless
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

  describe "Function applying helpers" $ do

    it "`applyIf` applies given function if value passes predicate\
       \ or returns original value" $ do
      applyIf (+5) True 10 `shouldBe` (15 :: Int)
      applyIf (+5) False 10 `shouldBe` (10 :: Int)

    it "`applyUnless` applies given function if value doesn't pass predicate\
       \ or returns original value" $ do
      applyUnless (+5) False 10 `shouldBe` (15 :: Int)
      applyUnless (+5) True 10 `shouldBe` (10 :: Int)
