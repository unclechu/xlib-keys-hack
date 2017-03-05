-- Author: Viacheslav Lotsmanov
-- License: GPLv3 https://raw.githubusercontent.com/unclechu/xlib-keys-hack/master/LICENSE

{-# LANGUAGE PackageImports #-}

module Utils.Sugar (spec) where

import "hspec" Test.Hspec (Spec, describe, it, shouldBe)

-- local imports

import "xlib-keys-hack" Utils.Sugar ((&), (.>), (<&>))

spec :: Spec
spec = do

  describe "Piping operators" $ do

    it "(&) flipped version of apply operator" $ do
      (subtract 6 $ (*2) $ subtract 2 $ 15) `shouldBe` (20 :: Int)
      (15 & subtract 2 & (*2) & subtract 6) `shouldBe` (20 :: Int)

    it "(.>) flipped version of composition operator" $ do
      (subtract 6 .  (*2) .  subtract 2) 15 `shouldBe` (20 :: Int)
      (subtract 2 .> (*2) .> subtract 6) 15 `shouldBe` (20 :: Int)

    it "(<&>) flipped version of 'fmap' operator" $ do
      (subtract 6 <$> (*2) <$> subtract 2 <$> Just 15) `shouldBe` Just (20 :: Int)
      (Just 15 <&> subtract 2 <&> (*2) <&> subtract 6) `shouldBe` Just (20 :: Int)
      ((+15) <$> Nothing) `shouldBe` (Nothing :: Maybe Int)
      (Nothing <&> (+15)) `shouldBe` (Nothing :: Maybe Int)
