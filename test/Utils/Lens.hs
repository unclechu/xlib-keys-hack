-- Author: Viacheslav Lotsmanov
-- License: GPLv3 https://raw.githubusercontent.com/unclechu/xlib-keys-hack/master/LICENSE

{-# LANGUAGE PackageImports #-}
{-# LANGUAGE TemplateHaskell #-}

module Utils.Lens (spec) where

import "lens" Control.Lens ((.~), (%~), (&))
import "hspec" Test.Hspec (Spec, describe, it, shouldBe)

-- local imports

import "xlib-keys-hack" Utils.Lens (makeApoLenses, makeApoClassy)


data Foo = Foo { fooFoo :: Int, fooBar :: Int } deriving (Eq, Show)
makeApoLenses ''Foo

data Bar = Bar { barFoo :: Int, barBar :: Int } deriving (Eq, Show)
makeApoClassy ''Bar


spec :: Spec
spec =

  describe "Making lenses templates" $ do

    it "'makeApoLenses' custom namer for lenses using apostrophe as suffix" $ do
      let x = Foo { fooFoo = 10, fooBar = 20 }
      (x & fooFoo' .~ 15) `shouldBe` Foo { fooFoo = 15, fooBar = 20 }
      (x & fooBar' .~ 25) `shouldBe` Foo { fooFoo = 10, fooBar = 25 }
      (x & fooFoo' %~ (+25) & fooBar' %~ (+32))
        `shouldBe` Foo { fooFoo = 35, fooBar = 52 }

    it "'makeApoClassy' custom namer for lenses using apostrophe as suffix" $ do
      let x = Bar { barFoo = 11, barBar = 22 }
          f :: HasBar a => a -> a
          f = barFoo' .~ 12
      f x `shouldBe` Bar { barFoo = 12, barBar = 22 }
      (x & barBar' .~ 23) `shouldBe` Bar { barFoo = 11, barBar = 23 }
      (x & barFoo' %~ (+2) & barBar' %~ (+3))
        `shouldBe` Bar { barFoo = 13, barBar = 25 }
