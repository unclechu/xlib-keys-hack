-- Author: Viacheslav Lotsmanov
-- License: GPLv3 https://raw.githubusercontent.com/unclechu/xlib-keys-hack/master/LICENSE

{-# LANGUAGE PackageImports #-}

module Main (main) where

import "hspec" Test.Hspec (hspec, describe)

-- local imports

import qualified Utils.BreakableMonad
import qualified Utils.StateMonad


main :: IO ()
main = hspec $ do
  describe "BreakableMonad" Utils.BreakableMonad.spec
  describe "State monad helpers" Utils.StateMonad.spec
