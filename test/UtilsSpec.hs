-- Author: Viacheslav Lotsmanov
-- License: GPLv3 https://raw.githubusercontent.com/unclechu/xlib-keys-hack/master/LICENSE

module Main (main) where

import "hspec" Test.Hspec (hspec, describe)

-- local imports

import qualified Utils.StateMonad
import qualified Utils.Sugar
import qualified Utils.Lens


main :: IO ()
main = hspec $ do
  describe "State monad helpers" Utils.StateMonad.spec
  describe "Sugar operators" Utils.Sugar.spec
  describe "Lenses helpers" Utils.Lens.spec
