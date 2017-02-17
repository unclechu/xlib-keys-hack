-- Author: Viacheslav Lotsmanov
-- License: GPLv3 https://raw.githubusercontent.com/unclechu/xlib-keys-hack/master/LICENSE

{-# LANGUAGE PackageImports #-}

module Main (main) where

import "HUnit" Test.HUnit ( Test(TestList, TestCase)
                          , runTestTT
                          , assertEqual
                          , assertBool
                          )
import "either" Control.Monad.Trans.Either (runEitherT)

-- local imports

import "xlib-keys-hack" Utils.BreakableMonad
  (BreakableMonad(continueIf, continueUnless))


main :: IO ()
main = () <$ runTestTT tests

tests :: Test
tests = TestList [ breakableMonadTests
                 ]


breakableMonadTests :: Test
breakableMonadTests = TestList [ testBreakableMonadDeducingVoidTypeEquality
                               ]

-- It's supposed to fail only at compilation time
testBreakableMonadDeducingVoidTypeEquality :: Test
testBreakableMonadDeducingVoidTypeEquality = TestCase $ do
  either (const ()) (const ()) <$> runEitherT (continueIf True)
  either (const ()) (const ()) <$> runEitherT (continueIf False)
  either (const ()) (const ()) <$> runEitherT (continueUnless True)
  either (const ()) (const ()) <$> runEitherT (continueUnless False)
