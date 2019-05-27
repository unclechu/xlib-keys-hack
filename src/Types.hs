-- Author: Viacheslav Lotsmanov
-- License: GPLv3 https://raw.githubusercontent.com/unclechu/xlib-keys-hack/master/LICENSE

{-# LANGUAGE DeriveGeneric, DeriveAnyClass #-}

module Types
     ( type AlternativeModeState
     , type AlternativeModeLevel (..)
     , numberToAlternativeModeLevel
     ) where

import "base" GHC.Generics (type Generic)

import "data-default" Data.Default (type Default (def))

import "deepseq" Control.DeepSeq (type NFData)


-- | @Bool@ indicates whether alternative mode is turned on permanently (@True@)
--   or temporarily (@False@) by holding a modifier.
type AlternativeModeState = Maybe (AlternativeModeLevel, Bool)


data AlternativeModeLevel
   = FirstAlternativeModeLevel
   | SecondAlternativeModeLevel
     deriving (Show, Eq, Ord, Bounded, Enum, Generic, NFData)

instance Default AlternativeModeLevel where
  def = FirstAlternativeModeLevel

numberToAlternativeModeLevel :: Integral a => a -> Maybe AlternativeModeLevel
numberToAlternativeModeLevel 1 = Just FirstAlternativeModeLevel
numberToAlternativeModeLevel 2 = Just SecondAlternativeModeLevel
numberToAlternativeModeLevel _ = Nothing
