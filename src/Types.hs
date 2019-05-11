-- Author: Viacheslav Lotsmanov
-- License: GPLv3 https://raw.githubusercontent.com/unclechu/xlib-keys-hack/master/LICENSE

{-# LANGUAGE DeriveGeneric, DeriveAnyClass #-}

module Types
     ( type AlternativeModeState
     , AlternativeModeLevel (..)
     ) where

import "base" GHC.Generics (Generic)

import "data-default" Data.Default (Default (def))

import "deepseq" Control.DeepSeq (NFData)


-- | @Bool@ indicates whether alternative mode is turned on permanently (@True@)
--   or temporarily (@False@) by holding a modifier.
type AlternativeModeState = Maybe (AlternativeModeLevel, Bool)


data AlternativeModeLevel = FirstAlternativeModeLevel
                          | SecondAlternativeModeLevel
                            deriving (Show, Eq, Generic, NFData)

instance Default AlternativeModeLevel where
  def = FirstAlternativeModeLevel
