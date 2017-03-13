-- Author: Viacheslav Lotsmanov
-- License: GPLv3 https://raw.githubusercontent.com/unclechu/xlib-keys-hack/master/LICENSE

{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE BangPatterns #-}

module Utils.Instances () where

import "base" System.IO (Handle)
import "process" System.Process (ProcessHandle)

import "deepseq" Control.DeepSeq (NFData, rnf)


instance Show ProcessHandle   where show _ = "ProcessHandle"


instance NFData ProcessHandle where rnf !_ = ()
instance NFData Handle        where rnf !_ = ()
