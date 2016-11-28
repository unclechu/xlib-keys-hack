-- Author: Viacheslav Lotsmanov
-- License: GPLv3 https://raw.githubusercontent.com/unclechu/xlib-keys-hack/master/LICENSE

{-# LANGUAGE TemplateHaskell #-}

module State
  ( State(..), HasState(..)
  , PressedKeys(..), HasPressedKeys(..)

  , MVars(..), HasMVars(..)
  , DebugData(..), HasDebugData(..)

  , initState
  ) where

import Control.Concurrent.MVar (MVar)

import Graphics.X11.Types (Window)

import Utils (makeApoClassy)


initState :: State
initState =
  State { lastWindow  = undefined
        , pressedKeys = PressedKeys { caps   = False
                                    , enter  = False
                                    , lCtrl  = False
                                    , rCtrl  = False
                                    , lAlt   = False
                                    , rAlt   = False
                                    , lShift = False
                                    , rShift = False
                                    }
        , debugFlag = False
        }


data State =
  State { lastWindow  :: Window
        , pressedKeys :: PressedKeys
        , debugFlag   :: Bool
        }
  deriving (Show, Eq)


-- Real keys on keyboard even if some of them rebound to another key.
data PressedKeys =
  PressedKeys { caps   :: Bool
              , enter  :: Bool
              , lCtrl  :: Bool
              , rCtrl  :: Bool
              , lAlt   :: Bool
              , rAlt   :: Bool
              , lShift :: Bool
              , rShift :: Bool
              }
  deriving (Show, Eq)


data DebugData = Noise String
                 deriving Show


data MVars =
  MVars { stateMVar :: MVar State
        , debugMVar :: MVar [DebugData]
        }
instance Show MVars where
  show _ = "MVars"


makeApoClassy ''State
makeApoClassy ''PressedKeys
makeApoClassy ''MVars
makeApoClassy ''DebugData
