-- Author: Viacheslav Lotsmanov
-- License: GPLv3 https://raw.githubusercontent.com/unclechu/xlib-keys-hack/master/LICENSE

{-# LANGUAGE TemplateHaskell #-}

module State
  ( State(..)
  , PressedKeys(..)
  , HasState(..)
  , HasPressedKeys(..)
  , initState
  ) where

import Graphics.X11.Types (Window)

-- import Control.Lens ((.~))

import Utils (makeApoClassy)


initState :: State
initState =
  State { lastWindow  = undefined
        , pressedKeys =
            PressedKeys { caps   = False
                        , enter  = False
                        , lCtrl  = False
                        , rCtrl  = False
                        , lAlt   = False
                        , rAlt   = False
                        , lShift = False
                        , rShift = False
                        }
        }


data State =
  State { lastWindow  :: Window
        , pressedKeys :: PressedKeys
        }


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


makeApoClassy ''State
makeApoClassy ''PressedKeys
