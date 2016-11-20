-- Author: Viacheslav Lotsmanov
-- License: GPLv3 https://raw.githubusercontent.com/unclechu/xlib-keys-hack/master/LICENSE

{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ViewPatterns #-}

module Options
  ( Options(..)
  , HasOptions(..)
  , extractOptions
  , usageInfo
  ) where

import qualified GHC.IO.Handle as IOHandle
import qualified System.Console.GetOpt as GetOpt

import Data.Either (Either(Left, Right))
import Data.Maybe (fromJust)
import Control.Lens ((.~), (%~))

import Utils ((&), (.>), makeApoClassy)


data Options =
  Options { showHelp                :: Bool
          , verboseMode             :: Bool
          , disableXInputDeviceName :: [String]
          , disableXInputDeviceId   :: [Int]
          , handleDevicePath        :: [FilePath]

          , handleDeviceFd          :: [IOHandle.Handle]
          , availableDevices        :: [FilePath]
          }
  deriving Show

makeApoClassy ''Options


defaultOptions =
  Options {

          -- from arguments
            showHelp                = False
          , verboseMode             = False
          , disableXInputDeviceName = []
          , disableXInputDeviceId   = []
          , handleDevicePath        = []

          -- Will be extracted from `handleDevicePath`
          -- and will be reduced with only available
          -- devices (that connected to pc).
          , handleDeviceFd          = []

          -- Same as `handleDevicePath` but contains only
          -- available devies (that exist in file system).
          -- Will be extracted at initialization step
          -- (as `handleDeviceFd`).
          , availableDevices        = []

          }


options :: [GetOpt.OptDescr (Options -> Options)]
options =
  [ GetOpt.Option ['h'] ["help"]
      (GetOpt.NoArg $ showHelp' .~ True)
      "Show this usage info"
  , GetOpt.Option ['v'] ["verbose"]
      (GetOpt.NoArg $ verboseMode' .~ True)
      "Start in verbose-mode"
  , GetOpt.Option  [ ]  ["disable-xinput-device-name"]
      (GetOpt.OptArg
        (\x -> disableXInputDeviceName' %~ (++ [fromJust x]))
        "NAME")
      "Name of device to disable using xinput"
  , GetOpt.Option  [ ]  ["disable-xinput-device-id"]
      (GetOpt.OptArg
        (\x -> disableXInputDeviceId' %~ (++ [fromJust x & read]))
        "ID")
      "Id of device to disable using xinput"
  , GetOpt.Option  [ ]  ["device-fd-path"]
      (GetOpt.OptArg
        (\x -> handleDevicePath' %~ (++ [fromJust x]))
        "FDPATH")
      "Path to device file descriptor to get events from"
  ]


type ErrorMessage = String
extractOptions :: [String] -> Either ErrorMessage Options
extractOptions argv =
  case GetOpt.getOpt GetOpt.Permute options argv of
    (o, n, []) ->
      Right (foldl (flip id) defaultOptions o & handleDevicePath' %~ (++ n))
    (_, _, errs) ->
      Left $ case concat errs of
                  (reverse -> '\n':xs) -> reverse xs
                  x -> x


usageInfo :: String
usageInfo = '\n' : GetOpt.usageInfo header options
  where header = "Usage: xlib-keys-hack [OPTION...] devices fd paths..."
