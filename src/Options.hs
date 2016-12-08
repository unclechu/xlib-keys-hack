-- Author: Viacheslav Lotsmanov
-- License: GPLv3 https://raw.githubusercontent.com/unclechu/xlib-keys-hack/master/LICENSE

{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE DeriveGeneric #-}

module Options
  ( Options(..)
  , HasOptions(..)
  , extractOptions
  , usageInfo
  , noise
  ) where

import GHC.Generics (Generic)
import qualified GHC.IO.Handle as IOHandle
import qualified System.Console.GetOpt as GetOpt

import Control.Lens ((.~), (%~), (^.), set, over, view)
import Control.DeepSeq (NFData, rnf, deepseq)

import Data.Either (Either(Left, Right))
import Data.Maybe (Maybe(Just, Nothing), fromJust, fromMaybe)

import Utils ((&), (.>), makeApoClassy)


data Options =
  Options { showHelp                :: Bool
          , verboseMode             :: Bool
          , realCapsLock            :: Bool
          , disableXInputDeviceName :: [String]
          , disableXInputDeviceId   :: [Int]
          , handleDevicePath        :: [FilePath]
          , xmobarPipeFile          :: Maybe FilePath

          , handleDeviceFd          :: [IOHandle.Handle]
          , availableDevices        :: [FilePath]
          , availableXInputDevices  :: [Int]
          , xmobarPipeFd            :: Maybe IOHandle.Handle
          }
  deriving (Show, Eq, Generic)

instance NFData Options where
  rnf opts =
    showHelp                opts `seq`
    verboseMode             opts `seq`
    realCapsLock            opts `seq`
    disableXInputDeviceName opts `deepseq`
    disableXInputDeviceId   opts `deepseq`
    handleDevicePath        opts `deepseq`
    xmobarPipeFile          opts `deepseq`

    handleDeviceFd          opts `seq`
    availableDevices        opts `deepseq`
    availableXInputDevices  opts `deepseq`
    xmobarPipeFd            opts `seq`
      ()

makeApoClassy ''Options


defaultOptions =
  Options {

          -- from arguments
            showHelp                = False
          , verboseMode             = False
          , realCapsLock            = False
          , disableXInputDeviceName = []
          , disableXInputDeviceId   = []
          , handleDevicePath        = []
          , xmobarPipeFile          = Nothing


          -- Will be extracted from `handleDevicePath`
          -- and will be reduced with only available
          -- devices (that connected to pc).
          , handleDeviceFd          = []

          -- Same as `handleDevicePath` but contains only
          -- available devies (that exist in file system).
          -- Will be extracted at initialization step
          -- (as `handleDeviceFd`).
          , availableDevices        = []

          -- Will be extracted from `disableXInputDeviceName`
          -- and from `disableXInputDeviceId` and filtered
          -- with only available devices.
          , availableXInputDevices  = []

          -- Pipe file handler.
          -- Will be extracted at initialization step.
          , xmobarPipeFd            = Nothing

          }


options :: [GetOpt.OptDescr (Options -> Options)]
options =
  [ GetOpt.Option ['h'] ["help"]
      (GetOpt.NoArg $ showHelp' .~ True)
      "Show this usage info"
  , GetOpt.Option ['v'] ["verbose"]
      (GetOpt.NoArg $ verboseMode' .~ True)
      "Start in verbose-mode"
  , GetOpt.Option  [ ]  ["real-capslock"]
      (GetOpt.NoArg $ realCapsLock' .~ True)
      "Use real Caps Lock instead of remapping it to Escape"
  , GetOpt.Option  [ ]  ["disable-xinput-device-name"]
      (GetOpt.OptArg
        (\x -> disableXInputDeviceName' %~ (++ [fromJust x]))
        "NAME")
      "Name of device to disable using 'xinput' tool"
  , GetOpt.Option  [ ]  ["disable-xinput-device-id"]
      (GetOpt.OptArg
        (\x -> disableXInputDeviceId' %~ (++ [fromJust x & read]))
        "ID")
      "Id of device to disable using 'xinput' tool"
  , GetOpt.Option  [ ]  ["device-fd-path"]
      (GetOpt.OptArg
        (\x -> handleDevicePath' %~ (++ [fromJust x]))
        "FDPATH")
      "Path to device file descriptor to get events from"
  , GetOpt.Option  [ ]  ["xmobar-pipe"]
      (GetOpt.OptArg (set xmobarPipeFile') "FILE")
      "Path to pipe file of xmobar to notify it about modes"
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


noise :: Options -> String -> IO ()
noise ((^. verboseMode') -> True) msg = putStrLn msg
noise _ _ = return ()
