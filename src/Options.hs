-- Author: Viacheslav Lotsmanov
-- License: GPLv3 https://raw.githubusercontent.com/unclechu/xlib-keys-hack/master/LICENSE

{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE PackageImports #-}

module Options
  ( Options(..)
  , HasOptions(..)
  , extractOptions
  , usageInfo
  , noise
  ) where

import "base" GHC.Generics (Generic)
import qualified "base" GHC.IO.Handle as IOHandle
import qualified "base" System.Console.GetOpt as GetOpt

import "lens" Control.Lens ((.~), (%~), (^.), set, over, view)
import "deepseq" Control.DeepSeq (NFData, rnf, deepseq)

import "base" Data.Maybe (fromJust, fromMaybe)
import "data-default" Data.Default (Default, def)

-- local imports

import Utils.Sugar ((&), (.>))
import Utils (makeApoClassy)


data Options =
  Options { showHelp                :: Bool
          , verboseMode             :: Bool

          , realCapsLock            :: Bool
          , additionalControls      :: Bool
          , resetByEscapeOnCapsLock :: Bool
          , resetByWindowFocusEvent :: Bool

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
    showHelp                opts `deepseq`
    verboseMode             opts `deepseq`

    realCapsLock            opts `deepseq`
    additionalControls      opts `deepseq`
    resetByEscapeOnCapsLock opts `deepseq`
    resetByWindowFocusEvent opts `deepseq`

    disableXInputDeviceName opts `deepseq`
    disableXInputDeviceId   opts `deepseq`
    handleDevicePath        opts `deepseq`
    xmobarPipeFile          opts `deepseq`

    handleDeviceFd          opts `seq`
    availableDevices        opts `deepseq`
    availableXInputDevices  opts `deepseq`
    xmobarPipeFd            opts `seq`
      ()

instance Default Options where
  def = Options

    -- From arguments
    { showHelp                = False
    , verboseMode             = False

    , realCapsLock            = False
    , additionalControls      = True
    , resetByEscapeOnCapsLock = True
    , resetByWindowFocusEvent = True

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

makeApoClassy ''Options


options :: [GetOpt.OptDescr (Options -> Options)]
options =
  [ GetOpt.Option ['h'] ["help"]
      (GetOpt.NoArg $ showHelp' .~ True)
      "Show this usage info"
  , GetOpt.Option ['v'] ["verbose"]
      (GetOpt.NoArg $ verboseMode' .~ True)
      "Start in verbose-mode"

  , GetOpt.Option  [ ]  ["real-capslock"]
      (GetOpt.NoArg $ (realCapsLock' .~ True)
                    . (resetByEscapeOnCapsLock' .~ False))
      "Use real Caps Lock instead of remapping it to Escape"
  , GetOpt.Option  [ ]  ["no-additional-controls"]
      (GetOpt.NoArg $ additionalControls' .~ False)
      "Disable additional controls behavior for Caps Lock and Enter keys\
      \ (could be comfortable for playing some video games)"
  , GetOpt.Option  [ ]  ["disable-reset-by-escape-on-capslock"]
      (GetOpt.NoArg $ resetByEscapeOnCapsLock' .~ False)
      "Disable resetting Caps Lock mode, Alternative mode\
      \ and keyboard layout by Escape that triggered by Caps Lock key\
      \ (only when it's remapped, no need to use this option\
      \ if you already used --real-capslock)"
  , GetOpt.Option  [ ]  ["disable-reset-by-window-focus-event"]
      (GetOpt.NoArg $ resetByWindowFocusEvent' .~ False)
      "Disable resetting Caps Lock mode, Alternative mode\
      \ and keyboard layout by switching between windows"

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
      Right (foldl (flip id) def o & handleDevicePath' %~ (++ n))
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
