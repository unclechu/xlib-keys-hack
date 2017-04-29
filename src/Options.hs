-- Author: Viacheslav Lotsmanov
-- License: GPLv3 https://raw.githubusercontent.com/unclechu/xlib-keys-hack/master/LICENSE

{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE QuasiQuotes #-}

module Options
  ( Options(..)
  , HasOptions(..)
  , extractOptions
  , usageInfo
  , noise
  ) where

import "base" GHC.Generics (Generic)
import "base" System.IO (Handle)
import qualified "base" System.Console.GetOpt as GetOpt

import "lens" Control.Lens ((.~), (%~), set)
import "deepseq" Control.DeepSeq (NFData, rnf, deepseq)

import "base" Data.Maybe (fromJust)
import "data-default" Data.Default (Default, def)
import "qm-interpolated-string" Text.InterpolatedString.QM (qm)

-- local imports

import Utils.Instances ()
import Utils.Sugar ((&))
import Utils.Lens (makeApoClassy)


data Options =
  Options { showHelp                     :: Bool
          , verboseMode                  :: Bool

          , realCapsLock                 :: Bool
          , alternativeMode              :: Bool
          , additionalControls           :: Bool
          , resetByEscapeOnCapsLock      :: Bool
          , resetByWindowFocusEvent      :: Bool

          , disableXInputDeviceName      :: [String]
          , disableXInputDeviceId        :: [Int]
          , handleDevicePath             :: [FilePath]

          , xmobarIndicators             :: Bool
          , xmobarIndicatorsObjPath      :: Maybe String
          , xmobarIndicatorsBusName      :: Maybe String
          , xmobarIndicatorsIface        :: Maybe String
          , xmobarIndicatorsFlushObjPath :: Maybe String
          , xmobarIndicatorsFlushIface   :: Maybe String

          , handleDeviceFd               :: [Handle]
          , availableDevices             :: [FilePath]
          , availableXInputDevices       :: [Int]
          }
  deriving (Show, Eq, Generic)

instance NFData Options where
  rnf opts =
    showHelp                     opts `deepseq`
    verboseMode                  opts `deepseq`

    realCapsLock                 opts `deepseq`
    alternativeMode              opts `deepseq`
    additionalControls           opts `deepseq`
    resetByEscapeOnCapsLock      opts `deepseq`
    resetByWindowFocusEvent      opts `deepseq`

    disableXInputDeviceName      opts `deepseq`
    disableXInputDeviceId        opts `deepseq`
    handleDevicePath             opts `deepseq`

    xmobarIndicators             opts `deepseq`
    xmobarIndicatorsObjPath      opts `deepseq`
    xmobarIndicatorsBusName      opts `deepseq`
    xmobarIndicatorsIface        opts `deepseq`
    xmobarIndicatorsFlushObjPath opts `deepseq`
    xmobarIndicatorsFlushIface   opts `deepseq`

    handleDeviceFd               opts `deepseq`
    availableDevices             opts `deepseq`
    availableXInputDevices       opts `deepseq`
      ()

instance Default Options where
  def = Options

    -- From arguments
    { showHelp                     = False
    , verboseMode                  = False

    , realCapsLock                 = False
    , alternativeMode              = True
    , additionalControls           = True
    , resetByEscapeOnCapsLock      = True
    , resetByWindowFocusEvent      = True

    , disableXInputDeviceName      = []
    , disableXInputDeviceId        = []
    , handleDevicePath             = []

    , xmobarIndicators             = False
    , xmobarIndicatorsObjPath      = Nothing
    , xmobarIndicatorsBusName      = Nothing
    , xmobarIndicatorsIface        = Nothing
    , xmobarIndicatorsFlushObjPath = Nothing
    , xmobarIndicatorsFlushIface   = Nothing

    -- Will be extracted from `handleDevicePath`
    -- and will be reduced with only available
    -- devices (that connected to pc).
    , handleDeviceFd               = []

    -- Same as `handleDevicePath` but contains only
    -- available devies (that exist in file system).
    -- Will be extracted at initialization step
    -- (as `handleDeviceFd`).
    , availableDevices             = []

    -- Will be extracted from `disableXInputDeviceName`
    -- and from `disableXInputDeviceId` and filtered
    -- with only available devices.
    , availableXInputDevices       = []
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
  , GetOpt.Option  [ ]  ["no-alternative-mode"]
      (GetOpt.NoArg $ alternativeMode' .~ False)
      "Disable Alternative mode feature"
  , GetOpt.Option  [ ]  ["no-additional-controls"]
      (GetOpt.NoArg $ additionalControls' .~ False)
      "Disable additional controls behavior for Caps Lock and Enter keys\n\
      \(could be comfortable for playing some video games)"
  , GetOpt.Option  [ ]  ["disable-reset-by-escape-on-capslock"]
      (GetOpt.NoArg $ resetByEscapeOnCapsLock' .~ False)
      "Disable resetting Caps Lock mode, Alternative mode\
      \ and keyboard layout by Escape that triggered by Caps Lock key\n\
      \(only when it's remapped, no need to use this option\
      \ if you already used --real-capslock)"
  , GetOpt.Option  [ ]  ["disable-reset-by-window-focus-event"]
      (GetOpt.NoArg $ resetByWindowFocusEvent' .~ False)
      "Disable resetting Caps Lock mode, Alternative mode\
      \ and keyboard layout by switching between windows.\n\
      \WARNING! If you don't disable this feature you should ensure\
      \ that you have directory that contains\
      \ 'xlib-keys-hack-watch-for-window-focus-events'\
      \ executable in your 'PATH' environment variable!"

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

  , GetOpt.Option  [ ]  ["xmobar-indicators"]
      (GetOpt.NoArg $ xmobarIndicators' .~ True)
      [qm| Enable notifying xmobar indicators process about indicators
             \ (num lock, caps lock and alternative mode)
             \ state changes by DBus.\n
           See also https://github.com/unclechu/xmonadrc
         |]
  , GetOpt.Option  [ ]  ["xmobar-indicators-dbus-path"]
      (GetOpt.OptArg (set xmobarIndicatorsObjPath') "PATH")
      [qm| DBus object path for xmobar indicators.\n
           Default is: '/'\n
           This option makes sense only with --xmobar-indicators
         |]
  , GetOpt.Option  [ ]  ["xmobar-indicators-dbus-bus"]
      (GetOpt.OptArg (set xmobarIndicatorsBusName') "BUS")
      [qm| DBus bus name for xmobar indicators.\n
           Default is: 'com.github.unclechu.xmonadrc.%DISPLAY%' where
             \ '%DISPLAY%' is view of $DISPLAY environment variable where
             \ ':' and '.' symbols are replaced to underscore '_',
             \ for example if we have $DISPLAY as ':1' bus name will be
             \ 'com.github.unclechu.xmonadrc._1'\n
           This option makes sense only with --xmobar-indicators\n
           Use --xmobar-indicators-dbus-bus=any to broadcast to everyone.
         |]
  , GetOpt.Option  [ ]  ["xmobar-indicators-dbus-interface"]
      (GetOpt.OptArg (set xmobarIndicatorsIface') "INTERFACE")
      [qm| DBus interface for xmobar indicators.\n
           Default is: 'com.github.unclechu.xmonadrc'\n
           This option makes sense only with --xmobar-indicators
         |]
  , GetOpt.Option  [ ]  ["xmobar-indicators-dbus-flush-path"]
      (GetOpt.OptArg (set xmobarIndicatorsFlushObjPath') "PATH")
      [qm| DBus object path for 'flush' request from xmobar indicators process.
         \nDefault is: '/com/github/unclechu/xmonadrc/%DISPLAY%' where
             \ '%DISPLAY%' is view of $DISPLAY environment variable where
             \ ':' and '.' symbols are replaced to underscore '_',
             \ for example if we have $DISPLAY as ':1' object path will be
             \ '/com/github/unclechu/xmonadrc/_1'\n
           This option makes sense only with --xmobar-indicators
         |]
  , GetOpt.Option  [ ]  ["xmobar-indicators-dbus-flush-interface"]
      (GetOpt.OptArg (set xmobarIndicatorsFlushIface') "INTERFACE")
      [qm| DBus interface for 'flush' request from xmobar indicators process.\n
           Default is: 'com.github.unclechu.xmonadrc'\n
           This option makes sense only with --xmobar-indicators
         |]
  ]


type ErrorMessage = String
extractOptions :: [String] -> Either ErrorMessage Options
extractOptions argv =
  case GetOpt.getOpt GetOpt.Permute options argv of
    (o, n, []) ->
      Right $ foldl (flip id) def o & handleDevicePath' %~ (++ n)
    (_, _, errs) ->
      Left $ case concat errs of
                  (reverse -> '\n':xs) -> reverse xs
                  x -> x


usageInfo :: String
usageInfo = '\n' : GetOpt.usageInfo header options
  where header = "Usage: xlib-keys-hack [OPTION...] DEVICES-FD-PATHS..."


noise :: Options -> String -> IO ()
noise (verboseMode -> True) msg = putStrLn msg
noise _ _ = return ()
