-- Author: Viacheslav Lotsmanov
-- License: GPLv3 https://raw.githubusercontent.com/unclechu/xlib-keys-hack/master/LICENSE

{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE RankNTypes #-}

module Options
  ( Options (..)
  , HasOptions (..)
  , extractOptions
  , usageInfo
  , noise
  , subsDisplay
  ) where

import "base" GHC.Generics (Generic)
import "base" System.IO (Handle)
import qualified "base" System.Console.GetOpt as GetOpt

import "base" Control.Arrow ((&&&))
import "lens" Control.Lens (Lens', (.~), (%~), (^.), set)
import "deepseq" Control.DeepSeq (NFData, rnf, deepseq)

import "base" Data.Monoid ((<>))
import "base" Data.Maybe (fromJust)
import "data-default" Data.Default (Default, def)
import "qm-interpolated-string" Text.InterpolatedString.QM (qm, qms, qmb)
import qualified "text" Data.Text as T (pack, unpack, replace)

-- local imports

import Utils.Instances ()
import Utils.Sugar ((.>), (&), (?))
import Utils.Lens (makeApoClassy)


data Options
  = Options
  { showHelp                     :: Bool
  , verboseMode                  :: Bool

  , realCapsLock                 :: Bool
  , additionalControls           :: Bool
  , shiftNumericKeys             :: Bool
  , rightControlAsRightSuper     :: Bool

  , toggleAlternativeModeByAlts  :: Bool
  , superDoublePress             :: Bool
  , leftSuperDoublePressCmd      :: Maybe String
  , rightSuperDoublePressCmd     :: Maybe String

  , resetByEscapeOnCapsLock      :: Bool
  , resetByWindowFocusEvent      :: Bool

  , disableXInputDeviceName      :: [String]
  , disableXInputDeviceId        :: [Int]
  , handleDevicePath             :: [FilePath]

  , xmobarIndicators             :: Bool
  , xmobarIndicatorsObjPath      :: String
  , xmobarIndicatorsBusName      :: String
  , xmobarIndicatorsIface        :: String
  , xmobarIndicatorsFlushObjPath :: String
  , xmobarIndicatorsFlushIface   :: String

  , externalControl              :: Bool
  , externalControlObjPath       :: String
  , externalControlBusName       :: String
  , externalControlIface         :: String

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
    additionalControls           opts `deepseq`
    shiftNumericKeys             opts `deepseq`
    rightControlAsRightSuper     opts `deepseq`

    toggleAlternativeModeByAlts  opts `deepseq`
    superDoublePress             opts `deepseq`
    leftSuperDoublePressCmd      opts `deepseq`
    rightSuperDoublePressCmd     opts `deepseq`

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

    externalControl              opts `deepseq`
    externalControlObjPath       opts `deepseq`
    externalControlBusName       opts `deepseq`
    externalControlIface         opts `deepseq`

    handleDeviceFd               opts `deepseq`
    availableDevices             opts `deepseq`
    availableXInputDevices       opts `deepseq`

    ()

instance Default Options where
  def
    = Options

    -- From arguments
    { showHelp                     = False
    , verboseMode                  = False

    , realCapsLock                 = False
    , additionalControls           = True
    , shiftNumericKeys             = False
    , rightControlAsRightSuper     = False

    , toggleAlternativeModeByAlts  = True
    , superDoublePress             = True
    , leftSuperDoublePressCmd      = Nothing
    , rightSuperDoublePressCmd     = Nothing

    , resetByEscapeOnCapsLock      = True
    , resetByWindowFocusEvent      = True

    , disableXInputDeviceName      = []
    , disableXInputDeviceId        = []
    , handleDevicePath             = []

    , xmobarIndicators             = False
    , xmobarIndicatorsObjPath      = "/"
    , xmobarIndicatorsBusName      = "com.github.unclechu.xmonadrc.%DISPLAY%"
    , xmobarIndicatorsIface        = "com.github.unclechu.xmonadrc"
    , xmobarIndicatorsFlushObjPath = "/com/github/unclechu/xmonadrc/%DISPLAY%"
    , xmobarIndicatorsFlushIface   = "com.github.unclechu.xmonadrc"

    , externalControl              = False
    , externalControlObjPath       = "/"
    , externalControlBusName       = [qm| com.github.unclechu.
                                          xlib_keys_hack.%DISPLAY% |]
    , externalControlIface         = "com.github.unclechu.xlib_keys_hack"

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
      [qmb| Start in verbose-mode
            Default is: {verboseMode def ? "On" $ "Off"}
            |]

  , GetOpt.Option  [ ]  ["real-capslock"]
      (GetOpt.NoArg $ set realCapsLock'            True
                    . set resetByEscapeOnCapsLock' False)
      [qmb| Use real Caps Lock instead of remapping it to Escape
            Default is: {realCapsLock def ? "On" $ "Off"}
            |]
  , GetOpt.Option  [ ]  ["no-additional-controls"]
      (GetOpt.NoArg $ additionalControls' .~ False)
      [qmb| Disable additional controls behavior for Caps Lock and Enter keys
            (could be comfortable for playing some video games)
            Default is: {additionalControls def ? "On" $ "Off"}
            |]
  , GetOpt.Option  [ ]  ["shift-numeric-keys"]
      (GetOpt.NoArg $ shiftNumericKeys' .~ True)
      [qmb| Shift numeric keys in numbers row one key righter, \
              and move 'minus' key to the left side at '1' key position.
            Could be more consistent for 10-fingers typing.
            Default is: {shiftNumericKeys def ? "On" $ "Off"}
            |]
  , GetOpt.Option  [ ]  ["right-control-as-super"]
      (GetOpt.NoArg $ rightControlAsRightSuper' .~ True)
      [qmb| Remap Right Control as Right Super key.
            Some keyboards doesn't have neither Right Super nor Menu key \
              at the right side, since you can have Control key pressing \
              Enter by "additional controls" feature, this could be a solution.
            Default is: {rightControlAsRightSuper def ? "On" $ "Off"}
            |]

  , GetOpt.Option  [ ]  ["disable-toggling-alternative-mode-by-alts"]
      (GetOpt.NoArg $ toggleAlternativeModeByAlts' .~ False)
      [qmb| Disable toggling alternative mode \
              by pressing Alt keys (Left and Right) both at the same time
            Default is: {toggleAlternativeModeByAlts def ? "On" $ "Off"}
            |]
  , GetOpt.Option  [ ]  [disableSuperDoublePress]
      (GetOpt.NoArg $ superDoublePress' .~ False)
      [qmb| Disable handling of double Super key press.
            Default is: {superDoublePress def ? "On" $ "Off"}
            |]
  , GetOpt.Option  [ ]  ["super-double-press-cmd"]
      (GetOpt.ReqArg
        (\(Just -> x) -> set leftSuperDoublePressCmd'  x
                       . set rightSuperDoublePressCmd' x)
        "COMMAND")
      [qmb| When Super key is pressed twice in short interval \
              alternative mode will be toggled or \
              specified shell command will be spawned.
            {makesNoSense disableSuperDoublePress}
            |]
  , GetOpt.Option  [ ]  ["left-super-double-press-cmd"]
      (GetOpt.ReqArg (Just .> set leftSuperDoublePressCmd') "COMMAND")
      [qmb| Double Left Super key press will spawn specified shell command \
              instead of toggling alternative mode.
            {makesNoSense disableSuperDoublePress}
            |]
  , GetOpt.Option  [ ]  ["right-super-double-press-cmd"]
      (GetOpt.ReqArg (Just .> set rightSuperDoublePressCmd') "COMMAND")
      [qmb| Double Right Super key press will spawn specified shell command \
              instead of toggling alternative mode.
            {makesNoSense disableSuperDoublePress}
            |]

  , GetOpt.Option  [ ]  ["disable-reset-by-escape-on-capslock"]
      (GetOpt.NoArg $ resetByEscapeOnCapsLock' .~ False)
      [qmb| Disable resetting Caps Lock mode, Alternative mode \
              and keyboard layout by Escape that triggered by Caps Lock key
            (only when it's remapped, no need to use this option \
              if you already use --real-capslock)
            Default is: {resetByEscapeOnCapsLock def ? "On" $ "Off"}
            |]
  , GetOpt.Option  [ ]  ["disable-reset-by-window-focus-event"]
      (GetOpt.NoArg $ resetByWindowFocusEvent' .~ False)
      [qmb| Disable resetting Caps Lock mode, Alternative mode \
              and keyboard layout by switching between windows.
            WARNING! If you don't disable this feature you should ensure \
              that you have directory that contains \
              'xlib-keys-hack-watch-for-window-focus-events' \
              executable in your 'PATH' environment variable!
            Default is: {resetByWindowFocusEvent def ? "On" $ "Off"}
            |]

  , GetOpt.Option  [ ]  ["disable-xinput-device-name"]
      (GetOpt.OptArg
        (\x -> disableXInputDeviceName' %~ (<> [fromJust x]))
        "NAME")
      "Name of device to disable using 'xinput' tool"
  , GetOpt.Option  [ ]  ["disable-xinput-device-id"]
      (GetOpt.OptArg
        (\x -> disableXInputDeviceId' %~ (<> [fromJust x & read]))
        "ID")
      "Id of device to disable using 'xinput' tool"
  , GetOpt.Option  [ ]  ["device-fd-path"]
      (GetOpt.OptArg
        (\x -> handleDevicePath' %~ (<> [fromJust x]))
        "FDPATH")
      "Path to device file descriptor to get events from"

  , GetOpt.Option  [ ]  [xmobarIndicatorsOptName]
      (GetOpt.NoArg $ xmobarIndicators' .~ True)
      [qmb| Enable notifying xmobar indicators process about indicators \
              (num lock, caps lock and alternative mode) \
              state changes by DBus.
            See also https://github.com/unclechu/xmonadrc
            See also https://github.com/unclechu/unclechu-i3-status \
              (this one isn't about xmobar but uses same IPC interface)
            Default is: {xmobarIndicators def ? "On" $ "Off"}
            |]
  , GetOpt.Option  [ ]  ["xmobar-indicators-dbus-path"]
      (GetOpt.ReqArg (set xmobarIndicatorsObjPath') "PATH")
      [qmb| DBus object path for xmobar indicators.
            {explainDef xmobarIndicatorsObjPath'}
            {makesSense xmobarIndicatorsOptName}
            |]
  , GetOpt.Option  [ ]  ["xmobar-indicators-dbus-bus"]
      (GetOpt.ReqArg (set xmobarIndicatorsBusName') "BUS")
      [qmb| DBus bus name for xmobar indicators.
            {explainDef xmobarIndicatorsBusName'}
            {makesSense xmobarIndicatorsOptName}
            Use --xmobar-indicators-dbus-bus=any to broadcast for everyone.
            |]
  , GetOpt.Option  [ ]  ["xmobar-indicators-dbus-interface"]
      (GetOpt.ReqArg (set xmobarIndicatorsIface') "INTERFACE")
      [qmb| DBus interface for xmobar indicators.
            {explainDef xmobarIndicatorsIface'}
            {makesSense xmobarIndicatorsOptName}
            |]
  , GetOpt.Option  [ ]  ["xmobar-indicators-dbus-flush-path"]
      (GetOpt.ReqArg (set xmobarIndicatorsFlushObjPath') "PATH")
      [qmb| DBus object path for 'flush' request from xmobar indicators process.
            {explainDef xmobarIndicatorsFlushObjPath'}
            {makesSense xmobarIndicatorsOptName}
            |]
  , GetOpt.Option  [ ]  ["xmobar-indicators-dbus-flush-interface"]
      (GetOpt.ReqArg (set xmobarIndicatorsFlushIface') "INTERFACE")
      [qmb| DBus interface for 'flush' request from xmobar indicators process.
            {explainDef xmobarIndicatorsFlushIface'}
            {makesSense xmobarIndicatorsOptName}
            |]

  , GetOpt.Option  [ ]  [externalControlOptName]
      (GetOpt.NoArg $ externalControl' .~ True)
      [qmb| Enabling handling of external control IPC-commands through DBus.
            Default is: {externalControl def ? "On" $ "Off"}
            |]
  , GetOpt.Option  [ ]  ["external-control-dbus-path"]
      (GetOpt.ReqArg (set externalControlObjPath') "PATH")
      [qmb| DBus object path of external control IPC.
            {explainDef externalControlObjPath'}
            {makesSense externalControlOptName}
            |]
  , GetOpt.Option  [ ]  ["external-control-dbus-bus"]
      (GetOpt.ReqArg (set externalControlBusName') "BUS")
      [qmb| DBus bus name of external control IPC.
            {explainDef externalControlBusName'}
            {makesSense externalControlOptName}
            |]
  , GetOpt.Option  [ ]  ["external-control-dbus-interface"]
      (GetOpt.ReqArg (set externalControlIface') "INTERFACE")
      [qmb| DBus interface of external control IPC.
            {explainDef externalControlIface'}
            {makesSense externalControlOptName}
            |]
  ]

  where explainDef :: Lens' Options String -> String
        explainDef ((def ^.) -> id &&& (not . _hasDpy) -> (d, isPlain))

          | isPlain   = [qmb| Default is: '{d}'
                              {_hint}
                              '%DISPLAY%' will be replaced with {_viewOf}
                              {_forExample d}
                              |]

          | otherwise = [qmb| Default is: '{d}' where '%DISPLAY%' is {_viewOf}
                              {_forExample d}
                              {_hint}
                              |]

        makesSense :: String -> String
        makesSense = ("This option makes sense only with --" <>)

        makesNoSense :: String -> String
        makesNoSense = ("This option makes no sense with --" <>)

        disableSuperDoublePress :: String
        disableSuperDoublePress = "disable-super-double-press"

        xmobarIndicatorsOptName, externalControlOptName :: String
        xmobarIndicatorsOptName = "xmobar-indicators"
        externalControlOptName  = "external-control"

        _forExample :: String -> String
        _forExample ((\x -> _hasDpy x ? x $ "foo.%DISPLAY%.bar") -> d) =
          [qms| For example if we have '$DISPLAY' as '{_demoDpy}'
                '{d}' will be replaced to '{_s d}'. |]

        _viewOf :: String
        _viewOf = [qms| view of '$DISPLAY' environment variable where
                        ':' and '.' symbols are replaced to underscore '_'. |]

        _hint :: String
        _hint = [qms| You can use '%DISPLAY%' in your own value of this
                      option (it will be automatically replaced). |]

        _s = subsDisplay _demoDpy ; _s       :: String -> String
        _hasDpy x = x /= _s x     ; _hasDpy  :: String -> Bool
        _demoDpy = ":0.0"         ; _demoDpy :: String


type ErrorMessage = String
extractOptions :: [String] -> Either ErrorMessage Options
extractOptions argv =
  case GetOpt.getOpt GetOpt.Permute options argv of
    (o, n, []) ->
      Right $ foldl (flip id) def o & handleDevicePath' %~ (<> n)
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


subsDisplay :: String -> String -> String
subsDisplay dpy =
  T.pack .> T.replace (T.pack "%DISPLAY%") (T.pack $ map f dpy) .> T.unpack
  where f ':' = '_'; f '.' = '_'; f x = x
