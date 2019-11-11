-- Author: Viacheslav Lotsmanov
-- License: GPLv3 https://raw.githubusercontent.com/unclechu/xlib-keys-hack/master/LICENSE

{-# LANGUAGE ScopedTypeVariables, RankNTypes, BangPatterns #-}
{-# LANGUAGE DeriveGeneric, DeriveAnyClass, TemplateHaskell #-}

module Options
     ( Options (..)
     , HasOptions (..)
     , ErgonomicMode (..)
     , extractOptions
     , usageInfo
     , noise
     , subsDisplay
     ) where

import "base" GHC.Generics (Generic)

import "base" Data.Maybe (fromJust)
import "data-default" Data.Default (Default, def)
import "base" Data.Word (Word8)
import qualified "text" Data.Text as T (pack, unpack, replace)
import "qm-interpolated-string" Text.InterpolatedString.QM (qm, qms, qmb)
import "time" Data.Time.Clock.POSIX (POSIXTime)

import "base" Control.Arrow ((&&&))
import "base" Control.Applicative ((<|>))
import "lens" Control.Lens (Lens', (.~), (%~), (^.), set)
import "deepseq" Control.DeepSeq (NFData)

import "base" System.IO (Handle)
import qualified "base" System.Console.GetOpt as GetOpt

-- local imports

import Utils.Instances ()
import Utils.Sugar ((.>), (&), (?))
import Utils.Lens (makeApoClassy)


-- ^ Indicates whether Ergonomic (ErgoDox) Mode feature is enabled
data ErgonomicMode
   = NoErgonomicMode
   | ErgonomicMode
   | ErgoDoxErgonomicMode
     deriving (Eq, Show, Generic, NFData)


data Options
   = Options
   { showHelp                     :: Bool
   , verboseMode                  :: Bool

   , realCapsLock                 :: Bool
   , additionalControls           :: Bool
   , shiftNumericKeys             :: Bool
   , shiftHJKLKeys                :: Bool
   , rightControlAsRightSuper     :: Bool

   , alternativeModeWithAltMod    :: Bool
   , toggleAlternativeModeByAlts  :: Bool
   , turnOffFourthRow             :: Bool
   , ergonomicMode                :: ErgonomicMode
   , superDoublePress             :: Bool
   , leftSuperDoublePressCmd      :: Maybe String
   , rightSuperDoublePressCmd     :: Maybe String
   , f24asVerticalBar             :: Bool

   , resetByRealEscape            :: Bool
   , resetByEscapeOnCapsLock      :: Bool
   , resetByWindowFocusEvent      :: Bool

   , debouncerTiming              :: Maybe POSIXTime

   , disableXInputDeviceName      :: [String]
   , disableXInputDeviceId        :: [Word]
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
   , availableXInputDevices       :: [Word]

   } deriving (Show, Eq, Generic, NFData)

instance Default Options where
  def
    = Options

    -- From arguments
    { showHelp                     = False
    , verboseMode                  = False

    , realCapsLock                 = False
    , additionalControls           = True
    , shiftNumericKeys             = False
    , shiftHJKLKeys                = False
    , rightControlAsRightSuper     = False

    , alternativeModeWithAltMod    = False
    , toggleAlternativeModeByAlts  = False
    , turnOffFourthRow             = False
    , ergonomicMode                = NoErgonomicMode
    , superDoublePress             = True
    , leftSuperDoublePressCmd      = Nothing
    , rightSuperDoublePressCmd     = Nothing
    , f24asVerticalBar             = False

    , resetByRealEscape            = False
    , resetByEscapeOnCapsLock      = True
    , resetByWindowFocusEvent      = True

    , debouncerTiming              = Nothing

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

  -- Features options

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
  , GetOpt.Option  [ ]  [shiftHjklOptName]
      (GetOpt.NoArg $ shiftHJKLKeys' .~ True)
      [qmb| Shift 'HJKL' keys one column right \
              ('semicolon' key would be moved on original 'H' key position).
            To place arrows keys (alternative mode, vim, tmux selection, etc.) \
              under four fingers to provide more convenient experience.
            Default is: {shiftHJKLKeys def ? "On" $ "Off"}
            |]
  , GetOpt.Option  [ ]  ["right-control-as-super"]
      (GetOpt.NoArg $ rightControlAsRightSuper' .~ True)
      [qmb| Remap Right Control as Right Super key.
            Some keyboards doesn't have neither Right Super nor Menu key \
              at the right side, since you can have Control key pressing \
              Enter by "additional controls" feature, this could be a solution.
            Default is: {rightControlAsRightSuper def ? "On" $ "Off"}
            |]

  , GetOpt.Option  [ ]  [holdAltForAlternativeModeOptName]
      (GetOpt.NoArg $ alternativeModeWithAltMod' .~ True)
      [qmb| When hold Alt key (left or right, doesn't matter) \
              alternative mode is turned on (real Alt keys aren't triggered).
            To trigger real Alt key you press Alt+Space, in this case \
              alternative mode is turned off and real Alt is triggered \
              from that moment.
            To turn alternative mode on Alt key supposed to be pressed first \
              before any other key or modifier.
            Do not use with --{toggleAlternativeModeByAltsOptName} to be able \
              to press combo like Alt-2 by just both Alts pressed plus "w" \
              key. Otherwise you'll just have turn alternative mode on \
              permanently (by pressing both Alts or by double pressing Super \
              key if such feature is enabled) \
              and then press Alt-w to trigger actual Alt-2.
            Default is: {alternativeModeWithAltMod def ? "On" $ "Off"}
            |]
  , GetOpt.Option  [ ]  [toggleAlternativeModeByAltsOptName]
      (GetOpt.NoArg $ toggleAlternativeModeByAlts' .~ True)
      [qmb| Enable toggling alternative mode \
              by pressing Alt keys (Left and Right) both at the same time.
            Default is: {toggleAlternativeModeByAlts def ? "On" $ "Off"}
            |]
  , GetOpt.Option  [ ]  [turnOffFourthRowOptName]
      (GetOpt.NoArg $ turnOffFourthRow' .~ True)
      [qmb| Turns off fourth keys row completely.
            This helps to change your reflexes when \
              --{holdAltForAlternativeModeOptName} feature is turned on.
            {makesNoSense ergonomicModeOptName}
            Default is: {turnOffFourthRow def ? "On" $ "Off"}
            |]
  , GetOpt.Option  [ ]  [ergonomicModeOptName]
      (GetOpt.NoArg $ (turnOffFourthRow' .~ True)
                    . (ergonomicMode' .~ ErgonomicMode))
      [qmb| Turns on kinda hardcore ergonomic mode \
              (an attempt to make the experience of using a traditional \
              keyboard to be more convenient, or I would say less painful).
            WARNING! It may force you to change a lot of your reflexes!
            I urgently recommend to use this option with \
              --{holdAltForAlternativeModeOptName} option!
            Also --{shiftHjklOptName} would probably get you even more \
              feeling of consistency.
            This mode implies --{turnOffFourthRowOptName} option.
            The main idea of this mode is that a finger moves vertically only \
              one row up/down, and horizontally the index/pinky finger moves \
              only one column left/right.
            Of course to achive this some keys wouldn't be available, to \
              solve this issue some keys will be remapped and some moved to \
              alternative mode (and those keys which don't satisfy the rules \
              will be turned off completely):
            \  Remappings:
            \    * Apostrophe ( ' " ) key will become Enter key \
                   (which also will work as additional Ctrl key \
                   if related option is enabled);
            \    * Open Bracket ( [ \{ ) key will become Apostrophe key \
                   (which was Minus ( - _ ) key in alternative mode).
            \  Rest keys are moved to alternative mode (to first level):
            \    * "A" key will become Minus ( - _ ) key;
            \    * "S" key will become Equal ( = + ) key;
            \    * "D" key will become Open Bracket ( [ \{ ) key;
            \    * "F" key will become Close Bracket ( ] } ) key;
            \    * "G" key will become Backslash ( \\ | ) key;
            \    * Open Bracket key will become Delete key \
                   (Apostrophe ( ' " ) key will be remapped to Enter key, so \
                   it can't be Delete key anymore because Enter key should be \
                   available in all modes).
            \  Also in second level of alternative mode some FN keys will be \
               rearranged:
            \    * Open Bracket key will become F11 key;
            \    * Tab key will become F12 key (it could be F1 and all next in \
                   ascending order in this row but you loose consistency with \
                   regular number keys from first alternative mode level in \
                   this case).
            Real keys which will be disabled (along with fourth row):
            \  * Close Bracket ( ] } ) key;
            \  * Backslash ( \\ | ) key;
            \  * Enter key.
            Default is: {ergonomicMode def /= ErgonomicMode ? "On" $ "Off"}
            |]
  , GetOpt.Option  [ ]  ["ergonomic-ergodox-mode"]
      (GetOpt.NoArg $ (turnOffFourthRow' .~ False)
                    . (ergonomicMode' .~ ErgoDoxErgonomicMode))
      [qmb| TODO add description |]
  , GetOpt.Option  [ ]  [disableSuperDoublePressOptName]
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
            {makesNoSense disableSuperDoublePressOptName}
            |]
  , GetOpt.Option  [ ]  ["left-super-double-press-cmd"]
      (GetOpt.ReqArg (Just .> set leftSuperDoublePressCmd') "COMMAND")
      [qmb| Double Left Super key press will spawn specified shell command \
              instead of toggling alternative mode.
            {makesNoSense disableSuperDoublePressOptName}
            |]
  , GetOpt.Option  [ ]  ["right-super-double-press-cmd"]
      (GetOpt.ReqArg (Just .> set rightSuperDoublePressCmd') "COMMAND")
      [qmb| Double Right Super key press will spawn specified shell command \
              instead of toggling alternative mode.
            {makesNoSense disableSuperDoublePressOptName}
            |]
  , GetOpt.Option  [ ]  ["f24-as-vertical-bar"]
      (GetOpt.NoArg (f24asVerticalBar' .~ True))
      [qmb| TODO add description |]

  , GetOpt.Option  [ ]  ["reset-by-real-escape"]
      (GetOpt.NoArg $ resetByRealEscape' .~ True)
      [qmb| Enable resetting Caps Lock mode, Alternative mode \
              and keyboard layout by real Escape key.
            Default is: {resetByRealEscape def ? "On" $ "Off"}
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

  , let defaultValue = 0.03 :: POSIXTime
        convertFn x = fromRational $ toRational (read x :: Word8) / 1000
        setter x = set debouncerTiming' $ fmap convertFn x <|> Just defaultValue

        defaultIs Nothing  = "Off"
        defaultIs (Just x) = [qm| On ({ floor $ x * 1000 :: Word8 }ms) |]

     in GetOpt.Option  [ ]  ["software-debouncer"]
          (GetOpt.OptArg setter "MILLISECONDS")
          [qmb| Enable software debouncer feature.
                Debouncing is usually done on hardware or firmware level of a \
                  keyboard but timing could be not configurable. \
                  Some keyboards may have issues with doubling or just \
                  shrapnelling some keys (triggering a key pressing more than \
                  once per one physical press) especially after long use time. \
                  By using this feature you could use your keyboard longer, \
                  give it a second life.
                How this feature works: when you press/release a key only \
                  first event is handled and then it waits for specified or \
                  default timing ignoring any other events of that key in that \
                  gap and only after that it handles next state of that key.
                If this feature is turned on but timing is not specified then \
                  default timing would be: \
                  { floor $ defaultValue * 1000 :: Word8 }ms.
                Default is: {defaultIs $ debouncerTiming def}
                |]

  -- Devices handling and disabling xinput devices options

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

  -- "xmobar indicators" options

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

  -- "External control" options

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
        makesSense x = [qm| This option makes sense only with --{x} option. |]

        makesNoSense :: String -> String
        makesNoSense x = [qm| This option makes no sense with --{x} option. |]

        holdAltForAlternativeModeOptName :: String
        holdAltForAlternativeModeOptName = "hold-alt-for-alternative-mode"

        toggleAlternativeModeByAltsOptName :: String
        toggleAlternativeModeByAltsOptName = "toggle-alternative-mode-by-alts"

        disableSuperDoublePressOptName :: String
        disableSuperDoublePressOptName = "disable-super-double-press"

        xmobarIndicatorsOptName, externalControlOptName :: String
        xmobarIndicatorsOptName = "xmobar-indicators"
        externalControlOptName  = "external-control"

        turnOffFourthRowOptName :: String
        turnOffFourthRowOptName = "turn-off-fourth-row"

        ergonomicModeOptName :: String
        ergonomicModeOptName = "ergonomic-mode"

        shiftHjklOptName :: String
        shiftHjklOptName = "shift-hjkl"

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
extractOptions = GetOpt.getOpt GetOpt.Permute options .> \case
  (o, n, []) ->
    Right $ foldl (flip id) def o & handleDevicePath' %~ (<> n)

  (_, _, errs) ->
    Left $ case concat errs of
                (reverse -> '\n':xs) -> reverse xs
                x -> x


usageInfo :: String
usageInfo = '\n' : GetOpt.usageInfo header options where
  header = "Usage: xlib-keys-hack [OPTION...] DEVICES-FD-PATHS..."


noise :: Options -> String -> IO ()
noise (verboseMode -> True) msg = putStrLn msg
noise _ _ = pure ()


subsDisplay :: String -> String -> String
subsDisplay dpy =
  T.pack .> T.replace (T.pack "%DISPLAY%") (T.pack $ map f dpy) .> T.unpack
  where f ':' = '_'; f '.' = '_'; f x = x
