-- Author: Viacheslav Lotsmanov
-- License: GPLv3 https://raw.githubusercontent.com/unclechu/xlib-keys-hack/master/LICENSE

{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE NoMonomorphismRestriction #-}

module Main (main) where

import "base" System.Exit (ExitCode (ExitFailure), exitSuccess)
import "base" System.Environment (getArgs)
import "directory" System.Directory (doesFileExist)
import "unix" System.Posix.Signals ( installHandler
                                   , Handler (Catch)
                                   , sigINT
                                   , sigTERM
                                   )
import "unix" System.Posix (exitImmediately)
import qualified "base" System.IO as SysIO
import qualified "base" GHC.IO.Handle.FD as IOHandleFD
import "process" System.Process (terminateProcess, waitForProcess)

import qualified "X11" Graphics.X11.Types      as XTypes
import qualified "X11" Graphics.X11.ExtraTypes as XTypes
import "X11" Graphics.X11.Xlib.Types (Display)
import "X11" Graphics.X11.Xlib.Display (closeDisplay)
import "X11" Graphics.X11.Xlib.Misc (keysymToKeycode)
import "X11" Graphics.X11.Xlib (displayString)

import "deepseq" Control.DeepSeq (deepseq, force)
import qualified "mtl" Control.Monad.State as St (get, gets, put, modify)
import "mtl" Control.Monad.State (StateT, execStateT, evalStateT)
import "base" Control.Monad.IO.Class (liftIO)
import "transformers" Control.Monad.Trans.Class (lift)
import "transformers" Control.Monad.Trans.Except (runExceptT, throwE)
import "base" Control.Monad ((>=>), when, unless, filterM, forever, forM_, void)
import "lens" Control.Lens ((.~), (^.), set, view)
import "base" Control.Concurrent ( forkIO
                                 , forkFinally
                                 , throwTo
                                 , ThreadId
                                 , threadDelay
                                 , tryTakeMVar
                                 )
import "base" Control.Concurrent.MVar (newMVar, modifyMVar_, readMVar)
import "base" Control.Concurrent.Chan (Chan, newChan, readChan)
import "base" Control.Exception (Exception (fromException))
import "base" Control.Arrow ((&&&))
import "extra" Control.Monad.Extra (whenJust)

import "base" Data.Word (Word8)
import "base" Data.Maybe (fromJust)
import "base" Data.List (intercalate)
import "base" Data.Typeable (Typeable)
import "data-default" Data.Default (def)
import "qm-interpolated-string" Text.InterpolatedString.QM (qm, qms, qns)

-- local imports

import "xlib-keys-hack" Utils.Sugar
                      ( (&), (<&>), (.>), (|?|), (?)
                      , preserveF', unnoticed, apart
                      )
import "xlib-keys-hack" Utils (errPutStrLn, dieWith)
import "xlib-keys-hack" Bindings.Xkb ( xkbGetDescPtr
                                     , xkbFetchControls
                                     , xkbGetGroupsCount
                                     , xkbGetDisplay
                                     )
-- import "xlib-keys-hack" Bindings.MoreXlib (initThreads)
import qualified "xlib-keys-hack" Options as O
import qualified "xlib-keys-hack" Actions
import qualified "xlib-keys-hack" XInput
import qualified "xlib-keys-hack" Keys
import "xlib-keys-hack" Actions (ActionType, Action, KeyAction)
import "xlib-keys-hack" IPC ( openIPC
                            , closeIPC
                            , setIndicatorState
                            , logView
                            )
import "xlib-keys-hack" Process ( initReset
                                , watchLeds

                                , handleKeyEvent
                                , getNextKeyboardDeviceKeyEvent

                                , getSoftwareDebouncer
                                , getSoftwareDebouncerTiming
                                , moveKeyThroughSoftwareDebouncer
                                , handleNextSoftwareDebouncerEvent

                                , processWindowFocus
                                , processKeysActions
                                , processKeyboardState
                                )
import qualified "xlib-keys-hack" Process.CrossThread as CrossThread
                                ( toggleAlternative
                                , turnAlternativeMode
                                )
import "xlib-keys-hack" State ( CrossThreadVars ( CrossThreadVars
                                                , stateMVar
                                                , actionsChan
                                                , keysActionsChan
                                                )
                              , State ( isTerminating
                                      , windowFocusProc
                                      , alternative
                                      , kbdLayout
                                      )
                              , HasState (isTerminating', leds')
                              , HasLedModes (numLockLed', capsLockLed')
                              )


type Options = O.Options
type KeyName = Keys.KeyName
type KeyCode = XTypes.KeyCode

-- Bool indicates if it's alive or dead
type ThreadsState = [(Bool, ThreadId)]


main :: IO ()
main = flip evalStateT ([] :: ThreadsState) $ do

  opts <- liftIO $ getArgs >>= parseOpts
  opts `deepseq` pure ()
  let noise = liftIO . O.noise opts

  -- We don't need this since we use own X Display instance for every thread
  -- noise "Enabling threads support for Xlib..."
  -- liftIO initThreads


  noise "Initialization of Xkb..."
  dpy <- liftIO xkbInit -- for main thread

  noise "Getting additional X Display for keys actions handler thread..."
  dpyForKeysActionsHanlder <- liftIO xkbInit

  noise "Getting additional X Display for keyboard state handler thread..."
  dpyForKeyboardStateHandler <- liftIO xkbInit

  noise "Getting additional X Display for leds watcher thread..."
  dpyForLedsWatcher <- liftIO xkbInit


  noise "Dynamically getting media keys X key codes..."
  (mediaKeysAliases :: [(KeyName, KeyCode)]) <- liftIO $ mapM
    (\(keyName, keySym) -> (keyName,) <$> keysymToKeycode dpy keySym)
    [ (Keys.MCalculatorKey,        XTypes.xF86XK_Calculator)
    , (Keys.MEjectKey,             XTypes.xF86XK_Eject)

    , (Keys.MAudioMuteKey,         XTypes.xF86XK_AudioMute)
    , (Keys.MAudioLowerVolumeKey,  XTypes.xF86XK_AudioLowerVolume)
    , (Keys.MAudioRaiseVolumeKey,  XTypes.xF86XK_AudioRaiseVolume)

    , (Keys.MAudioPlayKey,         XTypes.xF86XK_AudioPlay)
    , (Keys.MAudioStopKey,         XTypes.xF86XK_AudioStop)
    , (Keys.MAudioPrevKey,         XTypes.xF86XK_AudioPrev)
    , (Keys.MAudioNextKey,         XTypes.xF86XK_AudioNext)

    , (Keys.MMonBrightnessDownKey, XTypes.xF86XK_MonBrightnessDown)
    , (Keys.MMonBrightnessUpKey,   XTypes.xF86XK_MonBrightnessUp)
    ]
  noise
    $ "Media keys aliases:"
    <> foldMap (\(a, b) -> [qm|\n  {a}: {b}|]) mediaKeysAliases

  let keyMap = Keys.getKeyMap opts mediaKeysAliases

  -- Making it fail at start app time if media keys described incorrectly
  keyMap `deepseq` pure ()

  when (O.shiftNumericKeys opts) $
    noise "Numeric keys in numbers row are shifted"


  noise "Making cross-thread variables..."
  ctVars <- liftIO $ do

    ctState <- newMVar $ force def
    (ctActions     :: Chan (ActionType Action))    <- newChan
    (ctKeysActions :: Chan (ActionType KeyAction)) <- newChan

    pure CrossThreadVars { stateMVar       = ctState
                         , actionsChan     = ctActions
                         , keysActionsChan = ctKeysActions
                         }

  ctVars `deepseq` pure ()

  ipcHandle <- preserveF' (O.xmobarIndicators opts || O.externalControl opts) $
    do let for = intercalate " and "
                   $  (O.xmobarIndicators opts ? ["xmobar indicators"] $ mempty)
                   <> (O.externalControl  opts ? ["external control"]  $ mempty)
        in noise [qm| Opening DBus connection for {for}... |]

       h <- let flush = Actions.flushXmobar opts ctVars

                _noise' :: [String] -> IO ()
                _noise' = Actions.noise' opts ctVars

                _notify' :: [Actions.XmobarFlag] -> IO ()
                _notify' = Actions.notifyXmobar' opts ctVars

                _toggleAlternative :: IO ()
                _toggleAlternative =
                  modifyMVar_ (State.stateMVar ctVars) $
                    CrossThread.toggleAlternative _noise' _notify'

                _turnAlternativeMode :: Bool -> IO ()
                _turnAlternativeMode to =
                  modifyMVar_ (State.stateMVar ctVars) $
                    flip (CrossThread.turnAlternativeMode _noise' _notify') to

                altModeChange :: Maybe Bool -> IO ()
                altModeChange Nothing  = _toggleAlternative
                altModeChange (Just x) = _turnAlternativeMode x

             in lift $ openIPC (displayString dpy) opts flush altModeChange

       h <$ noise (logView h)


  noise "Initial resetting..."
  liftIO $ initReset opts ipcHandle keyMap dpy


  let termHook  = Actions.initTerminate ctVars
      catch sig = installHandler sig (Catch termHook) Nothing
   in liftIO $ mapM_ catch [sigINT, sigTERM]

  let runThread :: String -> IO () -> StateT ThreadsState IO ()
      runThread threadName m = go where
        go = do
          noise [qm| Starting {threadName} thread... |]
          (ids, tIdx) <- St.gets $ id &&& length
          tId <- liftIO $ forkFinally m $ handleFork tIdx
          St.put $ (True, tId) : ids

        handleFork idx = \case
          Left e ->
            case fromException e of
              Just MortifyThreadException ->
                Actions.threadIsDeath ctVars threadName idx

              _ -> do
                Actions.panicNoise ctVars
                  [qm| Unexpected thread #{idx} "{threadName}" exception: {e} |]
                Actions.overthrow ctVars

          Right _ -> do
            Actions.panicNoise ctVars
              [qm| Thread #{idx} "{threadName}" unexpectedly terminated |]
            Actions.overthrow ctVars

  runThread "keys actions handler" $
            processKeysActions ctVars keyMap dpyForKeysActionsHanlder

  runThread "keyboard state handler" $
            processKeyboardState ctVars opts dpyForKeyboardStateHandler

  when (O.resetByWindowFocusEvent opts) $
    runThread "window focus handler" $ processWindowFocus ctVars opts

  runThread "leds watcher" $ watchLeds ctVars opts dpyForLedsWatcher

  noise "Starting device handle threads (one thread per device)..."
  let keyEventHandler = handleKeyEvent ctVars opts keyMap
  O.handleDeviceFd opts `forM_` \fd -> do
    liftIO (getSoftwareDebouncer opts) >>= \case
      Nothing ->
        runThread [qm| handler for device: {fd} |] $ forever $
          getNextKeyboardDeviceKeyEvent keyMap fd >>= keyEventHandler

      Just softwareDebouncer -> do
        let timing = round $
              getSoftwareDebouncerTiming softwareDebouncer * 1000 :: Word8

        runThread [qms| software debouncer (with timing: {timing}ms)
                        debounced events handling for device: {fd} |]
          $ forever
          $ handleNextSoftwareDebouncerEvent softwareDebouncer
              >>= pure () `maybe` keyEventHandler

        runThread [qms| handler for device
                        (with software debouncer timing: {timing}ms): {fd} |]
          $ forever
          $ getNextKeyboardDeviceKeyEvent keyMap fd
              >>= moveKeyThroughSoftwareDebouncer softwareDebouncer
              >>= pure () `maybe` keyEventHandler

  St.modify reverse -- Threads in order they have been forked

  noise "Listening for actions in main thread..."
  forever $ do
    (action :: ActionType Action) <- liftIO $ readChan $ actionsChan ctVars

    let f :: ActionType Action -> StateT ThreadsState IO ()
        f (Actions.Single a) = m a
        f (Actions.Sequence []) = pure ()
        f (Actions.seqHead -> (x, xs)) = m x >> f xs

        m :: Action -> StateT ThreadsState IO ()

        m (Actions.Noise msg) = noise msg
        m (Actions.PanicNoise msg) = liftIO $ errPutStrLn msg

        m (Actions.NotifyXmobar x) = whenJust ipcHandle $ \ipc ->

          let flag a isOn title = do
                noise [qms| Setting xmobar {title} indicator state
                            {isOn ? "On" $ "Off"}... |]
                liftIO $ setIndicatorState ipc a

              value a v title = do
                noise [qm| Setting xmobar {title} indicator value to '{v}'... |]
                liftIO $ setIndicatorState ipc a

              flush = do

                noise "Flushing all xmobar indicators..."
                state <- liftIO $ readMVar $ State.stateMVar ctVars

                handle $ Actions.XmobarNumLockFlag
                       $ state ^. State.leds' . State.numLockLed'

                handle $ Actions.XmobarCapsLockFlag
                       $ state ^. State.leds' . State.capsLockLed'

                handle $ Actions.XmobarAlternativeFlag
                       $ State.alternative state

                handle $ Actions.XmobarXkbLayout
                       $ State.kbdLayout state

              handle a = case a of
                Actions.XmobarFlushAll          -> flush
                Actions.XmobarNumLockFlag     y -> flag a y "Num Lock"
                Actions.XmobarCapsLockFlag    y -> flag a y "Caps Lock"
                Actions.XmobarAlternativeFlag y -> flag a y "Alternative Mode"
                Actions.XmobarXkbLayout       y -> value a y "Keyboard layout"

           in handle x

        m Actions.InitTerminate = do

          liftIO handleTerminationTimeout

          threads <- St.get
          liftIO $ modifyMVar_ (State.stateMVar ctVars)
                 $ execStateT . runExceptT $ do

            -- Check if termination process already initialized
            St.gets (not . State.isTerminating)
              >>= pure () |?| let s = [qns| Attempt to initialize application
                                            termination process when it's
                                            already initialized was skipped |]
                               in noise s >> throwE ()

            St.modify $ State.isTerminating' .~ True
            noise "Application termination process initialization..."

            liftIO $ forM_ threads $ snd .> flip throwTo MortifyThreadException

        m (Actions.ThreadIsDead threadName tIdx) = do

          let markAsDead (_, []) = []
              markAsDead (l, (_, x) : xs) = l <> ((False, x) : xs)

           in St.modify $ splitAt tIdx .> markAsDead

          (dead, total) <- St.gets $ length . filter not . map fst &&& length
          noise [qms| Thread #{tIdx + 1} "{threadName}" is dead
                      ({dead} of {total} is dead) |]
          when (dead == total) $ liftIO $ Actions.overthrow ctVars

        m Actions.JustDie = do

          liftIO handleTerminationTimeout
          noise "Application is going to die"
          noise "Closing devices files descriptors..."

          O.handleDeviceFd opts `forM_` \fd -> do
            noise [qm| Closing device file descriptor: {fd}... |]
            liftIO $ SysIO.hClose fd

          let close h = noise "Closing DBus connection..." >> lift (closeIPC h)
           in maybe (pure ()) close ipcHandle

          noise "Closing X Display descriptors..."

          liftIO $ mapM_ closeDisplay [ dpy
                                      , dpyForKeysActionsHanlder
                                      , dpyForKeyboardStateHandler
                                      , dpyForLedsWatcher
                                      ]

          when (O.resetByWindowFocusEvent opts) $

            liftIO $ tryTakeMVar (State.stateMVar ctVars)
                      <&> fmap State.windowFocusProc

              >>= let fm (fromJust -> Just (execFilePath, procH, outH)) = do

                         noise [qms| Terminating of window focus events watcher
                                     '{execFilePath}' subprocess... |]

                         liftIO $ SysIO.hClose outH
                         liftIO $ terminateProcess procH
                         exitCode <- liftIO $ waitForProcess procH

                         noise [qms| Subprocess '{execFilePath}' terminated
                                     with exit code: {exitCode} |]

                      fm _ = pure ()

                   in fm

          noise "Enabling disabled before XInput devices back..."
          liftIO $ XInput.enable opts

          noise "The end"
          liftIO exitSuccess

     in f action

  where -- Parses arguments and returns options data structure
        -- or shows usage info and exit the application
        -- (by --help flag or because of error).
        getOptsFromArgs :: [String] -> IO Options
        getOptsFromArgs argv = case O.extractOptions argv of

          Left err -> errPutStrLn O.usageInfo >> dieWith err

          Right opts -> do

            when (O.showHelp opts) $ do
              putStrLn O.usageInfo
              exitSuccess

            O.handleDevicePath opts & length & (> 0) & \x -> unless x $ do
              errPutStrLn O.usageInfo
              dieWith "At least one device fd path must be specified!"

            O.noise opts "Started in verbose mode"
            pure opts

        -- Filters only existing descriptors files of devices,
        -- stores this list to 'availableDevices' option and
        -- open these files to read and puts these descriptors to
        -- 'handleDeviceFd' option or fail the application
        -- if there's no available devices.
        extractAvailableDevices :: Options -> IO Options
        extractAvailableDevices
           =  execStateT
           $  St.gets O.noise
          >>= \noise -> St.gets (view O.handleDevicePath')
          >>= lift . filterM doesFileExist

          >>= ( unnoticed $ length .> \availableDevicesCount ->
                  unless (availableDevicesCount > 0) $ lift $ dieWith
                    "All specified devices to get events from is unavailable!"
              )

          >>= ( unnoticed $ lift . noise
              . ("Devices that will be handled:" <>) . foldMap ("\n  " <>)
              )

          >>= unnoticed (St.modify . set O.availableDevices')

          >>= ( apart $ lift
              $ noise "Opening devices files descriptors for reading..."
              )

          >>= lift . mapM (`IOHandleFD.openFile` SysIO.ReadMode)
          >>= St.modify . set O.handleDeviceFd'


        -- Completely parse input arguments and returns options
        -- data structure based on them.
        parseOpts :: [String] -> IO Options
        parseOpts = go where
          go
             =  getOptsFromArgs
            >=> extractAvailableDevices
            >=> XInput.getAvailable
            >=> unnoticed XInput.disable
            >=> logDisabled

          logDisabled :: Options -> IO Options
          logDisabled opts
            = O.availableXInputDevices opts
            & ("XInput devices ids that was disabled: " <>) . show
            & (\x -> opts <$ O.noise opts x)

        -- For situations when something went wrong and application
        -- can't finish its stuff correctly.
        handleTerminationTimeout :: IO ()
        handleTerminationTimeout = void $ forkIO $ do

          threadDelay $ terminationTimeout * 1000 * 1000

          errPutStrLn [qms| Termination process timeout
                            after {terminationTimeout} seconds,
                            just exiting immidiately... |]

          exitImmediately $ ExitFailure 1


-- Initializes Xlib and Xkb and checks if everything is okay
-- and returns Xlib Display pointer then.
xkbInit :: IO Display
xkbInit = do

  (dpy :: Display) <- xkbGetDisplay >>= (`either` pure)
    (\err -> dieWith [qm| Xkb open display error: {err} |])

  xkbDescPtr <- xkbGetDescPtr dpy >>= (`either` pure)
    (\err -> dieWith [qm| Xkb error: get keyboard data error: {err} |])

  xkbFetchControls dpy xkbDescPtr
    >>= (`unless` dieWith "Xkb error: fetch controls error")

  (> 0) <$> xkbGetGroupsCount xkbDescPtr
    >>= (`unless` dieWith "Xkb error: groups count is 0")

  pure dpy


data MyThreadException = MortifyThreadException deriving (Show, Typeable)
instance Exception MyThreadException


-- In seconds
terminationTimeout :: Int
terminationTimeout = 5
