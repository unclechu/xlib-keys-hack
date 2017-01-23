-- Author: Viacheslav Lotsmanov
-- License: GPLv3 https://raw.githubusercontent.com/unclechu/xlib-keys-hack/master/LICENSE

{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE PackageImports #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NoMonomorphismRestriction #-}

module Main (main) where

import "base" System.Exit (ExitCode(ExitFailure), exitFailure, exitSuccess)
import "base" System.Environment (getArgs)
import "directory" System.Directory (doesFileExist)
import "unix" System.Posix.Signals ( installHandler
                                   , Handler(Catch)
                                   , sigINT
                                   , sigTERM
                                   )
import "unix" System.Posix (exitImmediately)
import qualified "base" System.IO as SysIO
import qualified "base" GHC.IO.Handle.FD as IOHandleFD

import qualified "X11" Graphics.X11.Types       as XTypes
import qualified "X11" Graphics.X11.ExtraTypes  as XTypes
import qualified "X11" Graphics.X11.Xlib.Extras as XExtras
import "X11" Graphics.X11.Types (Window)
import "X11" Graphics.X11.Xlib.Types (Display)
import "X11" Graphics.X11.Xlib.Display (defaultRootWindow, closeDisplay)
import "X11" Graphics.X11.Xlib.Misc (keysymToKeycode)

import "deepseq" Control.DeepSeq (deepseq, force)
import qualified "mtl" Control.Monad.State as St (get, put)
import "mtl" Control.Monad.State (StateT, execStateT, evalStateT)
import "either" Control.Monad.Trans.Either (runEitherT, left, right)
import "transformers" Control.Monad.IO.Class (liftIO)
import "transformers" Control.Monad.Trans.Class (lift)
import "base" Control.Monad (when, unless, filterM, forever, forM_, void)
import "lens" Control.Lens ((.~), (%~), (^.), set, over, view)
import "base" Control.Concurrent ( forkIO
                                 , forkFinally
                                 , throwTo
                                 , ThreadId
                                 , threadDelay
                                 )
import "base" Control.Concurrent.MVar (newMVar, modifyMVar_)
import "base" Control.Concurrent.Chan (Chan, newChan, readChan)
import "base" Control.Exception (Exception(fromException))
import "base" Control.Arrow ((&&&))

import "base" Data.Maybe (fromJust, isJust)
import "base" Data.Typeable (Typeable)

-- local imports

import Utils ( (&), (<&>), (.>), (<||>)
             , errPutStrLn
             , dieWith
             , updateState'
             , updateStateM'
             , modifyState
             , modifyStateM
             , writeToFd
             )
import Utils.String (qm)
import Bindings.Xkb ( xkbGetDescPtr
                    , xkbFetchControls
                    , xkbGetGroupsCount
                    , xkbGetDisplay
                    )
import Bindings.MoreXlib (initThreads)
import Process ( initReset
               , watchLeds
               , handleKeyboard
               , processWindowFocus
               , processKeysActions
               , processKeyboardState
               )
import qualified Options as O
import qualified XInput
import State ( CrossThreadVars ( CrossThreadVars
                               , stateMVar
                               , actionsChan
                               , keysActionsChan
                               )
             , State(isTerminating), HasState(isTerminating')
             , initState
             )
import qualified Actions
import Actions (ActionType, Action, KeyAction)
import qualified Keys


type Options = O.Options
type KeyMap  = Keys.KeyMap
type KeyName = Keys.KeyName
type KeyCode = XTypes.KeyCode

-- Bool indicates if it's alive or dead
type ThreadsState = [(Bool, ThreadId)]


main :: IO ()
main = flip evalStateT ([] :: ThreadsState) $ do

  opts <- liftIO $ getArgs >>= parseOpts
  opts `deepseq` return ()
  let noise = liftIO . O.noise opts

  -- We don't need this since we use own X Display instance for every thread
  -- noise "Enabling threads support for Xlib..."
  -- liftIO initThreads


  noise "Initialization of Xkb..."
  dpy <- liftIO xkbInit -- for main thread

  noise "Getting additional X Display for keys actions handler thread..."
  dpyForKeysActionsHanlder <- liftIO xkbInit

  noise "Getting additional X Display for window focus handler thread..."
  dpyForXWindowFocusHandler <- liftIO xkbInit

  noise "Getting additional X Display for keyboard state handler thread..."
  dpyForKeyboardStateHandler <- liftIO xkbInit

  noise "Getting additional X Display for leds watcher thread..."
  dpyForLedsWatcher <- liftIO xkbInit

  let rootWnd = defaultRootWindow dpy


  noise "Dynamically getting media keys X key codes..."
  (mediaKeysAliases :: [(KeyName, KeyCode)]) <- liftIO $ mapM
    (\(keyName, keySym) ->
      keysymToKeycode dpy keySym
        >>= \keyCode -> return (keyName, keyCode))
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
  noise $ "Media keys aliases:" ++
          foldr (\(a, b) -> ([qm|\n  {a}: {b}|] ++)) "" mediaKeysAliases

  let keyMap = Keys.getKeyMap opts mediaKeysAliases

  -- Making it fail at start app time if media keys described incorrectly
  keyMap `deepseq` return ()


  -- TODO remove it after window focus handler will be separated to own process
  -- Prevent errors with closed windows
  liftIO XExtras.xSetErrorHandler

  noise "Initial resetting..."
  liftIO $ initReset opts keyMap dpy rootWnd

  noise "Making cross-thread variables..."
  ctVars <- liftIO $ do
    let state = initState rootWnd
    ctState <- newMVar $ force state
    (ctActions     :: Chan (ActionType Action))    <- newChan
    (ctKeysActions :: Chan (ActionType KeyAction)) <- newChan
    return CrossThreadVars { stateMVar       = ctState
                           , actionsChan     = ctActions
                           , keysActionsChan = ctKeysActions
                           }
  ctVars `deepseq` return ()

  let termHook  = Actions.initTerminate ctVars
      catch sig = installHandler sig (Catch termHook) Nothing
   in liftIO $ mapM_ catch [sigINT, sigTERM]

  let _getThreadIdx = (length <$> St.get) :: StateT ThreadsState IO Int
      _handleFork idx (Left e) =
        case fromException e of
             Just MortifyThreadException -> Actions.threadIsDeath ctVars idx
             _ -> do Actions.panicNoise ctVars
                                        [qm|Unexpected thread exception: {e}|]
                     Actions.overthrow ctVars

      withData tDpy m = m ctVars opts keyMap tDpy rootWnd
      runThread m = modifyStateM $ \ids -> do
        tIdx <- _getThreadIdx
        (True,) .> (: ids) <$>
          liftIO (forkFinally (forever m) $ _handleFork tIdx)

  noise "Starting keys actions handler thread..."
  runThread $ withData dpyForKeysActionsHanlder processKeysActions

  noise "Starting keyboard state handler thread..."
  runThread $ withData dpyForKeyboardStateHandler processKeyboardState

  noise "Starting window focus handler thread..."
  runThread $ withData dpyForXWindowFocusHandler processWindowFocus

  noise "Starting leds watcher thread..."
  runThread $ withData dpyForLedsWatcher watchLeds

  noise "Starting device handle threads (one thread per device)..."
  (devicesDisplays :: [Display]) <-
    let m fd = do noise [qm|Getting own X Display for thread of device: {fd}|]
                  dpy <- liftIO xkbInit
                  noise [qm|Starting handle thread for device: {fd}|]
                  runThread $ withData dpy handleKeyboard fd
                  return dpy
     in mapM m $ O.handleDeviceFd opts

  modifyState reverse

  noise "Listening for actions in main thread..."
  forever $ do
    (action :: ActionType Action) <- liftIO $ readChan $ actionsChan ctVars
    let f :: ActionType Action -> StateT ThreadsState IO ()
        f (Actions.Single a) = m a
        f (Actions.Sequence []) = return ()
        f (Actions.seqHead -> (x, xs)) = m x >> f xs

        m :: Action -> StateT ThreadsState IO ()

        m (Actions.Noise msg) = noise msg
        m (Actions.PanicNoise msg) = liftIO $ errPutStrLn msg

        m (Actions.NotifyXmobar msg) =
          let pipeFd = opts ^. O.xmobarPipeFd'
              log :: String -> String
              log (reverse -> '\n':(reverse -> msg)) = msg
              log msg = msg
           in when (isJust pipeFd) $ do
              noise [qm|Notifying xmobar with message '{log msg}'...|]
              let xmobarFd = fromJust pipeFd
               in liftIO $ writeToFd xmobarFd msg

        m Actions.InitTerminate = do

          liftIO handleTerminationTimeout

          threads <- St.get
          liftIO $ modifyMVar_ (State.stateMVar ctVars)
                 $ execStateT . runEitherT $ do

            -- Check if termination process already initialized
            not . State.isTerminating <$> St.get
              >>= right () <||> let msg = "Attempt to initialize application\
                                          \ termination process when it's\
                                          \ already initialized was skipped"
                                 in noise msg >> left ()

            modifyState $ State.isTerminating' .~ True
            noise "Application termination process initialization..."

            liftIO $ forM_ threads $ snd .> flip throwTo MortifyThreadException

        m (Actions.ThreadIsDead tIdx) = do

          let markAsDead (_, []) = []
              markAsDead (l, (_, x) : xs) = l ++ (False, x) : xs
           in modifyState $ splitAt tIdx .> markAsDead

          (dead, total) <- St.get <&> (length . filter not . map fst &&& length)

          noise [qm| Thread #{tIdx + 1} is dead
                   \ ({dead} of {total} is dead) |]
          when (dead == total) $ liftIO $ Actions.overthrow ctVars

        m Actions.JustDie = do

          liftIO handleTerminationTimeout

          noise "Application is going to die"

          noise "Closing devices files descriptors..."
          forM_ (O.handleDeviceFd opts) $ \fd -> do
            noise [qm|Closing device file descriptor: {fd}...|]
            liftIO $ SysIO.hClose fd

          when (isJust $ O.xmobarPipeFd opts) $ do
            let Just fd = O.xmobarPipeFd opts
            noise [qm|Closing XMobar pipe file descriptor: {fd}...|]
            liftIO $ SysIO.hClose fd

          noise "Closing X Display descriptors..."
          liftIO $ mapM_ closeDisplay $ dpy
                                      : dpyForKeysActionsHanlder
                                      : dpyForXWindowFocusHandler
                                      : dpyForKeyboardStateHandler
                                      : dpyForLedsWatcher
                                      : devicesDisplays

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

            when (opts ^. O.showHelp') $ do
              putStrLn O.usageInfo
              exitSuccess

            opts ^. O.handleDevicePath' & length & (> 0) &
              \x -> unless x $ do
                errPutStrLn O.usageInfo
                dieWith "At least one device fd path must be specified!"

            O.noise opts "Started in verbose mode"
            return opts

        -- Filters only existing descriptors files of devices,
        -- stores this list to 'availableDevices' option and
        -- open these files to read and puts these descriptors to
        -- 'handleDeviceFd' option or fail the application
        -- if there's no available devices.
        extractAvailableDevices :: Options -> IO Options
        extractAvailableDevices opts = flip execStateT opts $
          fmap (^. O.handleDevicePath') St.get
            >>= lift . filterM doesFileExist
            >>= lift . checkForCount
            >>= updateStateM' logAndStoreAvailable
            >>= liftBetween
                  (noise "Opening devices files descriptors for reading...")
            >>= lift . mapM (flip IOHandleFD.openFile SysIO.ReadMode)
            >>= updateState' (flip $ set O.handleDeviceFd')

          where noise = O.noise opts

                logAndStoreAvailable :: O.HasOptions s
                                     => s -> [FilePath] -> StateT s IO s
                logAndStoreAvailable state files = do
                  let devicesList = foldr (("\n  " ++) .> (++)) "" files
                      title = "Devices that will be handled:"
                   in lift $ noise $ title ++ devicesList
                  return (state & O.availableDevices' .~ files)

                -- Checks if we have at least one available device
                -- and gets files list back.
                checkForCount :: [FilePath] -> IO [FilePath]
                checkForCount files = do
                  when (length files < 1) $
                    dieWith "All specified devices to get events from \
                            \is unavailable!"
                  return files

        -- Opens xmobar pipe file descriptor for writing
        extractPipeFd :: Options -> IO Options
        extractPipeFd opts =
          whenHasFile (opts ^. O.xmobarPipeFile') $ \file -> do
            noise "Opening xmobar pipe file for writing..."
            fd <- IOHandleFD.openFile file SysIO.WriteMode
            return (opts & O.xmobarPipeFd' .~ Just fd)

          where noise = O.noise opts

                whenHasFile :: Maybe FilePath
                            -> (FilePath -> IO Options)
                            -> IO Options
                whenHasFile (Just file) m = m file
                whenHasFile Nothing     _ = return opts

        -- Lift up a monad and return back original value
        liftBetween :: Monad m => m () -> a -> StateT s m a
        liftBetween monad x = lift monad >> return x

        -- Completely parse input arguments and returns options
        -- data structure based on them.
        parseOpts :: [String] -> IO Options
        parseOpts argv =
          getOptsFromArgs argv
            >>= extractAvailableDevices
            >>= XInput.getAvailable
            >>= XInput.disable
            >>= logDisabled
            >>= extractPipeFd

          where logDisabled :: Options -> IO Options
                logDisabled opts = opts ^. O.availableXInputDevices'
                  & show .> ("XInput devices ids that was disabled: " ++)
                  & (\x -> O.noise opts x >> return opts)

        -- For situations when something went wrong and application
        -- can't finish its stuff correctly.
        handleTerminationTimeout :: IO ()
        handleTerminationTimeout = void $ forkIO $ do
          threadDelay $ terminationTimeout * 1000 * 1000
          errPutStrLn [qm| Termination process timeout
                         \ after {terminationTimeout} seconds,
                         \ just exiting immidiately... |]
          exitImmediately $ ExitFailure 1


-- Initializes Xlib and Xkb and checks if everything is okay
-- and returns Xlib Display pointer then.
xkbInit :: IO Display
xkbInit = do

  (dpy :: Display) <- xkbGetDisplay >>= flip either return
    (\err -> dieWith [qm|Xkb open display error: {err}|])

  xkbDescPtr <- xkbGetDescPtr dpy >>= flip either return
    (\err -> dieWith [qm|Xkb error: get keyboard data error: {err}|])

  xkbFetchControls dpy xkbDescPtr
    >>= flip unless (dieWith "Xkb error: fetch controls error")

  (> 0) <$> xkbGetGroupsCount xkbDescPtr
    >>= flip unless (dieWith "Xkb error: groups count is 0")

  return dpy


data MyThreadException = MortifyThreadException
                         deriving (Show, Typeable)

instance Exception MyThreadException


-- In seconds
terminationTimeout :: Int
terminationTimeout = 5
