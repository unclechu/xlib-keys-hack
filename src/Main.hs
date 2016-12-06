-- Author: Viacheslav Lotsmanov
-- License: GPLv3 https://raw.githubusercontent.com/unclechu/xlib-keys-hack/master/LICENSE

{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE BangPatterns #-}

module Main (main) where

import System.Exit (exitFailure, exitSuccess)
import System.Environment (getArgs)
import System.Directory (doesFileExist)

import Control.DeepSeq (deepseq, force)
import qualified Control.Monad.State as St
import Control.Monad (when, unless, filterM, forever, forM_)
import Control.Lens ((.~), (%~), (^.), set, over, view)
import Control.Concurrent (forkIO, threadDelay)
import Control.Concurrent.MVar (newMVar)
import Control.Concurrent.Chan (Chan, newChan, readChan)

import Data.Either (Either(Left, Right), either)
import Data.Maybe (Maybe(Nothing, Just), fromJust, isJust)

import qualified Graphics.X11.Types       as XTypes
import qualified Graphics.X11.ExtraTypes  as XTypes
import qualified Graphics.X11.Xlib.Extras as XExtras
import Graphics.X11.Types (Window)
import Graphics.X11.Xlib.Types (Display)
import Graphics.X11.Xlib.Display (defaultRootWindow)
import Graphics.X11.Xlib.Misc (keysymToKeycode)

import qualified System.IO as SysIO
import qualified GHC.IO.Handle.FD as IOHandleFD

import Utils ( (&), (.>)
             , errPutStrLn
             , dieWith
             , updateState'
             , updateStateM'
             , writeToFd
             )
import Bindings.Xkb ( xkbGetDescPtr
                    , xkbFetchControls
                    , xkbGetGroupsCount
                    , xkbGetDisplay
                    )
import Bindings.MoreXlib (initThreads)
import Process ( initReset
               , processXEvents
               , watchLeds
               , handleKeyboard
               )
import qualified Options as O
import qualified XInput
import qualified State
import qualified Actions
import qualified Keys


-- Initializes Xlib and Xkb and checks if everything is okay
-- and returns Xlib Display pointer then.
xkbInit :: IO Display
xkbInit = do

  (dpy :: Display) <- xkbGetDisplay >>= flip either return
    (\err -> dieWith $ "Xkb open display error: " ++ show err)

  xkbDescPtr <- xkbGetDescPtr dpy >>= flip either return
    (\err -> dieWith $ "Xkb error: get keyboard data error" ++ show err)

  xkbFetchControls dpy xkbDescPtr
    >>= flip unless (dieWith "Xkb error: fetch controls error")

  xkbGetGroupsCount xkbDescPtr
    >>= return . (> 0)
    >>= flip unless (dieWith "Xkb error: groups count is 0")

  return dpy


main :: IO ()
main = do

  opts <- getArgs >>= parseOpts
  opts `deepseq` return ()
  let noise = O.noise opts

  noise "Enabling threads support for Xlib..."
  initThreads


  noise "Initialization of Xkb..."
  dpy <- xkbInit -- for main thread

  noise "Getting additional X Display for window focus handler thread..."
  dpyForXWindowFocusHandler <- xkbInit

  noise "Getting additional X Display for leds watcher thread..."
  dpyForLedsWatcher <- xkbInit


  let rootWnd = defaultRootWindow dpy

  let keyMap = Keys.getKeyMap []
  keyMap `deepseq` return ()

  -- prevent errors with closed windows
  XExtras.xSetErrorHandler

  noise "Initial resetting..."
  initReset opts keyMap dpy rootWnd

  noise "Making cross-thread variables..."
  ctVars <- do
    let state = State.initState rootWnd
    ctState <- newMVar $ force state
    (ctActions :: Chan Actions.ActionType) <- newChan
    return State.CrossThreadVars { State.stateMVar   = ctState
                                 , State.actionsChan = ctActions
                                 }
  ctVars `deepseq` return ()

  let withData :: Display
               -> ( State.CrossThreadVars
                    -> O.Options
                    -> Keys.KeyMap
                    -> Display
                    -> Window
                    -> IO () )
               -> IO ()
      withData tDpy m = m ctVars opts keyMap tDpy rootWnd

  noise "Starting window focus handler thread..."
  forkIO $ forever $ withData dpyForXWindowFocusHandler processXEvents

  noise "Starting leds watcher thread..."
  forkIO $ forever $ withData dpyForLedsWatcher watchLeds

  noise "Starting device handle threads (one thread per device)..."
  let m fd = do noise $ "Getting own X Display for thread of device: "
                      ++ show fd
                dpy <- xkbInit
                noise $ "Starting handle thread for device: " ++ show fd
                forkIO $ forever $
                  handleKeyboard ctVars opts keyMap dpy rootWnd fd
      in forM_ (opts ^. O.handleDeviceFd') m

  noise "Listening for actions in main thread..."
  forever $ do
    (action :: Actions.ActionType) <- readChan $ State.actionsChan ctVars
    let f :: Actions.ActionType -> IO ()
        f (Actions.Single a) = m a
        f (Actions.Sequence []) = return ()
        f (Actions.seqHead -> (x, xs)) = m x >> f xs

        m :: Actions.Action -> IO ()
        m (Actions.Noise msg) = noise msg
        m (Actions.NotifyXmobar msg) =
          let pipeFd = opts ^. O.xmobarPipeFd'
              log :: String -> String
              log (reverse -> '\n':(reverse -> msg)) = msg
              log msg = msg
              in when (isJust pipeFd) $ do
                noise $ "Notifying xmobar with message '" ++ log msg ++ "'..."
                let xmobarFd = fromJust pipeFd
                writeToFd xmobarFd msg

        in f action

  where -- Parses arguments and returns options data structure
        -- or shows usage info and exit the application
        -- (by --help flag or because of error).
        getOptsFromArgs :: [String] -> IO O.Options
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
        extractAvailableDevices :: O.Options -> IO O.Options
        extractAvailableDevices opts = flip St.execStateT opts $
          fmap (^. O.handleDevicePath') St.get
            >>= St.lift . filterM doesFileExist
            >>= St.lift . checkForCount
            >>= updateStateM' logAndStoreAvailable
            >>= liftBetween
                  (noise "Opening devices files descriptors for reading...")
            >>= St.lift . mapM (flip IOHandleFD.openFile SysIO.ReadMode)
            >>= updateState' (flip $ set O.handleDeviceFd')

          where noise = O.noise opts

                logAndStoreAvailable :: O.HasOptions s
                                     => s -> [FilePath] -> St.StateT s IO s
                logAndStoreAvailable state files = do
                  St.lift $ noise
                          $ "Devices that will be handled: " ++ show files
                  return (state & O.availableDevices' .~ files)

                -- Checks if we have at least one available device
                -- and gets files list back.
                checkForCount :: [FilePath] -> IO [FilePath]
                checkForCount files = do
                  when (length files < 1) $
                    dieWith "All specified devices to get events from \
                            \is unavailable!"
                  return files

        -- Opens xmobar pipe file descriptor for writing.
        extractPipeFd :: O.Options -> IO O.Options
        extractPipeFd opts =
          whenHasFile (opts ^. O.xmobarPipeFile') $ \file -> do
            noise "Opening xmobar pipe file for writing..."
            fd <- IOHandleFD.openFile file SysIO.WriteMode
            return (opts & O.xmobarPipeFd' .~ Just fd)

          where noise = O.noise opts

                whenHasFile :: Maybe FilePath
                            -> (FilePath -> IO O.Options)
                            -> IO O.Options
                whenHasFile (Just file) m = m file
                whenHasFile Nothing     _ = return opts

        -- Lift up a monad and return back original state.
        liftBetween :: Monad m => m () -> a -> St.StateT s m a
        liftBetween monad x = St.lift monad >> return x

        -- Completely parse input arguments and returns options
        -- data structure based on them.
        parseOpts :: [String] -> IO O.Options
        parseOpts argv =
          getOptsFromArgs argv
            >>= extractAvailableDevices
            >>= XInput.getAvailable
            >>= XInput.disable
            >>= logDisabled
            >>= extractPipeFd

          where logDisabled :: O.Options -> IO O.Options
                logDisabled opts = opts ^. O.availableXInputDevices'
                  & show .> ("XInput devices ids that was disabled: " ++)
                  & (\x -> O.noise opts x >> return opts)
