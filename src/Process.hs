-- Author: Viacheslav Lotsmanov
-- License: GPLv3 https://raw.githubusercontent.com/unclechu/xlib-keys-hack/master/LICENSE

{-# LANGUAGE DoAndIfThenElse #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Process
  ( processXEvents
  , initReset
  ) where

import System.Exit (exitFailure)

import Control.Monad (when, unless)
import qualified Control.Monad.State as St
import Control.Monad.State.Class (MonadState)
import Control.Lens ((.~), (^.))
import Control.Concurrent (forkIO, ThreadId)
import Control.Concurrent.MVar (MVar, takeMVar, modifyMVar_, putMVar)

import Data.Maybe (Maybe(Just, Nothing))
import Data.Bits ((.|.))

import qualified Graphics.X11.Types       as XTypes
import qualified Graphics.X11.ExtraTypes  as XTypes
import qualified Graphics.X11.Xlib.Event  as XEvent
import qualified Graphics.X11.Xlib.Extras as XExtras
import Graphics.X11.Xlib.Misc (getInputFocus)
import Graphics.X11.Xlib.Types (Display)
import Graphics.X11.Types (Window)

import Utils ( (&), (.>), (<||>)
             , nextEvent'
             , errPutStrLn
             , dieWith
             , updateState'
             )
import Bindings.Xkb (xkbSetGroup)
import Bindings.XTest (fakeKeyEvent, fakeKeyCodeEvent)
import qualified Options as O
import qualified State
import qualified Keys


processXEvents :: State.MVars
               -> O.Options
               -> Keys.KeyCodes
               -> Display
               -> Window
               -> IO ()
processXEvents mVars opts keyCodes dpy rootWnd = process $ \wnd -> do

  XEvent.sync dpy False
  XEvent.selectInput dpy wnd XTypes.focusChangeMask

  evPtr <- XEvent.allocaXEvent return
  nextEvent dpy evPtr
  ev <- XExtras.getEvent evPtr

  dealMap (XExtras.eventName ev) evPtr

  where nextEvent = nextEvent'

        process :: (Window -> IO ()) -> IO ()
        process m =
          fmap fst (getInputFocus dpy)
            >>= (\wnd -> when (wnd /= rootWnd) $ m wnd)

        dealMap :: String -> XEvent.XEventPtr -> IO ()
        dealMap evName evPtr
          | evName `elem` ["FocusIn", "FocusOut"] =
              processXFocusEvent evName evPtr
          | otherwise = return ()

        processXFocusEvent :: String -> XEvent.XEventPtr -> IO ()
        processXFocusEvent evName evPtr = f $ \prevState -> do

          noise $ "Handling focus event: " ++ evName ++ "..."

          let lastWnd = State.lastWindow prevState
          curWnd <- XEvent.get_Window evPtr

          when (evName == "FocusOut") $ do
            noise "Resetting keyboard layout..."
            resetKbdLayout dpy

          if curWnd == lastWnd
             then return prevState
             else do
               noise $ "Window focus moved from "
                         ++ show lastWnd
                         ++ " to " ++ show curWnd
               return $ prevState { State.lastWindow = curWnd }

          where f :: (State.State -> IO State.State) -> IO ()
                f = modifyMVar_ $ State.stateMVar mVars

                noise :: String -> IO ()
                noise msg = when (O.verboseMode opts)
                          $ putMVar (State.debugMVar mVars) [State.Noise msg]


resetKbdLayout :: Display -> IO ()
resetKbdLayout dpy =
  xkbSetGroup dpy 0 >>= flip unless (dieWith "xkbSetGroup error")


initReset :: Keys.RealKeyCodes -> Display -> Window -> IO ()
initReset realKeyCodes dpy rootWnd = resetKbdLayout dpy
