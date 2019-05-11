-- Author: Viacheslav Lotsmanov
-- License: GPLv3 https://raw.githubusercontent.com/unclechu/xlib-keys-hack/master/LICENSE

{-# LANGUAGE OverloadedStrings, ScopedTypeVariables, NamedFieldPuns #-}

module IPC
  ( IPCHandle
  , openIPC
  , closeIPC
  , setIndicatorState
  , logView
  ) where

import "base" System.Exit (die)
import "base" System.IO (stderr, hPutStrLn)

import "base" Data.Word (Word8)
import "base" Data.Maybe (fromMaybe)
import "base" Data.List (intercalate)
import "qm-interpolated-string" Text.InterpolatedString.QM (qm, qmb)

import "base" Control.Monad (when)

import "dbus" DBus ( ObjectPath
                   , objectPath_
                   , BusName
                   , busName_
                   , InterfaceName
                   , interfaceName_
                   , signal
                   , signalDestination
                   , IsVariant (fromVariant, toVariant)
                   , Signal (signalBody)
                   )

import "dbus" DBus.Client ( Client
                          , disconnect
                          , connectSession
                          , emit
                          , matchAny
                          , addMatch
                          , removeMatch
                          , SignalHandler
                          , RequestNameReply (NamePrimaryOwner)
                          , ReleaseNameReply (NameReleased)
                          , requestName
                          , releaseName

                          , MatchRule ( matchPath
                                      , matchInterface
                                      , matchDestination
                                      , matchMember
                                      )
                          )

-- local imports

import Utils.Sugar ((?))
import Types (AlternativeModeLevel (..))
import qualified Options as O
import Actions ( XmobarFlag ( XmobarNumLockFlag
                            , XmobarCapsLockFlag
                            , XmobarAlternativeFlag
                            , XmobarXkbLayout
                            , XmobarFlushAll
                            )
               )


type Options = O.Options


data XmobarIPC
   = XmobarIPC
   { xmobarObjectPath      :: ObjectPath
   , xmobarBus             :: Maybe BusName
   , xmobarInterface       :: InterfaceName
   , xmobarFlushObjectPath :: ObjectPath
   , xmobarFlushInterface  :: InterfaceName
   , xmobarFlushSigHandler :: SignalHandler
   }

data ExternalCtrlIPC
   = ExternalCtrlIPC
   { externalCtrlObjectPath  :: ObjectPath
   , externalCtrlBus         :: BusName
   , externalCtrlInterface   :: InterfaceName
   , externalCtrlSigHandlers :: [SignalHandler]
   }

data IPCHandle
   = IPCHandle
   { dbusClient      :: Client
   , xmobarIPC       :: Maybe XmobarIPC
   , externalCtrlIPC :: Maybe ExternalCtrlIPC
   }


openIPC :: String -> Options -> IO () -> (Maybe Bool -> IO ()) -> IO IPCHandle
openIPC dpyName opts flushAllCallback altModeChange = do

  dbusSession <- connectSession

  xmobar <- not (O.xmobarIndicators opts) ? pure Nothing $ fmap Just $ do

    let objPath        = objectPath_ $ resolveDpy $
                         O.xmobarIndicatorsObjPath opts

        bus            = case O.xmobarIndicatorsBusName opts of
                              "any" -> Nothing
                              x     -> Just $ busName_ $ resolveDpy x

        iface          = interfaceName_ $ resolveDpy $
                         O.xmobarIndicatorsIface opts

        flushObjPath   = objectPath_ $ resolveDpy $
                         O.xmobarIndicatorsFlushObjPath opts

        flushIface     = interfaceName_ $ resolveDpy $
                         O.xmobarIndicatorsFlushIface opts

        flushMatchRule = matchAny { matchPath      = Just flushObjPath
                                  , matchInterface = Just flushIface
                                  , matchMember    = Just "request_flush_all"
                                  }

        flushHandle (signalBody -> []) = flushAllCallback
        flushHandle _ = return () -- Incorrect arguments, just ignoring it

    -- Handling external request to refresh whole state of indicators
    flushSigHandler <- addMatch dbusSession flushMatchRule flushHandle

    return XmobarIPC { xmobarObjectPath      = objPath
                     , xmobarBus             = bus
                     , xmobarInterface       = iface
                     , xmobarFlushObjectPath = flushObjPath
                     , xmobarFlushInterface  = flushIface
                     , xmobarFlushSigHandler = flushSigHandler
                     }

  externalCtrl <- not (O.externalControl opts) ? pure Nothing $ fmap Just $ do

    let objPath   = objectPath_    $ resolveDpy $ O.externalControlObjPath opts
        bus       = busName_       $ resolveDpy $ O.externalControlBusName opts
        iface     = interfaceName_ $ resolveDpy $ O.externalControlIface   opts

        matchRule = matchAny { matchPath        = Just objPath
                             , matchDestination = Just bus
                             , matchInterface   = Just iface
                             }

        matchAltOnOff  = matchRule
                           { matchMember = Just "switch_alternative_mode" }

        matchAltToggle = matchRule
                           { matchMember = Just "toggle_alternative_mode" }

        altOnOffHandler (signalBody -> map fromVariant -> [Just (x :: Bool)])
          = altModeChange $ Just x
        altOnOffHandler _ = pure ()

        altToggleHandler (signalBody -> []) = altModeChange Nothing
        altToggleHandler _ = pure ()

    requestName dbusSession bus [] >>= \reply ->
      when (reply /= NamePrimaryOwner) $
        die [qm| Requesting name '{bus}' error: '{reply}' |]

    -- Handling external control requests
    switchSigHandler <- addMatch dbusSession matchAltOnOff  altOnOffHandler
    toggleSigHandler <- addMatch dbusSession matchAltToggle altToggleHandler

    return ExternalCtrlIPC { externalCtrlObjectPath  = objPath
                           , externalCtrlBus         = bus
                           , externalCtrlInterface   = iface
                           , externalCtrlSigHandlers = [ switchSigHandler
                                                       , toggleSigHandler
                                                       ]
                           }

  return IPCHandle { dbusClient      = dbusSession
                   , xmobarIPC       = xmobar
                   , externalCtrlIPC = externalCtrl
                   }

  where resolveDpy = O.subsDisplay dpyName


closeIPC :: IPCHandle -> IO ()
closeIPC IPCHandle { dbusClient      = c
                   , xmobarIPC       = xmobar
                   , externalCtrlIPC = externalCtrl
                   } = do

  f (removeMatch c . xmobarFlushSigHandler)           xmobar
  f (mapM_ (removeMatch c) . externalCtrlSigHandlers) externalCtrl

  flip f externalCtrl $ \ExternalCtrlIPC { externalCtrlBus = bus } ->
    releaseName c bus >>= \reply ->
      when (reply /= NameReleased) $
        hPutStrLn stderr [qm| Releasing name '{bus}' error: '{reply}' |]

  disconnect c

  where f = maybe $ pure ()


setIndicatorState :: IPCHandle -> XmobarFlag -> IO ()
setIndicatorState IPCHandle { dbusClient, xmobarIPC } flag = go where
  go = pure () `fromMaybe` do
    XmobarIPC { xmobarObjectPath, xmobarBus, xmobarInterface } <- xmobarIPC
    (`fmap` notifications) $ mapM_ $ \(member, args) ->
      emit dbusClient (signal xmobarObjectPath xmobarInterface member)
                        { signalDestination = xmobarBus
                        , signalBody        = args
                        }

  notifications = case flag of
    XmobarNumLockFlag  x -> Just $ pure ("numlock",   [toVariant x])
    XmobarCapsLockFlag x -> Just $ pure ("capslock",  [toVariant x])
    XmobarXkbLayout    x -> Just $ pure ("xkblayout", [toVariant x])

    XmobarAlternativeFlag alternativeModeState ->
      let
        (isTurnedOn, level, isPermanent) = case alternativeModeState of
          Nothing -> (False, minBound :: Word8, False)
          Just (FirstAlternativeModeLevel,  x) -> (True, 1, x)
          Just (SecondAlternativeModeLevel, x) -> (True, 2, x)
      in
        -- @"alternative"@ here is for backward compatibility.
        -- @"alternative_level"@ already indicates alternative mode is turned
        -- off if /level/ equals to @0@.
        Just [ ("alternative",       [toVariant isTurnedOn])
             , ("alternative_level", [toVariant level, toVariant isPermanent])
             ]

    XmobarFlushAll -> Nothing


logView :: IPCHandle -> String
logView IPCHandle { xmobarIPC       = xmobar
                  , externalCtrlIPC = externalCtrl
                  } =

  intercalate "\n" $ mconcat
    [pure "IPC data (DBus):", xmobarView xmobar, externalCtrlView externalCtrl]

  where _xmobarBusView = maybe "any (broadcasting to everyone)" show

        xmobarView = maybe mempty $ pure .
          \XmobarIPC { xmobarObjectPath      = objPath
                     , xmobarBus             = bus
                     , xmobarInterface       = iface
                     , xmobarFlushObjectPath = flushObjPath
                     , xmobarFlushInterface  = flushIface
                     } ->
            [qmb|\  xmobar object path: {objPath}
                 \  xmobar bus name: {_xmobarBusView bus}
                 \  xmobar interface name: {iface}
                 \  xmobar object path for listening for flush requests: \
                      {flushObjPath}
                 \  xmobar interface name for listening for flush requests: \
                      {flushIface}
                 |]

        externalCtrlView = maybe mempty $ pure .
          \ExternalCtrlIPC { externalCtrlObjectPath = objPath
                           , externalCtrlBus        = bus
                           , externalCtrlInterface  = iface
                           } ->
            [qmb|\  External control IPC object path: {objPath}
                 \  External control IPC bus name: {bus}
                 \  External control IPC interface name: {iface} |]
