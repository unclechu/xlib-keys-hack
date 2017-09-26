-- Author: Viacheslav Lotsmanov
-- License: GPLv3 https://raw.githubusercontent.com/unclechu/xlib-keys-hack/master/LICENSE

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module IPC
  ( IPCHandle
  , openIPC
  , closeIPC
  , setIndicatorState
  , logView
  ) where

import "base" System.Exit (die)
import "base" System.IO (stderr, hPutStrLn)

import "base" Data.List (intercalate)
import "base" Data.Maybe (fromMaybe)
import "qm-interpolated-string" Text.InterpolatedString.QM (qm)

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

data IPCHandle = IPCHandle { dbusClient      :: Client
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
setIndicatorState IPCHandle { dbusClient = c
                            , xmobarIPC  = Just XmobarIPC
                                { xmobarObjectPath = path
                                , xmobarBus = bus
                                , xmobarInterface = iface
                                }
                            }
                  flag =

  emit c (signal path iface member)
           { signalDestination = bus
           , signalBody        = [arg]
           }

  where (member, arg) =

          case flag of

               XmobarNumLockFlag     x -> ("numlock",     toVariant x)
               XmobarCapsLockFlag    x -> ("capslock",    toVariant x)
               XmobarAlternativeFlag x -> ("alternative", toVariant x)
               XmobarXkbLayout       x -> ("xkblayout",   toVariant x)

               XmobarFlushAll -> error "unexpected value"

setIndicatorState _ _ = pure ()


logView :: IPCHandle -> String
logView IPCHandle { xmobarIPC       = xmobar
                  , externalCtrlIPC = externalCtrl
                  } =

  intercalate "\n" $ mconcat
    [["IPC data (DBus):"], xmobarView xmobar, externalCtrlView externalCtrl]

  where _xmobarBusView = fromMaybe "any (broadcasting to everyone)" . fmap show
        _if f = maybe [] $ (: []) . f

        xmobarView = _if $
          \XmobarIPC { xmobarObjectPath      = objPath
                     , xmobarBus             = bus
                     , xmobarInterface       = iface
                     , xmobarFlushObjectPath = flushObjPath
                     , xmobarFlushInterface  = flushIface
                     } ->
            [qm|\  xmobar object path: {objPath}\n
                \  xmobar bus name: {_xmobarBusView bus}\n
                \  xmobar interface name: {iface}\n
                \  xmobar object path for listening for flush requests:
                     \ {flushObjPath}\n
                \  xmobar interface name for listening for flush requests:
                     \ {flushIface}
                |]

        externalCtrlView = _if $
          \ExternalCtrlIPC { externalCtrlObjectPath = objPath
                           , externalCtrlBus        = bus
                           , externalCtrlInterface  = iface
                           } ->
            [qm|\  External control IPC object path: {objPath}\n
                \  External control IPC bus name: {bus}\n
                \  External control IPC interface name: {iface}
                |]
