-- Author: Viacheslav Lotsmanov
-- License: GPLv3 https://raw.githubusercontent.com/unclechu/xlib-keys-hack/master/LICENSE

{-# LANGUAGE OverloadedStrings #-}

module IPC
  ( IPCHandle
  , openIPC
  , closeIPC
  , setIndicatorState
  ) where

import "base" Data.Maybe (fromMaybe)

import "dbus" DBus ( ObjectPath
                   , objectPath_
                   , BusName
                   , busName_
                   , InterfaceName
                   , interfaceName_
                   , signal
                   , signalBody
                   , signalDestination
                   , toVariant
                   )
import "dbus" DBus.Client ( Client
                          , disconnect
                          , connectSession
                          , emit
                          )

-- local imports

import Utils.Sugar ((<&>))
import qualified Options as O
import Actions ( XmobarFlag ( XmobarNumLockFlag
                            , XmobarCapsLockFlag
                            , XmobarAlternativeFlag
                            )
               )

type Options = O.Options


data IPCHandle = IPCHandle { dbusClient       :: Client
                           , xmobarObjectPath :: ObjectPath
                           , xmobarBus        :: BusName
                           , xmobarInterface  :: InterfaceName
                           }


openIPC :: String -> Options -> IO IPCHandle
openIPC dpyName opts = connectSession <&> \dbusSession ->

  IPCHandle { dbusClient = dbusSession
            , xmobarObjectPath = fromMaybe "/" $
                O.xmobarIndicatorsObjPath opts <&> objectPath_
            , xmobarBus = busName_ $
                fromMaybe (xmobarBusNamePfx ++ encodeDpyName dpyName) $
                O.xmobarIndicatorsBusName opts
            , xmobarInterface = fromMaybe "com.github.unclechu.xmonadrc" $
                O.xmobarIndicatorsIface opts <&> interfaceName_
            }

  where xmobarBusNamePfx = "com.github.unclechu.xmonadrc."
        encodeDpyName = map f where f ':' = '_'; f '.' = '_'; f x = x


closeIPC :: IPCHandle -> IO ()
closeIPC = disconnect . dbusClient


setIndicatorState :: IPCHandle -> XmobarFlag -> IO ()
setIndicatorState IPCHandle { dbusClient = c
                            , xmobarObjectPath = path
                            , xmobarBus = bus
                            , xmobarInterface = iface
                            }
                  flag =

  emit c (signal path iface member)
           { signalDestination = Just bus
           , signalBody        = [toVariant isOn]
           }

  where (member, isOn) = case flag of
                              XmobarNumLockFlag     x -> ("numlock",     x)
                              XmobarCapsLockFlag    x -> ("capslock",    x)
                              XmobarAlternativeFlag x -> ("alternative", x)
