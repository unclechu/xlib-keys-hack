-- Author: Viacheslav Lotsmanov
-- License: GPLv3 https://raw.githubusercontent.com/unclechu/xlib-keys-hack/master/LICENSE

{-# LANGUAGE PackageImports #-}
{-# LANGUAGE OverloadedStrings #-}

module IPC
  ( IPCHandle
  , openIPC
  , closeIPC
  , setIndicatorState
  ) where

import "dbus" DBus ( ObjectPath
                   , BusName
                   , busName_
                   , InterfaceName
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
import Actions ( XmobarFlag ( XmobarNumLockFlag
                            , XmobarCapsLockFlag
                            , XmobarAlternativeFlag
                            )
               )


data IPCHandle = IPCHandle { dbusClient       :: Client
                           , xmobarObjectPath :: ObjectPath
                           , xmobarBus        :: BusName
                           , xmobarInterface  :: InterfaceName
                           }


openIPC :: String -> IO IPCHandle
openIPC dpyName = connectSession <&> \dbusSession ->

  IPCHandle { dbusClient = dbusSession
            , xmobarObjectPath = "/"
            , xmobarBus = busName_ $ xmobarBusNamePfx ++ encodeDpyName dpyName
            , xmobarInterface = xmobarInterfaceName
            }

  where xmobarBusNamePfx = "com.github.unclechu.xmonadrc."
        xmobarInterfaceName = "com.github.unclechu.xmonadrc"
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
