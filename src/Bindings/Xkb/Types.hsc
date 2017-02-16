-- Author: Viacheslav Lotsmanov
-- License: GPLv3 https://raw.githubusercontent.com/unclechu/xlib-keys-hack/master/LICENSE

{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE PackageImports #-}

module Bindings.Xkb.Types
  ( KeyCode
  , XkbModsRec(..)
  , XkbDescRec(..)
  , XkbControlsRec(..)
  , XkbStateRec(..)
  ) where

import "base" Foreign
import "base" Foreign.Storable as Storable
import qualified "base" Foreign.C.Types as CTypes

import "X11" Graphics.X11.Xlib.Types (Display)


#include <X11/XKBlib.h>


type KeyCode = CTypes.CUChar



data XkbModsRec = XkbModsRec { mask      :: CTypes.CUChar
                             , real_mods :: CTypes.CUChar
                             , vmods     :: CTypes.CUShort
                             } deriving (Show)

instance Storable.Storable XkbModsRec where
  sizeOf _ = (#size XkbModsRec)
  alignment _ = Storable.alignment (undefined :: CTypes.CUShort)
  peek ptr = XkbModsRec
               <$> (#peek XkbModsRec, mask)      ptr
               <*> (#peek XkbModsRec, real_mods) ptr
               <*> (#peek XkbModsRec, vmods)     ptr
  poke ptr ( XkbModsRec
             v_mask
             v_real_mods
             v_vmods
           ) = do
    (#poke XkbModsRec, mask)      ptr v_mask
    (#poke XkbModsRec, real_mods) ptr v_real_mods
    (#poke XkbModsRec, vmods)     ptr v_vmods



data XkbControlsRec = XkbControlsRec { mk_dflt_btn      :: CTypes.CUChar
                                     , num_groups       :: CTypes.CUChar
                                     , groups_wrap      :: CTypes.CUChar
                                     , internal         :: XkbModsRec
                                     , ignore_lock      :: XkbModsRec
                                     , enabled_ctrls    :: CTypes.CUInt
                                     , repeat_delay     :: CTypes.CUShort
                                     , repeat_interval  :: CTypes.CUShort
                                     , slow_keys_delay  :: CTypes.CUShort
                                     , debounce_delay   :: CTypes.CUShort
                                     , mk_delay         :: CTypes.CUShort
                                     , mk_interval      :: CTypes.CUShort
                                     , mk_time_to_max   :: CTypes.CUShort
                                     , mk_max_speed     :: CTypes.CUShort
                                     , mk_curve         :: CTypes.CShort
                                     , ax_options       :: CTypes.CUShort
                                     , ax_timeout       :: CTypes.CUShort
                                     , axt_opts_mask    :: CTypes.CUShort
                                     , axt_opts_values  :: CTypes.CUShort
                                     , axt_ctrls_mask   :: CTypes.CUInt
                                     , axt_ctrls_values :: CTypes.CUInt
                                     , per_key_repeat   :: [CTypes.CUChar]
                                     } deriving (Show)

instance Storable.Storable XkbControlsRec where
  sizeOf _ = (#size XkbControlsRec)
  alignment _ = Storable.alignment (undefined :: CTypes.CUShort)

  peek ptr = XkbControlsRec
               <$> (#peek XkbControlsRec, mk_dflt_btn)      ptr
               <*> (#peek XkbControlsRec, num_groups)       ptr
               <*> (#peek XkbControlsRec, groups_wrap)      ptr
               <*> (#peek XkbControlsRec, internal)         ptr
               <*> (#peek XkbControlsRec, ignore_lock)      ptr
               <*> (#peek XkbControlsRec, enabled_ctrls)    ptr
               <*> (#peek XkbControlsRec, repeat_delay)     ptr
               <*> (#peek XkbControlsRec, repeat_interval)  ptr
               <*> (#peek XkbControlsRec, slow_keys_delay)  ptr
               <*> (#peek XkbControlsRec, debounce_delay)   ptr
               <*> (#peek XkbControlsRec, mk_delay)         ptr
               <*> (#peek XkbControlsRec, mk_interval)      ptr
               <*> (#peek XkbControlsRec, mk_time_to_max)   ptr
               <*> (#peek XkbControlsRec, mk_max_speed)     ptr
               <*> (#peek XkbControlsRec, mk_curve)         ptr
               <*> (#peek XkbControlsRec, ax_options)       ptr
               <*> (#peek XkbControlsRec, ax_timeout)       ptr
               <*> (#peek XkbControlsRec, axt_opts_mask)    ptr
               <*> (#peek XkbControlsRec, axt_opts_values)  ptr
               <*> (#peek XkbControlsRec, axt_ctrls_mask)   ptr
               <*> (#peek XkbControlsRec, axt_ctrls_values) ptr

               <*> -- unsigned char per_key_repeat[XkbPerKeyBitArraySize];
                   peekArray (#const XkbPerKeyBitArraySize)
                             ((#ptr XkbControlsRec, per_key_repeat) ptr)

  poke ptr ( XkbControlsRec
             v_mk_dflt_btn
             v_num_groups
             v_groups_wrap
             v_internal
             v_ignore_lock
             v_enabled_ctrls
             v_repeat_delay
             v_repeat_interval
             v_slow_keys_delay
             v_debounce_delay
             v_mk_delay
             v_mk_interval
             v_mk_time_to_max
             v_mk_max_speed
             v_mk_curve
             v_ax_options
             v_ax_timeout
             v_axt_opts_mask
             v_axt_opts_values
             v_axt_ctrls_mask
             v_axt_ctrls_values
             v_per_key_repeat
           ) = do
    (#poke XkbControlsRec, mk_dflt_btn)      ptr v_mk_dflt_btn
    (#poke XkbControlsRec, num_groups)       ptr v_num_groups
    (#poke XkbControlsRec, groups_wrap)      ptr v_groups_wrap
    (#poke XkbControlsRec, internal)         ptr v_internal
    (#poke XkbControlsRec, ignore_lock)      ptr v_ignore_lock
    (#poke XkbControlsRec, enabled_ctrls)    ptr v_enabled_ctrls
    (#poke XkbControlsRec, repeat_delay)     ptr v_repeat_delay
    (#poke XkbControlsRec, repeat_interval)  ptr v_repeat_interval
    (#poke XkbControlsRec, slow_keys_delay)  ptr v_slow_keys_delay
    (#poke XkbControlsRec, debounce_delay)   ptr v_debounce_delay
    (#poke XkbControlsRec, mk_delay)         ptr v_mk_delay
    (#poke XkbControlsRec, mk_interval)      ptr v_mk_interval
    (#poke XkbControlsRec, mk_time_to_max)   ptr v_mk_time_to_max
    (#poke XkbControlsRec, mk_max_speed)     ptr v_mk_max_speed
    (#poke XkbControlsRec, mk_curve)         ptr v_mk_curve
    (#poke XkbControlsRec, ax_options)       ptr v_ax_options
    (#poke XkbControlsRec, ax_timeout)       ptr v_ax_timeout
    (#poke XkbControlsRec, axt_opts_mask)    ptr v_axt_opts_mask
    (#poke XkbControlsRec, axt_opts_values)  ptr v_axt_opts_values
    (#poke XkbControlsRec, axt_ctrls_mask)   ptr v_axt_ctrls_mask
    (#poke XkbControlsRec, axt_ctrls_values) ptr v_axt_ctrls_values

    -- unsigned char per_key_repeat[XkbPerKeyBitArraySize];
    pokeArray ((#ptr XkbControlsRec, per_key_repeat) ptr)
              (take (#const XkbPerKeyBitArraySize) (v_per_key_repeat ++ repeat 0))



data XkbDescRec = XkbDescRec { dpy          :: Ptr Display
                             , flags        :: CTypes.CUShort
                             , device_spec  :: CTypes.CUShort
                             , min_key_code :: KeyCode
                             , max_key_code :: KeyCode

                             , ctrls        :: Ptr XkbControlsRec

                             -- not declared yet
                             , server       :: Ptr () -- :: TODO Ptr XkbServerMapRec
                             , map          :: Ptr () -- :: TODO Ptr XkbNamesRec
                             , indicators   :: Ptr () -- :: TODO Ptr XkbIndicatorRec
                             , names        :: Ptr () -- :: TODO Ptr XkbClientMapRec
                             , compat       :: Ptr () -- :: TODO Ptr XkbGeometryRec
                             , geom         :: Ptr () -- :: TODO Ptr XkbCompatMapRec
                             } deriving (Show)

instance Storable.Storable XkbDescRec where
  sizeOf _ = (#size XkbDescRec)
  alignment _ = Storable.alignment (undefined :: CTypes.CUShort)
  peek ptr = XkbDescRec
               <$> (#peek XkbDescRec, dpy)          ptr
               <*> (#peek XkbDescRec, flags)        ptr
               <*> (#peek XkbDescRec, device_spec)  ptr
               <*> (#peek XkbDescRec, min_key_code) ptr
               <*> (#peek XkbDescRec, max_key_code) ptr
               <*> (#peek XkbDescRec, ctrls)        ptr

               <*> (#peek XkbDescRec, server)       ptr
               <*> (#peek XkbDescRec, map)          ptr
               <*> (#peek XkbDescRec, indicators)   ptr
               <*> (#peek XkbDescRec, names)        ptr
               <*> (#peek XkbDescRec, compat)       ptr
               <*> (#peek XkbDescRec, geom)         ptr

  poke ptr ( XkbDescRec
             v_dpy
             v_flags
             v_device_spec
             v_min_key_code
             v_max_key_code
             v_ctrls

             v_server
             v_map
             v_indicators
             v_names
             v_compat
             v_geom
           ) = do
    (#poke XkbDescRec, dpy)          ptr v_dpy
    (#poke XkbDescRec, flags)        ptr v_flags
    (#poke XkbDescRec, device_spec)  ptr v_device_spec
    (#poke XkbDescRec, min_key_code) ptr v_min_key_code
    (#poke XkbDescRec, max_key_code) ptr v_max_key_code
    (#poke XkbDescRec, ctrls)        ptr v_ctrls

    (#poke XkbDescRec, server)       ptr v_server
    (#poke XkbDescRec, map)          ptr v_map
    (#poke XkbDescRec, indicators)   ptr v_indicators
    (#poke XkbDescRec, names)        ptr v_names
    (#poke XkbDescRec, compat)       ptr v_compat
    (#poke XkbDescRec, geom)         ptr v_geom



data XkbStateRec = XkbStateRec { group              :: CTypes.CUChar
                               , locked_group       :: CTypes.CUChar
                               , base_group         :: CTypes.CUShort
                               , latched_group      :: CTypes.CUShort
                               , mods               :: CTypes.CUChar
                               , base_mods          :: CTypes.CUChar
                               , latched_mods       :: CTypes.CUChar
                               , locked_mods        :: CTypes.CUChar
                               , compat_state       :: CTypes.CUChar
                               , grab_mods          :: CTypes.CUChar
                               , compat_grab_mods   :: CTypes.CUChar
                               , lookup_mods        :: CTypes.CUChar
                               , compat_lookup_mods :: CTypes.CUChar
                               , ptr_buttons        :: CTypes.CUShort
                               } deriving (Show)

instance Storable.Storable XkbStateRec where
  sizeOf _ = (#size XkbStateRec)
  alignment _ = Storable.alignment (undefined :: CTypes.CUShort)
  peek ptr = XkbStateRec
               <$> (#peek XkbStateRec, group)              ptr
               <*> (#peek XkbStateRec, locked_group)       ptr
               <*> (#peek XkbStateRec, base_group)         ptr
               <*> (#peek XkbStateRec, latched_group)      ptr
               <*> (#peek XkbStateRec, mods)               ptr
               <*> (#peek XkbStateRec, base_mods)          ptr
               <*> (#peek XkbStateRec, latched_mods)       ptr
               <*> (#peek XkbStateRec, locked_mods)        ptr
               <*> (#peek XkbStateRec, compat_state)       ptr
               <*> (#peek XkbStateRec, grab_mods)          ptr
               <*> (#peek XkbStateRec, compat_grab_mods)   ptr
               <*> (#peek XkbStateRec, lookup_mods)        ptr
               <*> (#peek XkbStateRec, compat_lookup_mods) ptr
               <*> (#peek XkbStateRec, ptr_buttons)        ptr
  poke ptr ( XkbStateRec
             v_group
             v_locked_group
             v_base_group
             v_latched_group
             v_mods
             v_base_mods
             v_latched_mods
             v_locked_mods
             v_compat_state
             v_grab_mods
             v_compat_grab_mods
             v_lookup_mods
             v_compat_lookup_mods
             v_ptr_buttons
           ) = do
    (#poke XkbStateRec, group)              ptr v_group
    (#poke XkbStateRec, locked_group)       ptr v_locked_group
    (#poke XkbStateRec, base_group)         ptr v_base_group
    (#poke XkbStateRec, latched_group)      ptr v_latched_group
    (#poke XkbStateRec, mods)               ptr v_mods
    (#poke XkbStateRec, base_mods)          ptr v_base_mods
    (#poke XkbStateRec, latched_mods)       ptr v_latched_mods
    (#poke XkbStateRec, locked_mods)        ptr v_locked_mods
    (#poke XkbStateRec, compat_state)       ptr v_compat_state
    (#poke XkbStateRec, grab_mods)          ptr v_grab_mods
    (#poke XkbStateRec, compat_grab_mods)   ptr v_compat_grab_mods
    (#poke XkbStateRec, lookup_mods)        ptr v_lookup_mods
    (#poke XkbStateRec, compat_lookup_mods) ptr v_compat_lookup_mods
    (#poke XkbStateRec, ptr_buttons)        ptr v_ptr_buttons
