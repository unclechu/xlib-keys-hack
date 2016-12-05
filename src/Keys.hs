-- Author: Viacheslav Lotsmanov
-- License: GPLv3 https://raw.githubusercontent.com/unclechu/xlib-keys-hack/master/LICENSE

{-# LANGUAGE TemplateHaskell #-}

module Keys
  ( KeyName(..)
  , KeyAlias
  , KeyMap(..)

  , keyMap
  , getAliasByKey
  , getAlternative
  ) where

import Prelude hiding (lookup)

import Graphics.X11.Types (KeyCode)
import System.Linux.Input.Event (Key(Key))

import Data.Map.Strict (Map, fromList, lookup, empty, member, (!), insert)
import Data.Maybe (Maybe(Nothing, Just))
import Data.Word (Word16)

import Utils ((&), makeApoClassy)


data KeyName = EscapeKey

             | F1Key  | F2Key  | F3Key  | F4Key
             | F5Key  | F6Key  | F7Key  | F8Key
             | F9Key  | F10Key | F11Key | F12Key

             | PrintScreenKey | ScrollLockKey | PauseKey

             -- Apple keyboard additional fn keys
             | F13Key | F14Key | F15Key
             | F16Key | F17Key | F18Key | F19Key

             | GraveKey
             | Number1Key | Number2Key | Number3Key | Number4Key | Number5Key
             | Number6Key | Number7Key | Number8Key | Number9Key | Number0Key
             | MinusKey   | EqualKey   | BackSpaceKey

             | TabKey
             | QKey | WKey | EKey | RKey | TKey
             | YKey | UKey | IKey | OKey | PKey
             | BracketLeftKey | BracketRightKey | BackslashKey

             | CapsLockKey
             | AKey | SKey | DKey | FKey | GKey
             | HKey | JKey | KKey | LKey
             | SemicolonKey | ApostropheKey | EnterKey

             | ShiftLeftKey
             | LessKey -- Near left shift
             | ZKey | XKey | CKey | VKey | BKey | NKey | MKey
             | CommaKey | PeriodKey | SlashKey | ShiftRightKey

             | ControlLeftKey | SuperLeftKey | AltLeftKey
             | SpaceKey
             | AltRightKey | SuperRightKey | MenuKey | ControlRightKey

             | FNKey -- On apple keyboard
             | InsertKey | HomeKey | PageUpKey
             | DeleteKey | EndKey  | PageDownKey

             | ArrowLeftKey | ArrowRightKey | ArrowUpKey | ArrowDownKey

             | NumLockKey   | KPEqualKey   | KPDivideKey  | KPMultiplyKey
             | KPNumber7Key | KPNumber8Key | KPNumber9Key | KPSubtractKey
             | KPNumber4Key | KPNumber5Key | KPNumber6Key | KPAddKey
             | KPNumber1Key | KPNumber2Key | KPNumber3Key | KPEnterKey
             | KPNumber0Key | KPDecimalKey

             -- Real original pseudo-keys

             | RealMenuKey

             -- Media keys

             | MCalculatorKey
             | MEjectKey -- Can't get this key from evdev
             | MAudioMuteKey | MAudioLowerVolumeKey | MAudioRaiseVolumeKey
             | MAudioPlayKey | MAudioStopKey | MAudioPrevKey | MAudioNextKey
             | MMonBrightnessDownKey | MMonBrightnessUpKey

               deriving (Eq, Show, Ord)


type KeyAlias  = (KeyName, Word16, KeyCode)

defaultKeyAliases :: [KeyAlias]
defaultKeyAliases =
  [ (EscapeKey,       1,   9)

  , (F1Key,           59,  67)
  , (F2Key,           60,  68)
  , (F3Key,           61,  69)
  , (F4Key,           62,  70)

  , (F5Key,           63,  71)
  , (F6Key,           64,  72)
  , (F7Key,           65,  73)
  , (F8Key,           66,  74)

  , (F9Key,           67,  75)
  , (F10Key,          68,  76)
  , (F11Key,          87,  95)
  , (F12Key,          88,  96)

  , (PrintScreenKey,  99,  107)
  , (ScrollLockKey,   70,  78)
  , (PauseKey,        119, 127)


  -- Apple keyboard additional fn keys

  , (F13Key,          183, 191)
  , (F14Key,          184, 192)
  , (F15Key,          185, 193)

  , (F16Key,          186, 194)
  , (F17Key,          187, 195)
  , (F18Key,          188, 196)
  , (F19Key,          189, 197)


  -- Numbers line

  , (GraveKey,        41,  49)

  , (Number1Key,      2,   10)
  , (Number2Key,      3,   11)
  , (Number3Key,      4,   12)
  , (Number4Key,      5,   13)
  , (Number5Key,      6,   14)

  , (Number6Key,      7,   15)
  , (Number7Key,      8,   16)
  , (Number8Key,      9,   17)
  , (Number9Key,      10,  18)
  , (Number0Key,      11,  19)

  , (MinusKey,        12,  20)
  , (EqualKey,        13,  21)
  , (BackSpaceKey,    14,  22)


  -- QWER line

  , (TabKey,          15,  23)

  , (QKey,            16,  24)
  , (WKey,            17,  25)
  , (EKey,            18,  26)
  , (RKey,            19,  27)
  , (TKey,            20,  28)

  , (YKey,            21,  29)
  , (UKey,            22,  30)
  , (IKey,            23,  31)
  , (OKey,            24,  32)
  , (PKey,            25,  33)

  , (BracketLeftKey,  26,  34)
  , (BracketRightKey, 27,  35)
  , (BackslashKey,    43,  51)


  -- ASDF line

  , (CapsLockKey,     58,  66)

  , (AKey,            30,  38)
  , (SKey,            31,  39)
  , (DKey,            32,  40)
  , (FKey,            33,  41)
  , (GKey,            34,  42)

  , (HKey,            35,  43)
  , (JKey,            36,  44)
  , (KKey,            37,  45)
  , (LKey,            38,  46)

  , (SemicolonKey,    39,  47)
  , (ApostropheKey,   40,  48)
  , (EnterKey,        28,  36)


  -- ZXCV

  , (ShiftLeftKey,    42,  50)

  -- Near left shift.
  -- It's 94 but let's remap it as Left Shift.
  , (LessKey,         86,  50)

  , (ZKey,            44,  52)
  , (XKey,            45,  53)
  , (CKey,            46,  54)
  , (VKey,            47,  55)
  , (BKey,            48,  56)
  , (NKey,            49,  57)
  , (MKey,            50,  58)

  , (CommaKey,        51,  59)
  , (PeriodKey,       52,  60)
  , (SlashKey,        53,  61)
  , (ShiftRightKey,   54,  62)


  -- Bottom line

  , (ControlLeftKey,  29,  37)
  , (SuperLeftKey,    125, 133)
  , (AltLeftKey,      56,  64)

  , (SpaceKey,        57,  65)

  , (AltRightKey,     100, 108)
  , (SuperRightKey,   126, 134)
  , (MenuKey,         127, 134) -- 135 but remapped as Right Super
  , (ControlRightKey, 97,  105)


  -- Right block

  -- On apple keyboard (no X num, remap as Insert).
  -- Also it will be handled as Fn when is used with combo.
  , (FNKey,           464, 118)

  , (InsertKey,       110, 118)
  , (HomeKey,         102, 110)
  , (PageUpKey,       104, 112)

  , (DeleteKey,       111, 119)
  , (EndKey,          107, 115)
  , (PageDownKey,     109, 117)

  , (ArrowLeftKey,    105, 113)
  , (ArrowRightKey,   106, 114)
  , (ArrowUpKey,      103, 111)
  , (ArrowDownKey,    108, 116)


  -- Numeric keypad

  , (NumLockKey,      69,  77)
  , (KPEqualKey,      117, 125)
  , (KPDivideKey,     98,  106)
  , (KPMultiplyKey,   55,  63)

  , (KPNumber7Key,    71,  79)
  , (KPNumber8Key,    72,  80)
  , (KPNumber9Key,    73,  81)
  , (KPSubtractKey,   74,  82)

  , (KPNumber4Key,    75,  83)
  , (KPNumber5Key,    76,  84)
  , (KPNumber6Key,    77,  85)
  , (KPAddKey,        78,  86)

  , (KPNumber1Key,    79,  87)
  , (KPNumber2Key,    80,  88)
  , (KPNumber3Key,    81,  89)
  , (KPEnterKey,      96,  104)

  , (KPNumber0Key,    82,  90)
  , (KPDecimalKey,    83,  91)
  ]


realKeys :: [(KeyName, KeyCode)]
realKeys =
  [ (RealMenuKey, 135)
  ]


alternativeModeRemaps :: [(KeyName, KeyName)]
alternativeModeRemaps =
  [ (HKey,            ArrowLeftKey)
  , (JKey,            ArrowDownKey)
  , (KKey,            ArrowUpKey)
  , (LKey,            ArrowRightKey)

  , (UKey,            PageUpKey)
  , (DKey,            PageDownKey)

  , (FKey,            EndKey)
  , (BKey,            HomeKey)

  , (BracketLeftKey,  BackSpaceKey)
  , (BracketRightKey, DeleteKey)

  , (MKey,            RealMenuKey)
  , (IKey,            InsertKey)
  ]


getByNameMap :: [KeyAlias] -> Map KeyName KeyAlias
getByNameMap keyMap = fromList $ map f keyMap
  where f (name, devNum, xNum) = (name, (name, devNum, xNum))

getByDevNumMap :: [KeyAlias] -> Map Word16 KeyAlias
getByDevNumMap keyMap = fromList $ map f keyMap
  where f (name, devNum, xNum) = (devNum, (name, devNum, xNum))


data KeyMap =
  KeyMap { aliases              :: [KeyAlias]
         , byNameMap            :: Map KeyName KeyAlias
         , byDevNumMap          :: Map Word16  KeyAlias
         , byNameAlternativeMap :: Map KeyName (KeyName, KeyCode)
         }
  deriving (Show, Eq)


-- `moreAliases` supposed to contain media keys.
keyMap :: KeyMap
keyMap =
  KeyMap { aliases              = defaultKeyAliases
         , byNameMap            = nameMap
         , byDevNumMap          = getByDevNumMap defaultKeyAliases
         , byNameAlternativeMap = getAltMap alternativeModeRemaps empty
         }

  where nameMap :: Map KeyName KeyAlias
        nameMap = getByNameMap defaultKeyAliases

        realMap :: Map KeyName KeyCode
        realMap = fromList realKeys

        getAltMap :: [(KeyName, KeyName)]
                  -> Map KeyName (KeyName, KeyCode)
                  -> Map KeyName (KeyName, KeyCode)
        getAltMap [] altMap = altMap
        getAltMap ((nameFrom, nameTo):xs) altMap
          | nameTo `member` nameMap =
            let (_, _, keyCode) = nameMap ! nameTo
                in getAltMap xs $ insert nameFrom (nameTo, keyCode) altMap
          | otherwise = getAltMap xs
                      $ insert nameFrom (nameTo, realMap ! nameTo) altMap


getAliasByKey :: KeyMap -> Key -> Maybe KeyAlias
getAliasByKey keyMap devKey =
  fromKey devKey `lookup` byDevNumMap keyMap
  where fromKey :: Key -> Word16
        fromKey (Key x) = x

getAlternative :: KeyMap -> KeyName -> Maybe (KeyName, KeyCode)
getAlternative keyMap keyName =
  case keyName `lookup` byNameAlternativeMap keyMap of
       Just x  -> Just x
       Nothing -> Nothing


makeApoClassy ''KeyMap
