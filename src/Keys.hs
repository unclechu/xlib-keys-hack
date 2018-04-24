-- Author: Viacheslav Lotsmanov
-- License: GPLv3 https://raw.githubusercontent.com/unclechu/xlib-keys-hack/master/LICENSE

{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveGeneric #-}

module Keys
  ( KeyName(..)
  , KeyAlias
  , KeyMap

  , getKeyMap
  , getAliasByKey
  , getKeyCodeByName
  , getRealKeyCodeByName

  , getAlternative
  , isAlternative

  , getMedia
  , isMedia

  , getAsName
  , lookupAsName
  , maybeAsName
  , getRemappedByName
  , getExtraKeys
  ) where

import Prelude hiding (lookup)

import "base" GHC.Generics (Generic)

import "X11" Graphics.X11.Types (KeyCode)
import "linux-evdev" System.Linux.Input.Event (Key (Key))

import "deepseq" Control.DeepSeq (NFData, rnf, deepseq)

import "containers" Data.Map.Strict ( Map, (!), fromList, toList
                                    , lookup, empty, member, insert
                                    )
import qualified "containers" Data.Map.Strict as Map
import qualified "containers" Data.Set as Set
import "base" Data.List (find)
import "base" Data.Word (Word16)
import "base" Data.Tuple (swap)
import "base" Data.Maybe (fromMaybe, fromJust)
import "base" Data.Monoid ((<>))
import "qm-interpolated-string" Text.InterpolatedString.QM (qm)

-- local imports

import Utils.Sugar ((.>), (&), (<&>), applyIf)
import Utils.Lens (makeApoClassy)
import qualified Options as O


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

             | RealMenuKey | RealCapsLockKey | RealControlRightKey

             -- Media keys

             | MCalculatorKey | MEjectKey
             | MAudioMuteKey | MAudioLowerVolumeKey | MAudioRaiseVolumeKey
             | MAudioPlayKey | MAudioStopKey | MAudioPrevKey | MAudioNextKey
             | MMonBrightnessDownKey | MMonBrightnessUpKey

               deriving (Eq, Show, Ord, Generic)

instance NFData KeyName


type KeyAlias = (KeyName, Word16, KeyCode)

-- Mappings between keys names and device key codes
-- and X key codes to trigger them.
-- Also with custom remaps for specific keys.
defaultKeyAliases :: [KeyAlias]
defaultKeyAliases =
  [ (EscapeKey,       1,   escapeKeyCode)

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

  , (CapsLockKey,     58,  escapeKeyCode) -- Remapped (see `realKeys`)

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
  , (SuperRightKey,   126, rightSuperKeyCode)
  , (MenuKey,         127, rightSuperKeyCode) -- Remapped (see `realKeys`)
  , (ControlRightKey, 97,  rightControlKeyCode)


  -- Right block

  -- On apple keyboard (no X num, remap as Insert).
  -- Also it will be handled as Fn when is used with media keys.
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


rightControlKeyCode = 105 ; rightControlKeyCode :: KeyCode
{-# INLINE rightControlKeyCode #-}
rightSuperKeyCode   = 134 ; rightSuperKeyCode   :: KeyCode
{-# INLINE rightSuperKeyCode #-}
escapeKeyCode       = 9   ; escapeKeyCode       :: KeyCode
{-# INLINE escapeKeyCode #-}


-- Real standard keys aliases to X key-codes to trigger them
-- if they were remapped to another keys.
realKeys :: [(KeyName, KeyCode)]
realKeys =
  [ (RealMenuKey,         135)
  , (RealCapsLockKey,     66)
  , (RealControlRightKey, rightControlKeyCode)
  ]


-- Remapping for keys when Alternative mode is turned on
alternativeModeRemaps :: [(KeyName, KeyName)]
alternativeModeRemaps =
  [ (HKey,            ArrowLeftKey)
  , (JKey,            ArrowDownKey)
  , (KKey,            ArrowUpKey)
  , (LKey,            ArrowRightKey)

  , (YKey,            PageDownKey)
  , (UKey,            PageUpKey)

  , (CommaKey,        HomeKey)
  , (PeriodKey,       EndKey)

  , (BracketLeftKey,  BackSpaceKey)
  , (BracketRightKey, DeleteKey)

  , (MKey,            RealMenuKey)
  , (IKey,            InsertKey)
  ]


-- Aliases for media keys
mediaDevNums :: [(KeyName, Word16)]
mediaDevNums =
  [ (MCalculatorKey,        140)
  , (MEjectKey,             161)

  , (MAudioMuteKey,         113)
  , (MAudioLowerVolumeKey,  114)
  , (MAudioRaiseVolumeKey,  115)

  , (MAudioPlayKey,         164)
  , (MAudioStopKey,         166)
  , (MAudioPrevKey,         165)
  , (MAudioNextKey,         163)

  , (MMonBrightnessDownKey, 224)
  , (MMonBrightnessUpKey,   225)
  ]


-- Aliases that indicates that specific key remapped to another.
-- If you want for example get all Shift keys you must also check this mapping
-- for alias, because for example `LessKey` is remapped to `ShiftLeftKey`,
-- to `LessKey` is Shift key too.
-- TODO Rename it since it's not just about "names"
--      but also marks remapped keys.
asNames :: [(KeyName, KeyName)]
asNames =
  [ (FNKey,       InsertKey)
  , (CapsLockKey, EscapeKey)
  , (LessKey,     ShiftLeftKey)
  , (MenuKey,     SuperRightKey)
  ]


numericShift :: Map.Map KeyName KeyName
numericShift = Map.fromList
  [ (Number1Key, MinusKey)
  , (Number2Key, Number1Key)
  , (Number3Key, Number2Key)
  , (Number4Key, Number3Key)
  , (Number5Key, Number4Key)
  , (Number6Key, Number5Key)
  , (Number7Key, Number6Key)
  , (Number8Key, Number7Key)
  , (Number9Key, Number8Key)
  , (Number0Key, Number9Key)
  , (MinusKey,   Number0Key)
  ]


-- Returns Map of aliases list using key name as a Map key
getByNameMap :: [KeyAlias] -> Map KeyName KeyAlias
getByNameMap keyMap = fromList $ fmap f keyMap
  where f (name, devNum, xNum) = (name, (name, devNum, xNum))

-- Returns Map of aliases list using device key code as a Map key
getByDevNumMap :: [KeyAlias] -> Map Word16 KeyAlias
getByDevNumMap keyMap = fromList $ fmap f keyMap
  where f (name, devNum, xNum) = (devNum, (name, devNum, xNum))


data KeyMap =
  KeyMap { byNameMap            :: Map KeyName KeyAlias
         , byDevNumMap          :: Map Word16  KeyAlias
         , byNameAlternativeMap :: Map KeyName (KeyName, KeyCode)
         , byNameMediaMap       :: Map KeyName KeyCode
         , byNameRealMap        :: Map KeyName KeyCode
         , asNamesMap           :: Map KeyName KeyName
         , extraByRemaps        :: [(KeyName, KeyName)]
         }
  deriving (Show, Eq, Generic)

instance NFData KeyMap where
  rnf x =
    byNameMap            x `deepseq`
    byDevNumMap          x `deepseq`
    byNameAlternativeMap x `deepseq`
    byNameMediaMap       x `deepseq`
    byNameRealMap        x `deepseq`
    asNamesMap           x `deepseq`
    extraByRemaps        x `deepseq`
      ()


-- `moreAliases` supposed to contain media keys
getKeyMap :: O.Options -> [(KeyName, KeyCode)] -> KeyMap
getKeyMap opts mediaKeyAliases =
  KeyMap { byNameMap            = nameMap
         , byDevNumMap          = getByDevNumMap keyAliases
         , byNameAlternativeMap = getAltMap alternativeModeRemaps empty
         , byNameMediaMap       = byNameMediaAliasesMap
         , byNameRealMap        = realMap
         , asNamesMap           = _asNamesMap
         , extraByRemaps        = fmap swap _asNames
         }

  where keyAliases :: [KeyAlias]
        keyAliases =
          let
            resolveMediaKey (keyName, keyCode) =
              case keyName `lookup` mediaMap of
                   Just devNum -> (keyName, devNum, keyCode)
                   Nothing -> error [qm| Unexpected media key: {keyName} |]

            makeCapsLockReal = fmap f
              where f (CapsLockKey, devNum, _) =
                      (CapsLockKey, devNum, realMap ! RealCapsLockKey)
                    f x = x

            shiftNumeric aliases = fmap f aliases
              where f alias@(keyName, devNum, _)
                      = maybe alias (resolve devNum)
                      $ keyName `Map.lookup` numericShift

                    resolve devNum keyName =
                      find (\(x, _, _) -> x == keyName) aliases
                        & fromJust
                        & (\(toName, _, toCode) -> (toName, devNum, toCode))

            controlAsSuper = fmap f
              where f (ControlRightKey, devNum, _) =
                      (ControlRightKey, devNum, rightSuperKeyCode)
                    f x = x
          in
            defaultKeyAliases <> fmap resolveMediaKey mediaKeyAliases
              & makeCapsLockReal `applyIf` O.realCapsLock             opts
              & shiftNumeric     `applyIf` O.shiftNumericKeys         opts
              & controlAsSuper   `applyIf` O.rightControlAsRightSuper opts

        _asNamesMap = fromList _asNames :: Map KeyName KeyName

        _asNames :: [(KeyName, KeyName)]
        _asNames = asNames
          & filter (fst .> (/= CapsLockKey)) `applyIf` O.realCapsLock opts
          & ((ControlRightKey, SuperRightKey) :)
              `applyIf` O.rightControlAsRightSuper opts

        byNameMediaAliasesMap :: Map KeyName KeyCode
        byNameMediaAliasesMap =
          fromList [(a, b) | (a, _, b) <- keyAliases, a `member` mediaMap]

        mediaMap = fromList mediaDevNums   :: Map KeyName Word16
        nameMap  = getByNameMap keyAliases :: Map KeyName KeyAlias
        realMap  = fromList realKeys       :: Map KeyName KeyCode

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
getAlternative keyMap keyName = keyName `lookup` byNameAlternativeMap keyMap

-- Check if key could be alternatively remapped
isAlternative :: KeyMap -> KeyName -> Bool
isAlternative keyMap keyName = keyName `member` byNameAlternativeMap keyMap


getAsName :: KeyMap -> KeyName -> KeyName
getAsName keyMap keyName = asNamesMap keyMap ! keyName

lookupAsName :: KeyMap -> KeyName -> Maybe KeyName
lookupAsName keyMap keyName = lookup keyName $ asNamesMap keyMap

maybeAsName :: KeyMap -> KeyName -> KeyName
maybeAsName keyMap keyName = fromMaybe keyName $ lookupAsName keyMap keyName

getRemappedByName :: KeyMap -> KeyName -> Set.Set KeyName
getRemappedByName keyMap keyName =
  Map.filter (== keyName) (asNamesMap keyMap)
    & toList & fmap fst & Set.fromList

-- Gets specific key and returns a Set that contains extra keys
-- aliased as this specific key (you get a Set of extra synonyms keys).
-- For example `LessKey` remapped as `ShiftLeftKey` and if you need to
-- handle in some way `ShiftLeftKey` you should also handle `LessKey`
-- same way, because they're same keys, they're kinda synonyms,
-- so, you could try this:
--   `getExtraKeys keyMap ShiftLeftKey`
-- and then you get a Set of extra aliases for this key like this:
--   `Set.fromList [LessKey]`
-- (this Set can contaion more than one aliases).
getExtraKeys :: KeyMap -> KeyName -> Set.Set KeyName
getExtraKeys (extraByRemaps -> extra) keyName =
  Set.fromList . fmap snd . filter ((== keyName) . fst) $ extra


-- Check if key is media key
isMedia :: KeyMap -> KeyName -> Bool
isMedia keyMap keyName = keyName `member` byNameMediaMap keyMap

getMedia :: KeyMap -> KeyName -> Maybe KeyCode
getMedia keyMap keyName = keyName `lookup` byNameMediaMap keyMap


-- Get mapped `KeyCode` to `KeyName`
getKeyCodeByName :: KeyMap -> KeyName -> Maybe KeyCode
getKeyCodeByName keyMap keyName =
  keyName `lookup` byNameMap keyMap <&> \(_, _, x) -> x


getRealKeyCodeByName :: KeyMap -> KeyName -> Maybe KeyCode
getRealKeyCodeByName keyMap keyName = keyName `lookup` byNameRealMap keyMap


makeApoClassy ''KeyMap
