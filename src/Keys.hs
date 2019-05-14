-- Author: Viacheslav Lotsmanov
-- License: GPLv3 https://raw.githubusercontent.com/unclechu/xlib-keys-hack/master/LICENSE

{-# LANGUAGE DeriveGeneric, DeriveAnyClass, OverloadedLists #-}
{-# LANGUAGE TemplateHaskell, TupleSections, FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Keys
     ( KeyName (..)
     , KeyMap

     , getKeyMap
     , getAliasByKeyDevNum
     , getKeyCodeByName
     , getDefaultKeyCodeByName

     , getAlternativeRemapByName
     , hasAlternativeRemap

     , getMediaKeyCode
     , isMediaKey

     , getRemapByName
     , getRemappedByName
     , getExtraKeys
     ) where

import Prelude hiding (lookup)

import "base" GHC.Generics (Generic)

import "containers" Data.Set (type Set)
import "containers" Data.Map.Strict (type Map, lookup, member, insert, delete)
import qualified "containers" Data.Map.Strict as Map
import qualified "containers" Data.Set as Set
import "base" Data.List (find)
import "base" Data.Word (Word16)
import "base" Data.Tuple (swap)
import "qm-interpolated-string" Text.InterpolatedString.QM (qm, qms)

import "base" Control.Arrow ((&&&))
import "lens" Control.Lens ((&~), (.=), view, use, set, _1, _2, _3)
import "base" Control.Applicative ((<**>))
import "base" Control.Monad (join)
import "extra" Control.Monad.Extra (whenM)
import "mtl" Control.Monad.Except (MonadError (throwError))
import "deepseq" Control.DeepSeq (NFData)
import "type-operators" Control.Type.Operator (type ($))

import "X11" Graphics.X11.Types (KeyCode)


-- local imports

import Utils.Sugar ((.>), (&), (<&>), (?), applyIf)
import Utils.Lens (makeApoClassy)
import qualified Options as O


data KeyName
   = EscapeKey

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

   -- Media keys

   | MCalculatorKey | MEjectKey
   | MAudioMuteKey | MAudioLowerVolumeKey | MAudioRaiseVolumeKey
   | MAudioPlayKey | MAudioStopKey | MAudioPrevKey | MAudioNextKey
   | MMonBrightnessDownKey | MMonBrightnessUpKey

     deriving (Eq, Show, Ord, Generic, NFData)


-- | Use @0@ ("minBound") for "Word16" (second tuple item) as a plug
--   (to turn key off on a keyboard).
--
-- But override it to "minBound" only inside "getKeyMap".
type KeyAlias = (KeyName, Word16, KeyCode)

-- | Mappings between keys names and device key codes
--   and X key codes to trigger them.
--
-- This mapping supposed to be as standard as possible, without any remaps,
-- it will be taken as a source of original key codes for actual remaps.
defaultKeyAliases :: Set KeyAlias
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

  -- An additional key on some keyboards between "Left Shift" and "Z".
  -- Could be found on Russian version of Apple Magic Keyboard.
  , (LessKey,         86,  94)

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
  , (MenuKey,         127, 135)
  , (ControlRightKey, 97,  105)


  -- Right block

  -- On apple keyboard (no X num).
  -- Also it will be handled as Fn when is used with media keys.
  , (FNKey,           464, minBound)

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


-- | A map of keys which supposed to act like another keys.
--
-- It could be extended in "getKeyMap" depending on provided "O.Options".
--
-- To remap a key you replace "KeyCode" in original mapping inside "getKeyMap".
basicKeyRemapping :: Map KeyName KeyName
basicKeyRemapping =
  [ (FNKey,       InsertKey)
  , (CapsLockKey, EscapeKey)
  , (LessKey,     ShiftLeftKey)
  , (MenuKey,     SuperRightKey)
  ]


-- | Remapping for keys when Alternative mode is turned on
alternativeModeRemaps :: Map KeyName KeyName
alternativeModeRemaps =
  -- 4th row shifted down to 3rd
  [ (TabKey,          GraveKey)
  , (QKey,            Number1Key)
  , (WKey,            Number2Key)
  , (EKey,            Number3Key)
  , (RKey,            Number4Key)
  , (TKey,            Number5Key)
  , (YKey,            Number6Key)
  , (UKey,            Number7Key)
  , (IKey,            Number8Key)
  , (OKey,            Number9Key)
  , (PKey,            Number0Key)
  , (BracketLeftKey,  MinusKey)
  , (BracketRightKey, EqualKey)
  , (BackslashKey,    BackSpaceKey)

  -- Arrow keys
  , (HKey,            ArrowLeftKey)
  , (JKey,            ArrowDownKey)
  , (KKey,            ArrowUpKey)
  , (LKey,            ArrowRightKey)

  -- Delete backward (BS) and forward (Del)
  , (SemicolonKey,    BackSpaceKey)
  , (ApostropheKey,   DeleteKey)

  -- Insert key
  , (NKey,            InsertKey)

  -- Home, PgDown, PgUp, End
  -- Symmetric to @hjkl@:
  --   Home/End    = Left/Right = j;
  --   PgDown/PgUp = Down/Up    = kl
  , (MKey,            HomeKey)
  , (CommaKey,        PageDownKey)
  , (PeriodKey,       PageUpKey)
  , (SlashKey,        EndKey)

  -- TODO move this key to second alternative mode level
  , (BKey,            MenuKey)
  ]


-- | Device key numbers aliases for media keys
mediaDevNums :: Map KeyName Word16
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


numericShift :: Map KeyName KeyName
numericShift =
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


hjklShift :: Map KeyName KeyName
hjklShift =
  [ (HKey,         SemicolonKey)
  , (JKey,         HKey)
  , (KKey,         JKey)
  , (LKey,         KKey)
  , (SemicolonKey, LKey)
  ]


fourhRow :: [KeyName]
fourhRow =
  [ GraveKey
  , Number1Key , Number2Key , Number3Key , Number4Key , Number5Key
  , Number6Key , Number7Key , Number8Key , Number9Key , Number0Key
  , MinusKey   , EqualKey   , BackSpaceKey
  ]


-- | Returns "Map" of aliases list using key name as a "Map" key
getByNameMap :: Set KeyAlias -> Map KeyName KeyAlias
getByNameMap = Map.fromList . Set.toList . Set.map f where
  f (name, devNum, xNum) = (name, (name, devNum, xNum))

-- | Returns "Map" of aliases list using device key code as a "Map" key
getByDevNumMap :: Set KeyAlias -> Map Word16 KeyAlias
getByDevNumMap = Map.fromList . Set.toList . Set.map f where
  f (name, devNum, xNum) = (devNum, (name, devNum, xNum))


data KeyMap
   = KeyMap
   { byNameDefaultKeyCode :: Map KeyName KeyCode
   , byNameMap            :: Map KeyName KeyAlias
   , byDevNumMap          :: Map Word16  KeyAlias
   , byNameAlternativeMap :: Map KeyName (KeyName, KeyCode)
   , byNameMediaMap       :: Map KeyName KeyCode
   , byNameRemaps         :: Map KeyName KeyName
   , extraByRemaps        :: Set (KeyName, KeyName)
   } deriving (Show, Eq, Generic, NFData)


-- | @moreAliases@ supposed to contain media keys.
--
-- Accept "Map" instead of list of pairs.
getKeyMap
  :: forall m. MonadError String m
  => O.Options
  -> Map KeyName KeyCode -- ^ Obtained media key codes from X server
  -> m KeyMap

getKeyMap opts mediaKeyCodes = go where
  go = do
    defaultKeyCodes'         <- defaultKeyCodes
    nameMap'                 <- nameMap
    devMap'                  <- devMap
    alternativeModeKeyCodes' <- alternativeModeKeyCodes
    byNameMediaAliasesMap'   <- byNameMediaAliasesMap

    pure
      $ KeyMap
      { byNameDefaultKeyCode = defaultKeyCodes'
      , byNameMap            = nameMap'
      , byDevNumMap          = devMap'
      , byNameAlternativeMap = alternativeModeKeyCodes'
      , byNameMediaMap       = byNameMediaAliasesMap'
      , byNameRemaps         = remaps
      , extraByRemaps        = remapsMirror
      }

  keyAliases :: m $ Set KeyAlias
  keyAliases = go' where
    go' = pure defaultKeyAliases
        <**> fmap mappend resolvedMediaKeys
        <**> pure (turnOffFourthRow `applyIf` O.turnOffFourthRow opts)

    resolvedMediaKeys =
      Set.fromList . Map.elems <$>
        Map.traverseWithKey mediaKeysReducer mediaKeyCodes

    mediaKeysReducer keyName keyCode
      = throwError [qm| Unexpected media key: {keyName} |] `maybe` pure
      $ (keyName,,keyCode) <$> keyName `lookup` mediaDevNums

    turnOffFourthRow =
      Set.map (&~ do whenM (use _1 <&> (`elem` fourhRow)) $ _2 .= minBound)

  defaultKeyCodes :: m $ Map KeyName KeyCode
  defaultKeyCodes =
    keyAliases <&> Map.fromList . Set.toList . Set.map (view _1 &&& view _3)

  remaps :: Map KeyName KeyName
  remaps = f basicKeyRemapping where
    f = (delete CapsLockKey `applyIf` O.realCapsLock             opts)
     .> (rightCtrlAsSuper   `applyIf` O.rightControlAsRightSuper opts)
     .> ((<> numericShift)  `applyIf` O.shiftNumericKeys         opts)
     .> ((<> hjklShift)     `applyIf` O.shiftHJKLKeys            opts)

    rightCtrlAsSuper = insert ControlRightKey SuperRightKey

  remapsMirror = Set.fromList $ swap <$> Map.toList remaps

  remapsWithKeyCodes :: m $ Map KeyName (KeyName, KeyCode)
  remapsWithKeyCodes = keyAliases >>= \aliases ->
    flip Map.traverseWithKey remaps $ \keyNameRemapFrom keyNameRemapTo ->
      let
        found   = find (view _1 .> (== keyNameRemapTo)) aliases
        failMsg = [qms| Default key code of {keyNameRemapTo} not found
                        to remap {keyNameRemapFrom} to |]
      in
        maybe (throwError failMsg) (pure . (keyNameRemapTo,) . view _3) found

  remappedKeyAliases :: m $ Set KeyAlias
  remappedKeyAliases = go' where
    go' = join $ f <$> keyAliases <*> remapsWithKeyCodes

    f aliases remaps' = resolve where
      (new, xsRemaps) = foldl reducer (Set.empty, remaps') aliases
      resolve         = Map.null xsRemaps ? pure new $ throwError failMsg
      failMsg         = [qms| Some keys which are supposed to be remapped
                              haven't found their targets: {xsRemaps} |]

    reducer (flip Set.insert -> append, xsRemaps) alias@(view _1 -> keyName) =
      case lookup keyName xsRemaps <&> view _2 <&> set _3 of
           Nothing    -> (append alias,         xsRemaps)
           Just remap -> (append $ remap alias, delete keyName xsRemaps)

  nameMap :: m $ Map KeyName KeyAlias
  nameMap = getByNameMap <$> remappedKeyAliases

  devMap :: m $ Map Word16 KeyAlias
  devMap = delete minBound . getByDevNumMap <$> remappedKeyAliases

  byNameMediaAliasesMap :: m $ Map KeyName KeyCode
  byNameMediaAliasesMap
     =  remappedKeyAliases
    <&> Set.filter (view _1 .> (`member` mediaDevNums))
    <&> Map.fromList . Set.toList . Set.map (view _1 &&& view _3)

  alternativeModeKeyCodes :: m $ Map KeyName (KeyName, KeyCode)
  alternativeModeKeyCodes = go' where
    go' = followRemaps <$> Map.traverseWithKey f alternativeModeRemaps

    followRemaps result = f' $ foldl reducer intialAcc remapsMirror where
      f' (toDelete, toAdd) = Map.withoutKeys result toDelete <> toAdd

      -- | A "Set" of keys to delete from original alternative mapping
      --   and a "Map" of new remapped keys to add to it.
      intialAcc = (Set.empty, Map.empty)

      reducer acc@(toDelete, toAdd) (toKey, fromKey)
        = maybe acc (\x -> (Set.insert toKey toDelete, insert fromKey x toAdd))
        $ lookup toKey result

    f keyNameFrom keyNameTo = x where
      x = lookup keyNameTo <$> defaultKeyCodes >>= resolve
      resolve = throwError failMsg `maybe` (pure . (keyNameTo,))
      failMsg = [qms| Default key code of {keyNameTo} not found
                      for alternative {keyNameFrom} |]


getAliasByKeyDevNum :: KeyMap -> Word16 -> Maybe KeyAlias
getAliasByKeyDevNum keyMap devNum = devNum `lookup` byDevNumMap keyMap


-- | Gets a key and returns a key it remmaped to in alternative mode
--   and remapped "KeyCode" to trigger remapped key.
getAlternativeRemapByName :: KeyMap -> KeyName -> Maybe (KeyName, KeyCode)
getAlternativeRemapByName keyMap keyName =
  keyName `lookup` byNameAlternativeMap keyMap

-- | Check if a key has remap in alternative mode.
hasAlternativeRemap :: KeyMap -> KeyName -> Bool
hasAlternativeRemap keyMap keyName =
  keyName `member` byNameAlternativeMap keyMap


-- | Get a key provided key remapped to.
getRemapByName :: KeyMap -> KeyName -> Maybe KeyName
getRemapByName keyMap keyName = keyName `lookup` byNameRemaps keyMap

-- | Returns a "Set" of keys which have been remapped to a specific key.
--
-- Mirrored getting of remapped keys. For example "MenuKey" and
-- "ControlRightKey" are both remapped to "SuperRightKey", so you call
-- "getRemappedByName" with "SuperRightKey" and you get a "Set" of
-- "ControlRightKey" and "MenuKey".
getRemappedByName :: KeyMap -> KeyName -> Set.Set KeyName
getRemappedByName keyMap keyName =
  Map.keysSet $ Map.filter (== keyName) $ byNameRemaps keyMap

-- | Gets a specific key and returns a "Set" that contains extra keys
--   aliased as this specific key (you get a "Set" of extra synonyms keys).
--
-- For example "LessKey" remapped as "ShiftLeftKey" and if you need to
-- handle in some way "ShiftLeftKey" you should also handle and treat "LessKey"
-- the same way you do it with "ShiftLeftKey", because they're same keys,
-- they're kinda synonyms.
--
-- So, you could try this:
--
-- @
-- getExtraKeys keyMap ShiftLeftKey
-- @
--
-- And then you get a "Set" of extra aliases for this key like this:
--
-- @
-- Set.fromList [LessKey]
-- @
--
-- (this "Set" may contain more than one alias).
--
-- TODO Is this duplicate of "getRemappedByName"?
--      Find out and tell the difference (or remove one and keep another).
getExtraKeys :: KeyMap -> KeyName -> Set.Set KeyName
getExtraKeys (extraByRemaps -> extra) keyName =
  extra & Set.filter (view _1 .> (== keyName)) & Set.map (view _2)


-- | Check if a key is a media key
isMediaKey :: KeyMap -> KeyName -> Bool
isMediaKey keyMap keyName = keyName `member` byNameMediaMap keyMap

getMediaKeyCode :: KeyMap -> KeyName -> Maybe KeyCode
getMediaKeyCode keyMap keyName = keyName `lookup` byNameMediaMap keyMap


-- | Get mapped "KeyCode" to "KeyName"
getKeyCodeByName :: KeyMap -> KeyName -> Maybe KeyCode
getKeyCodeByName keyMap keyName = keyName `lookup` byNameMap keyMap <&> view _3


getDefaultKeyCodeByName :: KeyMap -> KeyName -> Maybe KeyCode
getDefaultKeyCodeByName keyMap keyName =
  keyName `lookup` byNameDefaultKeyCode keyMap


makeApoClassy ''KeyMap
