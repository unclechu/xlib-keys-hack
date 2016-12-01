-- Author: Viacheslav Lotsmanov
-- License: GPLv3 https://raw.githubusercontent.com/unclechu/xlib-keys-hack/master/LICENSE

module Keys.Map
  ( KeyName(..)
  , DevKeyNum
  , XKeyNum

  , keysMap
  , byNameMap
  , byDevNumMap
  ) where

import Data.Map.Strict (Map, fromList)


data KeyName = EscapeKey

             | F1Key  | F2Key  | F3Key  | F4Key
             | F5Key  | F6Key  | F7Key  | F8Key
             | F9Key  | F10Key | F11Key | F12Key

             | PrintScreenKey | ScrollLockKey | PauseKey

             -- apple magic keyboard
             | F13Key | F14Key | F15Key | F16Key
             | F17Key | F18Key | F19Key

             | GraveKey
             | Number1Key | Number2Key | Number3Key | Number4Key | Number5Key
             | Number6Key | Number7Key | Number8Key | Number9Key | Number0Key
             | MinusKey   | EqualKey   | BackspaceKey

             | TabKey
             | QKey | WKey | EKey | RKey | TKey
             | YKey | UKey | IKey | OKey | PKey
             | BracketLeftKey | BracketRightKey | BackslashKey

             | CapsLockKey
             | AKey | SKey | DKey | FKey | GKey
             | HKey | JKey | KKey | LKey
             | SemicolonKey | ApostropheKey | EnterKey

             | ShiftLeftKey
             | LessKey -- near left shift
             | ZKey | XKey | CKey | VKey | BKey | NKey | MKey
             | CommaKey | PeriodKey | SlashKey | ShiftRightKey

             | ControlLeftKey | SuperLeftKey | AltLeftKey
             | SpaceKey
             | AltRightKey | SuperRightKey | MenuKey | ControlRightKey

             | FNKey -- on apple magic keyboard
             | InsertKey | HomeKey | PageUpKey
             | DeleteKey | EndKey  | PageDownKey

             | ArrowLeftKey | ArrowRightKey | ArrowUpKey | ArrowDownKey

             | NumLockKey   | KPEqualKey   | KPDivideKey  | KPMultiplyKey
             | KPNumber7Key | KPNumber8Key | KPNumber9Key | KPSubtractKey
             | KPNumber4Key | KPNumber5Key | KPNumber6Key | KPAddKey
             | KPNumber1Key | KPNumber2Key | KPNumber3Key | KPEnterKey
             | KPNumber0Key | KPDecimalKey

             -- media keys

             | MCalculatorKey
             | MEjectKey
             | MAudioMuteKey | MAudioLowerVolumeKey | MAudioRaiseVolumeKey
             | MAudioPlayKey | MAudioStopKey | MAudioPrevKey | MAudioNextKey
             | MMonBrightnessDownKey | MMonBrightnessUpKey

               deriving (Eq, Show, Ord)


type DevKeyNum = Int
type XKeyNum = Int

keysMap :: [(KeyName, DevKeyNum, XKeyNum)]
keysMap =
  [ (EscapeKey, 0, 9) -- escape
  ]

byNameMap :: Map KeyName (DevKeyNum, XKeyNum)
byNameMap = fromList $ map f keysMap
  where f (name, devNum, xNum) = (name, (devNum, xNum))

byDevNumMap :: Map DevKeyNum (KeyName, XKeyNum)
byDevNumMap = fromList $ map f keysMap
  where f (name, devNum, xNum) = (devNum, (name, xNum))
