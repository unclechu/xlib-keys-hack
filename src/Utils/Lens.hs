-- Author: Viacheslav Lotsmanov
-- License: GPLv3 https://raw.githubusercontent.com/unclechu/xlib-keys-hack/master/LICENSE

{-# LANGUAGE PackageImports #-}

module Utils.Lens
  ( makeApoLenses
  , makeApoClassy
  ) where

import qualified "template-haskell" Language.Haskell.TH as TH

import "lens" Control.Lens ((.~))
import qualified "lens" Control.Lens.TH as LTH

import "base" Data.Char (toLower)

-- local imports

import Utils.Sugar ((&), (.>))


lensNamer :: LTH.FieldNamer
lensNamer = LTH.mappingNamer $ (++ "'") .> (:[])


-- Customization of 'makeLenses'.
-- Original "foo" transforms to "foo'"
-- instead of "_foo" transforms "foo".
makeApoLenses :: TH.Name -> TH.DecsQ
makeApoLenses = LTH.makeLensesWith rules
  where rules :: LTH.LensRules
        rules = LTH.lensRules & LTH.lensField .~ lensNamer


-- Same way as 'makeApoLenses'
-- customization of 'makeClassy'.
makeApoClassy :: TH.Name -> TH.DecsQ
makeApoClassy = LTH.makeLensesWith rules
  where rules :: LTH.LensRules
        rules = LTH.classyRules
              & LTH.lensField .~ lensNamer
              & LTH.lensClass .~ classNamer
        classNamer :: TH.Name -> Maybe (TH.Name, TH.Name)
        classNamer name = let base = TH.nameBase name in
          Just ( TH.mkName $ "Has" ++ base
               , TH.mkName $ [toLower $ head base] ++ tail base ++ "'c"
               )
