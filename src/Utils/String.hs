-- Fork of: https://github.com/audreyt/interpolatedstring-perl6/blob/63d91a83eb5e48740c87570a8c7fd4668afe6832/src/Text/InterpolatedString/Perl6.hs
-- Author of the 'interpolatedstring-perl6' package: Audrey Tang
--
-- Author of this fork: Viacheslav Lotsmanov
-- License: GPLv3 https://raw.githubusercontent.com/unclechu/xlib-keys-hack/master/LICENSE

{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE IncoherentInstances #-}

{-# LANGUAGE PackageImports #-}
{-# LANGUAGE ViewPatterns #-}

module Utils.String (qm, ShowQ(..)) where

import qualified "template-haskell" Language.Haskell.TH as TH
import "template-haskell" Language.Haskell.TH.Quote
import "haskell-src-meta" Language.Haskell.Meta.Parse
import "base" GHC.Exts (IsString(..))
import "base" Data.Monoid (Monoid(..))
import "bytestring" Data.ByteString.Char8 as Strict (ByteString, unpack)
import "bytestring" Data.ByteString.Lazy.Char8 as Lazy (ByteString, unpack)
import "text" Data.Text as T (Text, unpack)
import "text" Data.Text.Lazy as LazyT(Text, unpack)
import "base" Data.Char (isAlpha, isAlphaNum)

-- local imports

import Utils (ifMaybe)


class ShowQ a where
  showQ :: a -> String

instance ShowQ Char where
  showQ = (:[])

instance ShowQ String where
  showQ = id

instance ShowQ Strict.ByteString where
  showQ = Strict.unpack

instance ShowQ Lazy.ByteString where
  showQ = Lazy.unpack

instance ShowQ T.Text where
  showQ = T.unpack

instance ShowQ LazyT.Text where
  showQ = LazyT.unpack

instance Show a => ShowQ a where
  showQ = show

-- todo: this should really be rewritten into RULES pragmas, but so far
-- I can't convince GHC to let the rules fire.
class QQ a string where
  toQQ :: a -> string

instance IsString s => QQ s s where
  toQQ = id

instance (ShowQ a, IsString s) => QQ a s where
  toQQ = fromString . showQ

data StringPart = Literal String | AntiQuote String deriving Show


unQM a []          = [Literal (reverse a)]
unQM a ('\\':x:xs) = unQM (x:a) xs
unQM a ("\\")      = unQM ('\\':a) []
unQM a ('}':xs)    = AntiQuote (reverse a) : parseQM [] xs
unQM a (x:xs)      = unQM (x:a) xs

parseQM a []             = [Literal (reverse a)]
parseQM a ('\\':'\\':xs) = parseQM ('\\':a) xs
parseQM a ('\\':'{':xs)  = parseQM ('{':a) xs
parseQM a ('\\':' ':xs)  = parseQM (' ':a) xs
parseQM a ('\\':'\n':xs) = parseQM a ('\n':xs)
parseQM a ('\\':'n':xs)  = parseQM ('\n':a) xs
parseQM a ("\\")         = parseQM ('\\':a) []
parseQM a ('{':xs)       = Literal (reverse a) : unQM [] xs
parseQM a (clearIndentAtSOF   -> Just clean) = parseQM a clean
parseQM a (clearIndentTillEOF -> Just clean) = parseQM a clean
parseQM a ('\n':xs)      = parseQM a xs -- cut off line breaks
parseQM a (x:xs)         = parseQM (x:a) xs

clearIndentTillEOF :: String -> Maybe String
clearIndentTillEOF str@((ifMaybe (`elem` "\t ") -> Just _) : _) = cutOff str
  where cutOff :: String -> Maybe String
        cutOff ""            = Just ""
        cutOff eof@('\n':xs) = Just eof
        cutOff ((ifMaybe (`elem` "\t ") -> Just _) : xs) = cutOff xs
        cutOff _ = Nothing
clearIndentTillEOF _ = Nothing

clearIndentAtSOF :: String -> Maybe String
clearIndentAtSOF str@('\n' : xs) = if result /= xs
                                      then Just $ '\n' : cutOff xs
                                      else Nothing
  where cutOff :: String -> String
        cutOff ((ifMaybe (`elem` "\t ") -> Just _) : xs) = cutOff xs
        cutOff s = s
        result = cutOff xs
clearIndentAtSOF _ = Nothing

clearIndentAtStart :: String -> String
clearIndentAtStart ((ifMaybe (`elem` "\t ") -> Just _) : xs) =
  clearIndentAtStart xs
clearIndentAtStart s = s


makeExpr [] = [| mempty |]
makeExpr (Literal a : xs) =
  TH.appE [| mappend (fromString a) |]    $ makeExpr xs
makeExpr (AntiQuote a : xs) =
  TH.appE [| mappend (toQQ $(reify a)) |] $ makeExpr xs

reify s =
  case parseExp s of
       Left s  -> TH.reportError s >> [| mempty |]
       Right e -> return e


-- Just like `qc` from 'interpolatedstring-perl6' package
-- but ignores any indentation and line-breaks (except explicitly written).
--
-- 'm' means 'multiline'.
--
-- You could write decoratively formatted string and your
-- decorative indentation and line-breaks wont go to string,
-- but when you need it, you could just escape it using backslash,
-- see for some examples below.
--
-- Examples:
--
--   [qm|   hello world,
--        \ what's going on?  |]
--   -- Result: "hello world, what's going on here?"
--
--   [qm|
--         it's actual
--         ly ignored
--      |]
--   -- Result: "it's actually ignored"
--
--   [qm|  \  You could explicitly escape indentation or\n
--            line-breaks when you really need it!  \
--      |]
--   -- Result: "  You could explicitly escape indentation or\nline-breaks when you really need it!  "
--
--   [qm| {1+2} \{3+4} |]
--   -- Result: "3 {3+4}"
qm :: QuasiQuoter
qm = QuasiQuoter f
  (error "Cannot use qm as a pattern")
  (error "Cannot use qm as a type")
  (error "Cannot use qm as a dec")
  where f = makeExpr . parseQM [] . clearIndentAtStart . filter (/= '\r')
