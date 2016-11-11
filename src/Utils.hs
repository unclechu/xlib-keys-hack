-- Author: Viacheslav Lotsmanov
-- License: GPLv3 https://raw.githubusercontent.com/unclechu/xlib-keys-hack/master/LICENSE

module Utils
  ( (&), (.>) -- pipes
  , (<||>)
  , nextEvent'
  , errPutStrLn
  , makeApoLenses
  , makeApoClassy
  ) where

import Graphics.X11.Xlib (pending)
import Graphics.X11.Xlib.Types (Display)
import qualified Graphics.X11.Xlib.Event as XEvent
import Graphics.X11.Xlib.Display (connectionNumber)

import Control.Concurrent (threadWaitRead)

import System.Posix.Types (Fd(Fd))
import System.IO (hPutStrLn, stderr)

import qualified Language.Haskell.TH as TH

import Control.Lens ((.~))
import qualified Control.Lens.TH as LTH

import qualified Data.Maybe as Maybe
import Data.Char (toLower)


-- Pipe operator.
(&) :: a -> (a -> b) -> b
(&) = flip ($)
infixl 0 &

-- Pipe composition operator.
(.>) :: (a -> b) -> (b -> c) -> a -> c
(.>) = flip (.)
infixl 9 .>


(<||>) :: a -> a -> (Bool -> a)
a <||> b = \x -> if x then a else b
infixl 2 <||>


-- https://wiki.haskell.org/X_window_programming_in_Haskell
-- A version of nextEvent that does not block in foreign calls.
nextEvent' :: Display -> XEvent.XEventPtr -> IO ()
nextEvent' dpy evPtr =
  pending dpy
    >>= return . (/= 0)
    >>= XEvent.nextEvent dpy evPtr <||>
        (threadWaitRead (Fd fd) >> nextEvent' dpy evPtr)
  where fd = connectionNumber dpy


errPutStrLn = hPutStrLn stderr


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
        classNamer :: TH.Name -> Maybe.Maybe (TH.Name, TH.Name)
        classNamer name = let base = TH.nameBase name in
          Maybe.Just ( TH.mkName $ "Has" ++ base
                     , TH.mkName $ [toLower $ head base] ++ tail base ++ "'c"
                     )
