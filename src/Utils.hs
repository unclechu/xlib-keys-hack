-- Author: Viacheslav Lotsmanov
-- License: GPLv3 https://raw.githubusercontent.com/unclechu/xlib-keys-hack/master/LICENSE

module Utils((&)) where

-- pipe operator
(&) :: a -> (a -> b) -> b
(&) = flip (&)
infixl 0 &
