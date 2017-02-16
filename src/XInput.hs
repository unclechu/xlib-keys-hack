-- Author: Viacheslav Lotsmanov
-- License: GPLv3 https://raw.githubusercontent.com/unclechu/xlib-keys-hack/master/LICENSE

{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE PackageImports #-}
{-# LANGUAGE QuasiQuotes #-}

module XInput
  ( getAvailable
  , disable
  , enable
  ) where

import "base" System.Exit (ExitCode(ExitSuccess, ExitFailure))
import "process" System.Process (readProcessWithExitCode)

import "base" Control.Monad (when, unless, forM_)
import "mtl" Control.Monad.State (execStateT, StateT)
import qualified "mtl" Control.Monad.State as St (get)
import "mtl" Control.Monad.State.Class (MonadState)
import "transformers" Control.Monad.Trans.Class (lift)
import "lens" Control.Lens (set, view)

import "containers" Data.Set (toList, fromList, difference)
import "base" Data.List (elemIndex, find)
import "base" Data.Maybe (isJust)

-- local imports

import qualified Options as O
import Utils (errPutStrLn, errPutStr, dieWith)
import Utils.StateMonad (updateState')
import Utils.Sugar ((.>), (<&>))
import Utils.String (qm)


type Options = O.Options


-- Gets only available ids of 'xinput' devices
-- and store it to 'availableXInputDevices' option.
getAvailable :: Options -> IO Options
getAvailable = execStateT $
  -- Deal with bare ids first.
  lift (fromProc "xinput" ["list", "--id-only"])
    >>= return . map (\x -> read x :: Int)
    >>= filterAvailableDeviceId
    >>= checkForExplicitAvailable
    >>= updateState' (flip $ set O.availableXInputDevices')

    -- Extract ids from names.
    >>  lift (fromProc "xinput" ["list", "--short"])
    >>= return . filter (\(_id, name) -> _id /= 0 && name /= "")
               . map extractIdNamePair
    >>= getAvailableIdsFromNames

    -- Merge and store this both results.
    >>= mergeAvailableIdsWithPrevious
    >>= updateState' (flip $ set O.availableXInputDevices')

  where -- Filters only available devices ids.
        filterAvailableDeviceId :: (MonadState s m, O.HasOptions s, Functor m)
                                => [Int] -> m [Int]
        filterAvailableDeviceId allDevs = fmap f St.get
          where f = view O.disableXInputDeviceId' .> filter (`elem` allDevs)

        -- All explicit devices ids from arguments
        -- must be available! Check if it's true.
        checkForExplicitAvailable :: [Int] -> StateT Options IO [Int]
        checkForExplicitAvailable filteredAvailable =
          filteredAvailable <$ (fmap f St.get >>= lift . check)
          where f = (fromList . view O.disableXInputDeviceId')
                 .> (\x -> (x, fromList filteredAvailable))
                check (a, b) = when (a /= b) $ do
                  errPutStrLn [qm| 'xinput' error:
                                 \ these ids is unavailable: {diff} |]
                  dieWith "'xinput': all explicit ids of devices\
                          \ must be available"
                  where diff = toList $ difference a b

        -- Get pair with (id, name) from single line of 'xinput' output.
        extractIdNamePair :: String -> (Int, String)
        extractIdNamePair = words .> reducer (0, "")
          where -- Parse next word from line.
                reducer :: (Int, String) -> [String] -> (Int, String)
                -- If we have no more symbols for extracting name
                -- and it's end of the line, it means this line
                -- doesn't contain name or id of device.
                reducer (_, "") [] = (0, "")
                -- If we don't have any extracted name symbols yet
                -- and we don't have any valid name symbol in current word
                -- then it's decorative symbols, skip it.
                reducer (_, "")
                        ((hasNameSymbols -> False):xs) = reducer (0, "") xs
                -- Got id, end of recursion.
                reducer (_, name) ((getId -> Just _id):xs)
                  -- invalid line (no name) or it's not a keyboard (pointer)
                  | name == "" || not (isKeyboard xs) = (0, "")
                  -- got id and name, done
                  | otherwise = (_id, name)
                -- Extracting name.
                reducer (_, name) (x:xs)
                  | name == "" = reducer (0, x) xs -- first word of name
                  | otherwise  = reducer (0, name ++ " " ++ x) xs -- another word
                reducer _ _ = error "unexpected behavior"

                getId :: String -> Maybe Int
                getId ('i':'d':'=':(read -> _id :: Int)) = Just _id
                getId _ = Nothing

                isKeyboard :: [String] -> Bool
                isKeyboard (unwords -> x) = isJust $
                  elemIndex '[' x <&> (+1) <&> flip drop x
                    >>= \y -> elemIndex ']' y <&> flip take y <&> words
                    >>= find (== "keyboard")

                hasNameSymbols :: String -> Bool
                hasNameSymbols "" = False
                hasNameSymbols ((flip elem nameSymbols -> True):_) = True
                hasNameSymbols (_:xs) = hasNameSymbols xs
                nameSymbols = ['a'..'z'] ++ ['A'..'Z'] ++ ['0'..'9']

        -- Gets ids from 'disableXInputDeviceName' but only available.
        -- This typing means we can touch only state but never IO.
        getAvailableIdsFromNames :: (MonadState s m, O.HasOptions s, Functor m)
                                 => [(Int, String)] -> m [Int]
        getAvailableIdsFromNames available = do
          (names :: [String]) <-
            -- `unwords.words` is removing double spaces
            -- (becaused they are removed in `available`).
            St.get <&> view O.disableXInputDeviceName'
                        .> map (map rmTabs)
                        .> map (unwords . words)

          return [ avId | name           <- names
                        , (avId, avName) <- available
                        , name == avName
                        ]

        -- Merge it with previous option value (removes duplicates)
        mergeAvailableIdsWithPrevious ::
          (MonadState s m, O.HasOptions s, Functor m) => [Int] -> m [Int]
        mergeAvailableIdsWithPrevious new =
          St.get <&> view O.availableXInputDevices'
                      .> (++ new) .> fromList .> toList


disable :: Options -> IO ()
disable opts = forM_ (O.availableXInputDevices opts) off
  where off _id = fromProc "xinput" ["disable", show _id]

enable :: Options -> IO ()
enable opts = forM_ (O.availableXInputDevices opts) on
  where on _id = fromProc "xinput" ["enable", show _id]


rmTabs :: Char -> Char
rmTabs '\t' = ' '
rmTabs   x  =  x

-- Run child process and extract its output.
fromProc :: String -> [String] -> IO [String]
fromProc proc args = readProcessWithExitCode proc args ""
                       >>= checkExitCode (proc:args)
                       >>= return . lines . map rmTabs

-- Check if exit code is okay and then return stdout,
-- or fail the application and print to stderr error message.
checkExitCode :: [String] -> (ExitCode, String, String) -> IO String
checkExitCode  _  (ExitSuccess,   out,  _ ) = return out
checkExitCode cmd (ExitFailure n,  _ , err) = do
  unless (null cmd) $ errPutStrLn [qm|'xinput' command: {unwords cmd}|]
  errPutStr [qm|'xinput' error: {err}|]
  dieWith [qm|'xinput' failed with exit status: {n}|]
