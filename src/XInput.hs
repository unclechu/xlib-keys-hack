-- Author: Viacheslav Lotsmanov
-- License: GPLv3 https://raw.githubusercontent.com/unclechu/xlib-keys-hack/master/LICENSE

{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ViewPatterns #-}

module XInput
  ( getAvailable
  ) where

import System.Exit (ExitCode(ExitSuccess, ExitFailure))
import qualified System.Process as P

import qualified Control.Monad.State as St
import Control.Monad.State.Class (MonadState)
import Control.Monad (when, unless)
import Control.Lens ( (.~), (%~), (^.), (.=), (&~)
                    , set, over, view
                    )

import Data.Maybe (Maybe(Just, Nothing))
import Data.Set (toList, fromList, insert, difference)

import qualified Options as O
import Utils (errPutStrLn, errPutStr, dieWith, (&), (.>), updateState')


-- Gets only available ids of 'xinput' devices
-- and store it to 'availableXInputDevices' option.
getAvailable :: O.Options -> IO O.Options
getAvailable opts = flip St.execStateT opts $
  -- Deal with bare ids first.
  St.lift (fromProc "xinput" ["list", "--id-only"])
    >>= return . map (\x -> read x :: Int)
    >>= filterAvailableDeviceId
    >>= checkForExplicitAvailable
    >>= updateState' (flip $ set O.availableXInputDevices')

    -- Extract ids from names.
    >> St.lift (fromProc "xinput" ["list", "--short"])
    >>= return . filter (\(id, name) -> id /= 0 && name /= "")
               . map extractIdNamePair
    >>= getAvailableIdsFromNames

    -- Merge and store this both results.
    >>= mergeAvailableIdsWithPrevious
    >>= updateState' (flip $ set O.availableXInputDevices')

  where -- Check if exit code is okay and then return stdout,
        -- or fail the application and print to stderr error message.
        checkExitCode :: [String] -> (ExitCode, String, String) -> IO String
        checkExitCode  _  (ExitSuccess,   out,  _ ) = return out
        checkExitCode cmd (ExitFailure n,  _ , err) = do
          unless (null cmd) $ errPutStrLn $ "'xinput' command: " ++ unwords cmd
          errPutStr $ "'xinput' error: " ++ err
          dieWith $ "'xinput' failed with exit status: " ++ show n

        rmTabs '\t' = ' '
        rmTabs   x  =  x

        -- Run child process and extract its output.
        fromProc :: String -> [String] -> IO [String]
        fromProc proc args = P.readProcessWithExitCode proc args ""
                         >>= checkExitCode (proc:args)
                         >>= return . lines . map rmTabs

        -- Filters only available devices ids.
        filterAvailableDeviceId :: (MonadState s m, O.HasOptions s, Functor m)
                                => [Int] -> m [Int]
        filterAvailableDeviceId all = fmap f St.get
          where f = view O.disableXInputDeviceId' .> filter (`elem` all)

        -- All explicit devices ids from arguments
        -- must be available! Check if it's true.
        checkForExplicitAvailable :: [Int] -> St.StateT O.Options IO [Int]
        checkForExplicitAvailable filteredAvailable =
          fmap f St.get
            >>= St.lift . check
            >> return filteredAvailable
          where f = (fromList . view O.disableXInputDeviceId')
                 .> (\x -> (x, fromList filteredAvailable))
                check (a, b) = when (a /= b) $ do
                  errPutStrLn  $  "'xinput' error: these ids is unavailable: "
                              ++  show diff
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
                reducer (_, name) ((getId -> Just id):_)
                  | name == "" = (0, "") -- no name, invalid line
                  | otherwise  = (id, name) -- got id and name, done
                -- Extracting name.
                reducer (_, name) (x:xs)
                  | name == "" = reducer (0, x) xs -- first word of name
                  | otherwise  = reducer (0, name ++ " " ++ x) xs -- another word

                getId :: String -> Maybe Int
                getId ('i':'d':'=':(read -> id :: Int)) = Just id
                getId _ = Nothing

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
            let f = view O.disableXInputDeviceName'
                 .> map (map rmTabs)
                 .> map (unwords . words)
            in fmap f St.get

          return [ avId
                 | name <- names
                 , (avId, avName) <- available
                 , name == avName
                 ]

        -- Merge it with previous option value (removes duplicates).
        mergeAvailableIdsWithPrevious ::
          (MonadState s m, O.HasOptions s, Functor m) => [Int] -> m [Int]
        mergeAvailableIdsWithPrevious new = fmap f St.get
          where f = view O.availableXInputDevices'
                 .> (++ new) .> fromList .> toList
