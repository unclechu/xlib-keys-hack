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
import Control.Monad (when, unless, filterM)
import Control.Lens ( (.~), (%~), (^.), (.=), (&~)
                    , set, over, view
                    )

import Data.Maybe (Maybe(Just, Nothing))
import Data.Set (toList, fromList, insert)

import qualified Options as O
import Utils (errPutStrLn, errPutStr, dieWith, (&), (.>), updateState')


-- Gets only available ids of 'xinput' devices
-- and store it to 'availableXInputDevices' option.
getAvailable :: O.Options -> IO O.Options
getAvailable opts = flip St.execStateT opts $
  -- Deal with bare ids first.
  St.lift (fromProc "xinput" ["list", "--id-only"])
    >>= mapM (\x -> return (read x :: Int))
    >>= filterAvailableDeviceId
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

        -- Run child process and extract its output.
        fromProc :: String -> [String] -> IO [String]
        fromProc proc args = P.readProcessWithExitCode proc args ""
                         >>= checkExitCode (proc:args)
                         >>= return . map (\x -> if x == '\t' then ' ' else x)
                         >>= return . lines

        -- Filters only available devices ids.
        filterAvailableDeviceId :: (MonadState s m, O.HasOptions s, Functor m)
                                => [Int] -> m [Int]
        filterAvailableDeviceId all = fmap (^. O.disableXInputDeviceId') St.get
                                  >>= filterM (return . (`elem` all))

        -- Get pair with (id, name) from single line of 'xinput' output.
        -- FIXME it looks awful, refactor it!
        extractIdNamePair :: String -> (Int, String)
        extractIdNamePair = words .> reducer (0, "")
          where reducer :: (Int, String) -> [String] -> (Int, String)
                reducer (id, "") ((hasNameSymbols -> False):xs) =
                  reducer (id, "") xs
                reducer (_, "")   [] = (0, "")
                reducer (_, "")   ((getId -> Just id):_) = (0, "")
                reducer (_, name) ((getId -> Just id):_) = (id, name)
                reducer (_, "")   (x:xs) = reducer (0, x) xs
                reducer (_, name) (x:xs) = reducer (0, name ++ " " ++ x) xs

                getId :: String -> Maybe Int
                getId ('i':'d':'=':(read -> id :: Int)) = Just id
                getId _ = Nothing

                hasNameSymbols :: String -> Bool
                hasNameSymbols "" = False
                hasNameSymbols ((flip elem nameSymbols -> True):_) = True
                hasNameSymbols (x:xs) = hasNameSymbols xs
                nameSymbols = ['a'..'z'] ++ ['A'..'Z'] ++ ['0'..'9'] ++ "_-"

        -- Gets ids from 'disableXInputDeviceName' but only available.
        -- This typing means we can touch only state but never IO.
        getAvailableIdsFromNames :: (MonadState s m, O.HasOptions s, Functor m)
                                 => [(Int, String)] -> m [Int]
        getAvailableIdsFromNames available = do
          (names :: [String]) <-
            -- `unwords.words` is removing double spaces
            -- (becaused they are removed in `available`).
            let f = view O.disableXInputDeviceName' .> map (unwords . words)
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
