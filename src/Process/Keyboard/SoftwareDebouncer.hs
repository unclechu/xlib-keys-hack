-- Author: Viacheslav Lotsmanov
-- License: GPLv3 https://raw.githubusercontent.com/unclechu/xlib-keys-hack/master/LICENSE

{-# LANGUAGE RecordWildCards #-}

module Process.Keyboard.SoftwareDebouncer
     ( SoftwareDebouncer
     , getSoftwareDebouncerTiming
     , getSoftwareDebouncer
     , moveKeyThroughSoftwareDebouncer
     , handleNextSoftwareDebouncerEvent
     ) where

import "base" Data.Word (Word8)
import "containers" Data.Set (type Set)
import qualified "containers" Data.Set as Set
import "time" Data.Time.Clock.POSIX (POSIXTime, getPOSIXTime)

import "type-operators" Control.Type.Operator (type ($))
import "base" Control.Monad (when)
import "base" Control.Concurrent (threadDelay)
import qualified "stm" Control.Monad.STM as STM
import qualified "stm" Control.Concurrent.STM.TVar as STM
import qualified "stm" Control.Concurrent.STM.TChan as STM

-- local imports

import           Utils.Sugar ((?), (<&>), (<$.), (.>), preserve')
import           Options (Options)
import qualified Options as O
import           Process.Keyboard.Types (OrderedKey, HandledKey (HandledKey))


data SoftwareDebouncer
   = SoftwareDebouncer
   { hwPressedKeysVar :: STM.TVar $ Set OrderedKey
       -- ^ Hardware actual pressed keys set (pressed right now)

   , debouncingKeysVar :: STM.TVar $ Set OrderedKey
       -- ^ Set of keys which events temporarily ignored (debounced)

   , debouncedChan :: STM.TChan (HandledKey, Int)
       -- ^ FIFO to handle event of a key again after some amount of time

   , debouncerTiming :: POSIXTime
       -- ^ Gap between first and next events handling.
       --   See "Options" module for details.
   }


-- | Only a getter (for exporting from module).
getSoftwareDebouncerTiming :: SoftwareDebouncer -> POSIXTime
getSoftwareDebouncerTiming = debouncerTiming


-- | Returns "SoftwareDebouncer" and event handler
--   if debouncer option is set.
--
-- You supposed to start software debouncer event handler in own thread,
-- see "handleNextSoftwareDebouncerEvent" for details.
getSoftwareDebouncer :: Options -> IO $ Maybe SoftwareDebouncer
getSoftwareDebouncer = O.debouncerTiming .> go where
  go = maybe (pure Nothing) $ Just <$. getDebouncer

  getDebouncer debouncerTiming'
    = STM.atomically
    $ SoftwareDebouncer <$> STM.newTVar mempty
                        <*> STM.newTVar mempty
                        <*> STM.newTChan
                        <*> pure debouncerTiming'


-- | Move @HandledKey@ through software debouncer logic.
--
-- It checks whether a key is currently debounced so an event is supposed to be
-- ignored. Otherwise it supposed to be added to debounced keys list.
--
-- If software debouncer feature is turned on you supposed to pass all your
-- @HandledKey@ through this function, like this:
--
-- @
-- let keyEventHandler = handleKeyEvent ctVars opts keyMap
--
-- forkIO $ forever $
--   getNextKeyboardDeviceKeyEvent keyMap fd
--     >>= moveKeyThroughSoftwareDebouncer softwareDebouncer
--     >>= pure () `maybe` keyEventHandler
-- @
--
-- TODO Add logging of events handling.
moveKeyThroughSoftwareDebouncer
  :: SoftwareDebouncer
  -> HandledKey
  -> IO $ Maybe HandledKey
  -- ^ @Maybe@ Indicates whether event should be triggered (or it's ignored)

moveKeyThroughSoftwareDebouncer
  SoftwareDebouncer {..}
  ev@(HandledKey key name code isPressed) = go where

  go = getPOSIXTime
    <&> (+ debouncerTiming) .> posixTimeToMicroseconds
    >>= STM.atomically . stmTransaction

  stmTransaction debouncedTime = do
    -- Always storing last received state of a key (pressed or released)
    STM.modifyTVar' hwPressedKeysVar $ (isPressed ? Set.insert $ Set.delete) key

    -- Indicates that current key isn't debounced right now
    -- so we have to trigger first event immidiately
    -- and mark that key as debounced.
    isKeyEventPassed <- Set.notMember key <$> STM.readTVar debouncingKeysVar

    -- Debounce a key if it wasn't debounced
    when isKeyEventPassed $ do
      -- Temporarily ignoring that key from now
      STM.modifyTVar' debouncingKeysVar $ Set.insert key

      -- Debounce/delay next handle
      STM.writeTChan debouncedChan
        (HandledKey key name code isPressed, debouncedTime)

    pure $ preserve' isKeyEventPassed ev


-- | A handler of a debounced event from queue.
--
-- You're supposed to run this forever again and again in own thread, like that:
--
-- @
-- let keyEventHandler = handleKeyEvent ctVars opts keyMap
--
-- forkIO $ forever $
--   handleNextSoftwareDebouncerEvent softwareDebouncer >>=
--     pure () `maybe` keyEventHandler
-- @
--
-- TODO Add logging of events handling.
handleNextSoftwareDebouncerEvent :: SoftwareDebouncer -> IO $ Maybe HandledKey
handleNextSoftwareDebouncerEvent SoftwareDebouncer {..} = do
  -- Getting next event
  (HandledKey key name code isPressed, debouncedTime) <-
    STM.atomically $ STM.readTChan debouncedChan

  currentTime <- getPOSIXTime

  -- Delayed time threshold - current time
  let diffTime = debouncedTime - posixTimeToMicroseconds currentTime

  -- Waiting for a gap before checking next state of a key (debouncing).
  --
  -- In case there was a lag or next event was very close to previous one
  -- @diffTime@ may have negative value so we don't have to wait at all and act
  -- immidiately, that's why we're checking it is greater than zero).
  --
  -- It's okay to use single waiter for many keys here, it won't block anything
  -- since it waits only for @diffTime@ (not for some fixed delay value).
  -- Next key in order is always supposed to be handled later than current one,
  -- so after current one is handled it will immidiately try to handle next one
  -- and if a gap is shorter it will just wait less amount of time.
  when (diffTime > 0) $ threadDelay diffTime

  -- Checking if after a gap state of a key is changed
  -- (e.g. key was pressed but now, after a gap, it's released or vise versa).
  isItInverted <- STM.atomically $ do
    -- Like right now.
    isPhysicallyPressed <- Set.member key <$> STM.readTVar hwPressedKeysVar

    -- Comparing last known state of a key, before a gap,
    -- with current state, after a gap
    -- (any state changes in between were ignored).
    if isPressed == isPhysicallyPressed

       then -- State of a key isn't changed after a gap,
            -- so just forgetting a key ever have been debounced,
            -- so it could be debounced again now.
            False <$ STM.modifyTVar' debouncingKeysVar (Set.delete key)

       else -- A key was pressed but now, after a gap, it's released,
            -- or vise versa, a key was released but now it's pressed.
            -- Leaving it marked as debounced (see @debouncingKeysVar@),
            -- as it was, and adding new delayed/debounced event for new
            -- inverted state of a key.
            let silenceGap =
                  posixTimeToMicroseconds $ currentTime + debouncerTiming

                debouncedEvent =
                  (HandledKey key name code isPhysicallyPressed, silenceGap)

             in True <$ STM.writeTChan debouncedChan debouncedEvent

  -- So since after a gap state of a key is inverted we have to trigger
  -- new key state right now, to trigger fake key event.
  pure $ preserve' isItInverted $ HandledKey key name code $ not isPressed


posixTimeToMicroseconds :: Integral a => POSIXTime -> a
posixTimeToMicroseconds = round . (* 10 ^ (6 :: Word8))
