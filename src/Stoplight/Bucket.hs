{-# LANGUAGE DeriveDataTypeable #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Stoplight.Bucket
-- Copyright   :  Soostone Inc, 2014
-- License     :  All Rights Reserved
--
-- Maintainer  :  Ozgun Ataman
-- Stability   :  experimental
--
-- A simple in-memory rate limiter based on the bucket model. You can
-- spend any capacity available in your bucket of water. When it's
-- full, you can burst it down. When it's empty, you need to wait for
-- it to sufficiently fill up to continue.
----------------------------------------------------------------------------

module Stoplight.Bucket
    ( Throttle
    , new
    , wait
    , peekAvail
    ) where

-------------------------------------------------------------------------------
import           Control.Concurrent
import qualified Control.Concurrent.MSemN as Sem
import qualified Control.Immortal         as Im
import           Control.Monad
import           Control.Monad.Fix
import           Data.IORef
import           Data.Typeable
import           System.Mem.Weak
-------------------------------------------------------------------------------


data Throttle = Throttle {
      semRef :: IORef (Sem.MSemN Int)
    , feeder :: Im.Thread
    } deriving (Typeable)


-------------------------------------------------------------------------------
-- | New rate limiter with a surplus buffer and capacity recovery with
-- custom ticks.
--
-- How it works:
--
-- >>> new initial reserve tick regen
--
-- Throttle will start with @initial@ capacity units and burst it down
-- to zero without any limiting.
--
-- Right from the start, it will regenerate by @regen@ every @tick@
-- microseconds, up to a maximum of @reserve@.
--
-- Minimum tick size of 1000 is recommended, as underlying MVar delays
-- disrupt expected results in lower settings.
new :: Int
    -- ^ Initial reserve
    -> Int
    -- ^ Maximum reserve
    -> Int
    -- ^ Regeneration tick length in microseconds
    -> Int
    -- ^ Regeneration amount
    -> IO Throttle
new start buffer tick recovery = do
    sref <- newIORef =<< Sem.new start
    t <- mfix (\ t -> do
      s <- mkWeakIORef sref (Im.stop t)
      Im.create $ const $ forever $ do
        threadDelay tick
        s' <- deRefWeak s
        case s' of
          Nothing -> return ()
          Just s'' -> do
            sref' <- readIORef s''
            void $ Sem.signalF sref' $ \ i -> (min (buffer - i) recovery, ()))

    return $ Throttle {semRef = sref, feeder = t}


-------------------------------------------------------------------------------
-- | Acquire n capacity units from throttle or block until it's
-- available. Smaller waiters may block in FIFO fashion due to larger
-- waiters earlier in the queue.
wait :: Throttle -> Int -> IO ()
wait (Throttle sem _) w = do
  s <- readIORef sem
  Sem.wait s w

-------------------------------------------------------------------------------
-- | Peek currently available slot count
peekAvail :: Throttle -> IO Int
peekAvail (Throttle sem _) = do
  s <- readIORef sem
  Sem.peekAvail s
