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
    , close
    , withThrottle
    , wait
    , peekAvail
    ) where

-------------------------------------------------------------------------------
import           Control.Concurrent
import qualified Control.Concurrent.MSemN as Sem
import qualified Control.Immortal         as Im
import           Control.Monad
import           Control.Monad.Catch
import           Control.Monad.IO.Class
import           Data.Typeable
-------------------------------------------------------------------------------


data Throttle = Throttle {
      sem    :: Sem.MSemN Int
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
--
-- Every 'new' call must be paired with a call to 'close' in order to
-- stop the regenerating thread.
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
    s <- Sem.new start
    t <- Im.create $ const $ forever $ do
      threadDelay tick
      void $ Sem.signalF s $ \ i -> (min (buffer - i) recovery, ())

    return $ Throttle {sem = s, feeder = t}


-------------------------------------------------------------------------------
-- | Clean up resources associated with a throttle. Use with 'bracket' or with
-- 'allocate' from the resourcet package.
close :: Throttle -> IO ()
close (Throttle _ t) = Im.stop t


-------------------------------------------------------------------------------
-- | Create a throttle and run an action with it, automatically closing
-- the throttle on completion.
withThrottle :: (MonadIO m, MonadMask m)
             => Int
             -- ^ Initial reserve
             -> Int
             -- ^ Maximum reserve
             -> Int
             -- ^ Regeneration tick length in microseconds
             -> Int
             -- ^ Regeneration amount
             -> (Throttle -> m b)
             -- ^ Action to perform with the throttle
             -> m b
withThrottle start buffer tick recovery m =
    bracket (liftIO $ new start buffer tick recovery) (liftIO . close) m


-------------------------------------------------------------------------------
-- | Acquire n capacity units from throttle or block until it's
-- available. Smaller waiters may block in FIFO fashion due to larger
-- waiters earlier in the queue.
wait :: Throttle -> Int -> IO ()
wait (Throttle s _) = Sem.wait s


-------------------------------------------------------------------------------
-- | Peek currently available slot count
peekAvail :: Throttle -> IO Int
peekAvail = Sem.peekAvail . sem
