{-# LANGUAGE ScopedTypeVariables #-}

module BucketSpec where

-------------------------------------------------------------------------------
import           Control.Concurrent
import           Control.Monad
import           Data.IORef
import           Test.Tasty.Hspec
import           Test.Tasty.HUnit
-------------------------------------------------------------------------------
import qualified Stoplight.Bucket   as T
-------------------------------------------------------------------------------

spec_throttle :: Spec
spec_throttle = parallel $ describe "throttle" $ do
  forM_ [1000, 10000, 100000, 250000] $ \ tick ->
    forM_ [1, 5, 20, 40, 50] $ \ regen ->
      it ("should work at " ++ show (tick,regen)) $ do
        testThrottle tick regen
  where
    testThrottle tick regen =  do
        i <- newIORef (0 :: Integer)
        T.withThrottle 0 50 tick regen $ \t -> do
          replicateM_ 20 $ forkIO $ forever $ do
            T.wait t 1
            atomicModifyIORef' i $ \ i' -> (i'+1, ())

          -- wait a multiple of tick
          let w = tick * 20

          threadDelay w
          cnt <- readIORef i

          let maxLim = (fromIntegral w / fromIntegral tick) * fromIntegral regen
              (cnt' :: Double) = fromIntegral cnt

          assertBool ("meets upperbound: " ++ show cnt') $ cnt' <= maxLim
          assertBool ("meets lowerbound: " ++ show cnt') $ cnt' >= maxLim * 0.8


spec_overflow :: Spec
spec_overflow = parallel $ describe "overflow" $ do
  forM_ [1000, 10000, 100000, 250000] $ \ tick ->
    forM_ [1, 5, 20] $ \ regen ->
      it ("should not overflow at " ++ show (tick,regen)) $ do
        testOverflow tick regen
  where
    testOverflow tick regen = do
      t <- T.new 0 5 tick regen
      threadDelay (tick * 10)
      availableSlots <- T.peekAvail t
      assertBool "has the maximum reserve after two ticks" $ availableSlots == 5
