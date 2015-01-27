{-# LANGUAGE ScopedTypeVariables #-}

module BucketSpec where

-------------------------------------------------------------------------------
import           Control.Concurrent
import           Control.Monad
import           Data.IORef
import           Test.Hspec
import           Test.HUnit
-------------------------------------------------------------------------------
import qualified Stoplight.Bucket   as T
-------------------------------------------------------------------------------

spec :: Spec
spec = parallel $ describe "throttle" $ do

  forM_ [1000, 10000, 100000, 250000] $ \ tick ->
    forM_ [1, 5, 20, 40, 50] $ \ regen ->
      it ("should work at " ++ show (tick,regen)) $ do
        testThrottle tick regen



testThrottle tick regen =  do
    i <- newIORef 0
    t <- T.new 0 50 tick regen

    replicateM 20 $ forkIO $ forever $ do
      T.wait t 1
      atomicModifyIORef' i $ \ i -> (i+1, ())

    -- wait a multiple of tick
    let w = tick * 20

    threadDelay w
    cnt <- readIORef i

    let maxLim = (fromIntegral w / fromIntegral tick) * fromIntegral regen
        cnt' = fromIntegral cnt

    assertBool ("meets upperbound: " ++ show cnt') $ cnt' <= maxLim
    assertBool ("meets lowerbound: " ++ show cnt') $ cnt' >= maxLim * 0.8





