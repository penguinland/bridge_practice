--{-# LANGUAGE BangPatterns #-}
module ThreadPool(ThreadPool, newThreadPool, enqueue, StIO) where

import Control.Concurrent(forkIO)
import Control.Concurrent.Classy.BoundedChan(
    BoundedChan, newBoundedChan, writeBoundedChan, readBoundedChan)
import Control.Monad.Trans.State.Strict(StateT, runStateT)
import System.Random(StdGen, split)


type StIO = StateT StdGen IO
type ThreadPool = BoundedChan IO (StIO ())


splitRNG :: StdGen -> Int -> [StdGen]
splitRNG _ 0 = []
splitRNG rng n = let (rng', rng'') = split rng
  in rng' : splitRNG rng'' (n - 1)


newThreadPool :: Int -> StdGen -> IO ThreadPool
newThreadPool nThreads rng = do
    channel <- newBoundedChan (nThreads * 2)
    let rngs = splitRNG rng nThreads
    sequence_ . map (forkIO . runWorker channel) $ rngs
    return channel
  where
    runWorker :: ThreadPool -> StdGen -> IO ()
    runWorker chan rng' = do
        f <- readBoundedChan chan
        (_, rng'') <- runStateT f rng'
        runWorker chan rng''


enqueue :: ThreadPool -> StIO () -> IO ()
enqueue = writeBoundedChan
