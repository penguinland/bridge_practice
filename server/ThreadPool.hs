--{-# LANGUAGE BangPatterns #-}
module ThreadPool(ThreadPool, newThreadPool, enqueue) where

import Control.Concurrent(forkIO)
import Control.Concurrent.Classy.BoundedChan(
    BoundedChan, newBoundedChan, writeBoundedChan, readBoundedChan)
import Control.Monad(replicateM_)
import Control.Monad.Trans.State.Strict(StateT, runStateT)
import System.Random(StdGen)


type StIO = StateT StdGen IO
type ThreadPool = BoundedChan IO (StIO ())


newThreadPool :: Int -> StdGen -> IO ThreadPool
newThreadPool nThreads rng = do
    channel <- newBoundedChan nThreads
    -- TODO: get separate RNGs for each worker
    replicateM_ nThreads . forkIO $ runWorker channel rng
    return channel
  where
    runWorker :: ThreadPool -> StdGen -> IO ()
    runWorker chan rng' = do
        f <- readBoundedChan chan
        (_, rng'') <- runStateT f rng'
        runWorker chan rng''


enqueue :: ThreadPool -> StIO () -> IO ()
enqueue = writeBoundedChan
