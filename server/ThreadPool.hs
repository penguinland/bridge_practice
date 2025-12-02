module ThreadPool(ThreadPool, newThreadPool, enqueue, StIO) where

import Control.Concurrent(forkIO)
import Control.Concurrent.Classy.BoundedChan(
    BoundedChan, newBoundedChan, writeBoundedChan, readBoundedChan)
import Control.Monad.Trans(liftIO)
import Control.Monad.Trans.State.Strict(StateT, runStateT, get)
import System.Random(StdGen, split)


type StIO = StateT StdGen IO
type ThreadPool = BoundedChan IO (StIO ())


newThreadPool :: Int -> StIO ThreadPool
newThreadPool nThreads = do
    rng <- get
    channel <- liftIO $ newBoundedChan (nThreads * 2)
    let rngs = splitRNG nThreads rng
    sequence_ . map (liftIO . forkIO . runWorker channel) $ rngs
    return channel
  where
    splitRNG :: Int -> StdGen -> [StdGen]
    splitRNG 0 _ = []
    splitRNG n rng' = let (rng'', rng''') = split rng'
      in rng'' : splitRNG (n - 1) rng'''
    runWorker :: ThreadPool -> StdGen -> IO ()
    runWorker chan rng' = do
        f <- readBoundedChan chan
        -- TODO: make sure this doesn't crash
        (_, rng'') <- runStateT f rng'
        runWorker chan rng''


enqueue :: ThreadPool -> StIO () -> IO ()
enqueue = writeBoundedChan
