module ThreadPool(ThreadPool, newThreadPool, enqueue, StIO) where

import Control.Concurrent(forkIO)
import Control.Concurrent.Classy.BoundedChan(
    BoundedChan, newBoundedChan, writeBoundedChan, readBoundedChan)
import Control.Monad.Trans(liftIO)
import Control.Monad.Trans.State.Strict(StateT, runStateT, get, put)
import System.Random(StdGen, split)


type StIO = StateT StdGen IO
type ThreadPool = BoundedChan IO (StIO ())


newThreadPool :: Int -> StIO ThreadPool
newThreadPool nThreads = do
    rng <- get
    let (rngs, finalRng) = splitRNG nThreads rng
    put finalRng
    channel <- liftIO $ newBoundedChan (nThreads * 2)
    sequence_ . map (liftIO . forkIO . runWorker channel) $ rngs
    return channel
  where
    splitRNG :: Int -> StdGen -> ([StdGen], StdGen)
    splitRNG 0 finalRng = ([], finalRng)
    splitRNG n rng1 = let
        (rng2, rng3) = split rng1
        (rngs, finalRng) = splitRNG (n - 1) rng3
      in (rng2:rngs, finalRng)
    runWorker :: ThreadPool -> StdGen -> IO ()
    runWorker chan rng1 = do
        f <- readBoundedChan chan
        -- TODO: make sure this doesn't crash
        (_, rng2) <- runStateT f rng1
        runWorker chan rng2


enqueue :: ThreadPool -> StIO () -> IO ()
enqueue = writeBoundedChan
