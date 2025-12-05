module ThreadPool(ThreadPool, newThreadPool, enqueue) where

import Control.Concurrent(forkIO)
import Control.Concurrent.Classy.BoundedChan(
    BoundedChan, newBoundedChan, writeBoundedChan, readBoundedChan)
import Control.Exception(handle)
import Control.Monad.Trans(liftIO)
import Control.Monad.Trans.State.Strict(runStateT, get, put)
import System.Random(StdGen, split)

import Types(StIO)

import ErrorSaver(ErrorSaver, newErrorSaver, saveError)

type ThreadPool = BoundedChan IO (StIO ())


newThreadPool :: Int -> StIO ThreadPool
newThreadPool nThreads = do
    errorSaver <- liftIO $ newErrorSaver
    rng <- get
    let (rngs, finalRng) = splitRNG nThreads rng
    put finalRng
    channel <- liftIO $ newBoundedChan (nThreads * 2)
    sequence_ . map (liftIO . forkIO . runWorker channel errorSaver) $ rngs
    return channel
  where
    splitRNG :: Int -> StdGen -> ([StdGen], StdGen)
    splitRNG 0 finalRng = ([], finalRng)
    splitRNG n rng1 = let
        (rng2, rng3) = split rng1
        (rngs, finalRng) = splitRNG (n - 1) rng3
      in (rng2:rngs, finalRng)
    runWorker :: ThreadPool -> ErrorSaver -> StdGen -> IO ()
    runWorker chan errorSaver rng1 = do
        f <- readBoundedChan chan
        -- TODO: make sure this doesn't crash
        (_, rng2) <- handle (recordError errorSaver) (runStateT f rng1)
        runWorker chan errorSaver rng2
      where
        recordError saver e = do
            saveError saver e
            -- If this situation failed only because it's rare and our RNG got
            -- unlucky, change the RNG seed for next time in the hopes that it
            -- can succeed again later.
            let (_, rng2) = split rng1
            -- No need to re-enqueue a failed run: if the Cacher becomes empty,
            -- it will fill itself up again.
            -- TODO: revisit this if we get so much traffic that a temporarily
            -- empty Cacher becomes an actual issue.
            return ((), rng2)


enqueue :: ThreadPool -> StIO () -> IO ()
enqueue = writeBoundedChan
