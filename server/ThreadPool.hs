module ThreadPool(ThreadPool, newThreadPool, enqueue) where

import Control.Concurrent(forkIO)
import Control.Concurrent.Classy.BoundedChan(
    BoundedChan, newBoundedChan, writeBoundedChan, readBoundedChan)
import Control.Exception(handle)
import Control.Monad.Trans(liftIO)
import Control.Monad.Trans.State.Strict(runStateT, get, put)
import Data.Tuple.Extra(first)
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
        (_, rng2) <- handle (recordError f) (runStateT f rng1)
        runWorker chan errorSaver rng2
      where
        recordError f e = do
            saveError errorSaver e
            -- Re-enqueue a failed run: we still need to precompute a
            -- SituationInstance for this Cacher. However, do it from a separate
            -- thread! If the channel is full, enqueuing will block until there
            -- is capacity, and this thread itself is the thing that can free up
            -- more capacity.
            _ <- forkIO $ enqueue chan f  -- Ignore the threadID: we don't care
            -- If this situation failed only because it's rare and our RNG got
            -- unlucky, change the RNG seed for next time in the hopes that it
            -- can succeed next time.
            return . first (const ()) . split $ rng1


enqueue :: ThreadPool -> StIO () -> IO ()
enqueue = writeBoundedChan
