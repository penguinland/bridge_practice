module Cacher(Cacher, newCacher, getProblem) where

import Control.Concurrent.MVar(MVar, newMVar, takeMVar, putMVar)
import Control.Concurrent.Pool(Pool, newPool, queue)
import Control.Monad(replicateM)
import Control.Monad.Trans(liftIO)
import Control.Monad.Trans.State.Strict(StateT)
import System.Random(StdGen)

import ProblemSet(generate)
import Topic(Topic)
import SituationInstance(SituationInstance)


data Cacher = Cacher Topic (MVar [SituationInstance])


_threadPool :: Pool (StateT StdGen IO) Cacher a
_threadPool = newPool 4 False  -- 4 threads, don't return results


newCacher :: Topic -> Int -> StateT StdGen IO Cacher
newCacher topic cacheCount = do
    mv <- liftIO $ newMVar []
    let cacher = Cacher topic mv
    replicateM cacheCount $ queue _threadPool makeProblem_ cacher
    return cacher


makeProblem_ :: Cacher -> StateT StdGen IO ()
makeProblem_ (Cacher t mv) = do
    sitInst <- generate 1 [t]
    sitInsts <- liftIO $ takeMVar mv
    liftIO $ putMVar mv ((sitInst !! 0) : sitInsts)


getProblem :: Cacher -> StateT StdGen IO SituationInstance
getProblem c@(Cacher t mv) = do
    sitInsts <- liftIO $ takeMVar mv
    case sitInsts of
      [] -> do
        liftIO $ putMVar mv []
        newSitInsts <- generate 1 [t]
        return $ newSitInsts !! 0
      (first:rest) -> do
        liftIO $ putMVar mv rest
        queue _threadPool makeProblem_ c
        return first
