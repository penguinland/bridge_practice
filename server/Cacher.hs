{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
--module Cacher(Cacher, newCacher, getProblem) where
module Cacher(Cacher, newCacher) where

import Control.Concurrent.MVar(MVar, newMVar, takeMVar, putMVar)
import Control.Concurrent.Pool(Pool, newPool, queue, Task, runTask)
import Control.Monad(replicateM_)
import Control.Monad.Trans(liftIO)
import Control.Monad.Trans.State.Strict(StateT)
import System.Random(StdGen)

import ProblemSet(generate)
import Topic(Topic)
import SituationInstance(SituationInstance)


type StIO = StateT StdGen IO
type PoolType = Pool StIO () ()

instance Task StIO a r where
    runTask = liftIO runTask


data Cacher = Cacher Topic (MVar [SituationInstance]) PoolType


makeThreadPool :: StIO PoolType
makeThreadPool = newPool 4 False  -- 4 threads, don't return results


newCacher :: PoolType -> Int -> Topic -> StIO Cacher
newCacher pool cacheCount topic = do
    mv <- liftIO $ newMVar []
    let cacher = Cacher topic mv pool
    --liftIO . replicateM_ cacheCount $ queue pool (makeProblem_ cacher) ()
    queue pool (makeProblem_ cacher) ()
    return cacher


makeProblem_ :: Cacher -> StIO ()
makeProblem_ (Cacher t mv _) = do
    sitInst <- generate 1 [t]
    sitInsts <- liftIO $ takeMVar mv
    liftIO $ putMVar mv ((sitInst !! 0) : sitInsts)

{-
getProblem :: Cacher -> StIO SituationInstance
getProblem c@(Cacher t mv p) = do
    sitInsts <- liftIO $ takeMVar mv
    case sitInsts of
      [] -> do
        liftIO $ putMVar mv []
        newSitInsts <- generate 1 [t]
        return $ newSitInsts !! 0
      (first:rest) -> do
        liftIO $ putMVar mv rest
        liftIO $ queue p (makeProblem_ c) ()
        return first
-}
