module Cacher(Cacher, newCacher, getProblem) where

import Control.Concurrent.MVar(MVar, newMVar, takeMVar, putMVar)
import Control.Monad(replicateM_)
import Control.Monad.Trans(liftIO)

import ProblemSet(generate)
import Topic(Topic)
import SituationInstance(SituationInstance)

import ThreadPool(ThreadPool, enqueue, StIO)


data Cacher = Cacher Topic (MVar [SituationInstance]) ThreadPool


newCacher :: ThreadPool -> Int -> Topic -> StIO Cacher
newCacher pool cacheCount topic = do
    mv <- liftIO $ newMVar []
    let cacher = Cacher topic mv pool
    liftIO . replicateM_ cacheCount $ enqueue pool (makeProblem_ cacher)
    return cacher


makeProblem_ :: Cacher -> StIO ()
makeProblem_ (Cacher t mv _) = do
    -- TODO: make sure this gets evaluated in a timely manner
    sitInst <- generate 1 [t]
    sitInsts <- liftIO $ takeMVar mv
    liftIO $ putMVar mv ((sitInst !! 0) : sitInsts)


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
        liftIO . enqueue p $ makeProblem_ c
        return first
