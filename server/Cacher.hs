module Cacher(Cacher, newCacher, getProblem) where

import Control.Concurrent.MVar(MVar, newMVar, takeMVar, putMVar)
import Control.Monad(replicateM_)
import Control.Monad.Trans(liftIO)
import Data.Time(getCurrentTime, diffUTCTime)

import ProblemSet(generate)
import Topic(Topic)
import SituationInstance(SituationInstance, debugString)
import Types(StIO)

import ThreadPool(ThreadPool, enqueue)


data Cacher = Cacher Topic (MVar [SituationInstance]) ThreadPool


targetCacheSize_ :: Int
targetCacheSize_ = 3


newCacher :: ThreadPool -> Topic -> IO Cacher
newCacher pool topic = do
    mv <- newMVar []
    let cacher = Cacher topic mv pool
    replicateM_ targetCacheSize_ $ enqueue pool (makeProblem_ cacher)
    return cacher


makeProblem_ :: Cacher -> StIO ()
makeProblem_ (Cacher t mv _) = do
    start <- liftIO getCurrentTime
    sitInstList <- generate 1 [t]
    stop <- liftIO getCurrentTime
    let sitInst = head sitInstList
        elapsed = diffUTCTime stop start
    liftIO . putStrLn $
        "Created new SituationInstance: " ++ debugString sitInst ++ " in " ++
        show elapsed
    liftIO $ takeMVar mv >>= (putMVar mv . (sitInst:))


getProblem :: Cacher -> StIO SituationInstance
getProblem c@(Cacher t mv p) = do
    sitInsts <- liftIO $ takeMVar mv
    case sitInsts of
      [] -> do
        liftIO $ putMVar mv []
        -- Why is the Cacher empty? Demand for this Topic must be high and
        -- generating SituationInstances must be slow. Increase the cache size
        -- so we can try to stock up during periods of low traffic.
        liftIO . replicateM_ targetCacheSize_ $ enqueue p (makeProblem_ c)
        generate 1 [t] >>= return . head
      (first:rest) -> do
        liftIO $ putMVar mv rest
        liftIO . enqueue p $ makeProblem_ c
        return first
