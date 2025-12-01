module Cacher(Cacher, newCacher, getProblem) where

import Control.Concurrent.MVar(MVar, newMVar, takeMVar, putMVar)
import Control.Monad.Trans(liftIO)
import Control.Monad.Trans.State.Strict(StateT)
import System.Random(StdGen)

import ProblemSet(generate)
import Topic(Topic)
import SituationInstance(SituationInstance)


data Cacher = Cacher Topic (MVar [SituationInstance])


newCacher :: Topic -> Int -> StateT StdGen IO Cacher
newCacher topic cacheCount = do
    mv <- liftIO $ newMVar []
    let cacher = Cacher topic mv
    -- Call `makeProblem_ cacher` cacheCount times in parallel
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
        -- Call `makeProblem_ c` in a separate thread
        return first
