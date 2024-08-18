{-# LANGUAGE OverloadedStrings #-}
module Main where

import Control.Monad.Trans(liftIO)
import Data.IORef(IORef, newIORef, readIORef, writeIORef)
import Data.Text(pack)
import Network.Wai.Middleware.Static(staticPolicy, addBase)
import System.Random(StdGen, getStdGen)
import Web.Spock(SpockM, file, text, get, root, spock, runSpock, json,
                 getState, middleware, param)
import Web.Spock.Config(PoolOrConn(PCNoDatabase), defaultSpockCfg)

import ProblemSet(generate)

import SupportedTopics(assertUniqueTopicIndices, topicNames, findTopics)


data MySession = EmptySession
data MyAppState = IoRng (IORef StdGen)


main :: IO ()
main = do
    assertUniqueTopicIndices
    rng <- getStdGen
    ref <- newIORef rng
    spockCfg <- defaultSpockCfg EmptySession PCNoDatabase (IoRng ref)
    runSpock 8765 (spock spockCfg app)


app :: SpockM () MySession MyAppState ()
app = do
    -- NOTE: these first two are relative to the current working directory when
    -- you execute the program! So, you need to run it from the right place.
    middleware (staticPolicy (addBase "static"))
    get root $ file "text/html" "static/index.html"
    get "topics" $ json topicNames
    get ("situation") $ do
        requested <- param "topics"
        case maybe (Left "no topics selected") findTopics requested of
            Left err -> text . pack $ err
            Right topics -> do
                (IoRng ioRng) <- getState
                rng <- liftIO . readIORef $ ioRng
                (sitInstList, rng') <- liftIO $ generate 1 topics rng
                liftIO . writeIORef ioRng $ rng'
                json . head $ sitInstList
