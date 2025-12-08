{-# LANGUAGE OverloadedStrings #-}
module Main where

import Control.Monad.Trans(liftIO)
import Control.Monad.Trans.State.Strict(runStateT)
import Data.IORef(IORef, newIORef, readIORef, writeIORef)
import Data.Text(pack, toLower, isInfixOf)
import Data.Text.Encoding(decodeUtf8)
import Data.Time(getCurrentTime)
import Network.Wai(Request, requestHeaders)
-- Take the `Dev` off the end to get more standardized server logs.
import Network.Wai.Middleware.RequestLogger(logStdoutDev)
import Network.Wai.Middleware.Static(staticPolicy, addBase)
import System.Random(StdGen, getStdGen)
import Web.Spock(SpockM, file, text, get, root, spock, runSpock, json,
                 getState, middleware, param, request)
import Web.Spock.Config(PoolOrConn(PCNoDatabase), defaultSpockCfg)

import Random(pickItem)

import Cacher(getProblem)
import TopicRegistry(topicNames, findCachers, TopicRegistry, makeTopicRegistry)
import ThreadPool(newThreadPool)

data MySession = EmptySession
data MyAppState = IOState (IORef StdGen) TopicRegistry


main :: IO ()
main = do
    rng <- getStdGen
    (registry, rng') <- runStateT (newThreadPool 4 >>= makeTopicRegistry) rng
    ref <- newIORef rng'
    spockCfg <- defaultSpockCfg EmptySession PCNoDatabase (IOState ref registry)
    runSpock 8765 (spock spockCfg app)


isMobile :: Request -> Bool
isMobile req = let
    headers = requestHeaders req
    maybeUseragent = fmap (toLower . decodeUtf8) . lookup "User-Agent" $ headers
    mobileAgents = ["mobile", "android", "iphone", "ipad", "blackberry"]
    isMobileAgent useragent = any (`isInfixOf` useragent) mobileAgents
  in
    maybe False isMobileAgent maybeUseragent


app :: SpockM () MySession MyAppState ()
app = do
    middleware logStdoutDev
    -- NOTE: these next two are relative to the current working directory when
    -- you execute the program! So, you need to run it from the right place.
    middleware (staticPolicy (addBase "static"))
    get root $ do
        req <- request
        file "text/html" $ if isMobile req then "static/mobile.html"
                                           else "static/index.html"
    get "topics" $ json topicNames
    get "situation" $ do
        liftIO $ getCurrentTime >>= print
        requested <- param "topics"
        (IOState ioRng registry) <- getState
        let malformed = Left "no topics selected"
        case maybe malformed (findCachers registry) requested of
            Left err -> text . pack $ err
            Right cachers -> do
                rng <- liftIO . readIORef $ ioRng
                (sitInst, rng') <- liftIO $
                    runStateT (pure cachers >>= pickItem >>= getProblem) rng
                liftIO . writeIORef ioRng $ rng'
                json sitInst
