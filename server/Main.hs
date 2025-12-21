{-# LANGUAGE OverloadedStrings #-}
module Main where

import Control.Monad.Trans(liftIO)
import Control.Monad.Trans.Class(lift)
import Control.Monad.Trans.State.Strict(runStateT, evalStateT, StateT)
-- We need to distinguish State.get from Scotty.get
import qualified Control.Monad.Trans.State.Strict as State
import Data.IORef(IORef, newIORef, readIORef, writeIORef)
import Data.Text.Lazy(pack, toLower, isInfixOf)
import Data.Text.Lazy.Encoding(decodeUtf8)
import Data.Time(getCurrentTime)
import Network.Wai(Request, requestHeaders)
-- Take the `Dev` off the end to get more standardized server logs.
import Network.Wai.Middleware.RequestLogger(logStdoutDev)
import Network.Wai.Middleware.Static(staticPolicy, addBase)
import System.Random(StdGen, getStdGen)

import Web.Scotty(ScottyM, file, text, get, post, scotty, json,
                  middleware, param, request)


--import Web.Spock(SpockM, file, text, get, post, root, spock, runSpock, json,
--                 getState, middleware, param, request)
--import Web.Spock.Config(PoolOrConn(PCNoDatabase), defaultSpockCfg)

import Random(pickItem)

import Cacher(getProblem)
import TopicRegistry(topicNames, findCachers, TopicRegistry, makeTopicRegistry)
import ThreadPool(newThreadPool)


data MyAppState = IOState (IORef StdGen) TopicRegistry


main :: IO ()
main = do
    (pool, rng) <- getStdGen >>= runStateT (newThreadPool 4)
    registry <- makeTopicRegistry pool
    ref <- newIORef rng
    scotty 8765 . evalStateT app $ IOState ref registry


isMobile :: Request -> Bool
isMobile req = let
    headers = requestHeaders req
    maybeUseragent = fmap (toLower . decodeUtf8) . lookup "User-Agent" $ headers
    mobileAgents = ["mobile", "android", "iphone", "ipad", "blackberry"]
    isMobileAgent useragent = any (`isInfixOf` useragent) mobileAgents
  in
    maybe False isMobileAgent maybeUseragent


app :: StateT MyAppState ScottyM ()
app = do
    lift $ middleware logStdoutDev
    -- NOTE: these next two are relative to the current working directory when
    -- you execute the program! So, you need to run it from the right place.
    lift $ middleware (staticPolicy (addBase "static"))
    lift $ get "/" $ do
        req <- request
        file "text/html" $ if isMobile req then "static/mobile.html"
                                           else "static/index.html"
    lift $ post "/bugreport" $ text . pack $ "bug report received!\n"
    lift $ get "/topics" $ json topicNames
    lift $ get "/situation" $ do
        liftIO $ getCurrentTime >>= print
        requested <- param "topics"
        (IOState ioRng registry) <- State.get
        let malformed = Left "no topics selected"
        case maybe malformed (findCachers registry) requested of
            Left err -> text . pack $ err
            Right cachers -> do
                rng <- liftIO $ readIORef ioRng
                (sitInst, rng') <- liftIO $
                    runStateT (pure cachers >>= pickItem >>= getProblem) rng
                liftIO $ writeIORef ioRng rng'
                json sitInst
