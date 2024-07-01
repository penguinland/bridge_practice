{-# LANGUAGE OverloadedStrings #-}
module Main where

import Control.Monad.Trans(liftIO)
import Data.Either.Extra(maybeToEither, mapLeft)
import Data.IORef(IORef, newIORef, readIORef, writeIORef)
import Data.List.Utils(join, split)
import Data.Map(Map, fromList, (!?))
import Data.Text(pack)
import Network.Wai.Middleware.Static(staticPolicy, addBase)
import System.Random(StdGen, getStdGen)
import Web.Spock(SpockM, file, text, var, get, root, (<//>), spock, runSpock, json, getState, middleware)
import Web.Spock.Config(PoolOrConn(PCNoDatabase), defaultSpockCfg)

import Output(toHtml)
import ProblemSet(generate)
import Topic(Topic, topicName)

import qualified Topics.JacobyTransfers as JacobyTransfers
import qualified Topics.StandardOpeners as StandardOpeners
import qualified Topics.TexasTransfers as TexasTransfers
import qualified Topics.MajorSuitRaises as MajorSuitRaises

import qualified Topics.StandardModernPrecision.OpeningBids as SmpOpenings
import qualified Topics.StandardModernPrecision.OneClubResponses as Smp1CResponses
import qualified Topics.StandardModernPrecision.OneDiamondResponses as Smp1DResponses
import qualified Topics.StandardModernPrecision.Mafia as Mafia
import qualified Topics.StandardModernPrecision.MafiaResponses as MafiaResponses

-- I don't think I ever finished making these topics...
--import qualified Topics.MinorTransfersScott as MinorTransfers
--import qualified Topics.StandardModernPrecision.TwoDiamondOpeners as Smp2DOpen


topicList :: [Topic]
topicList = [ StandardOpeners.topic
            , MajorSuitRaises.topic
            , JacobyTransfers.topic
            , TexasTransfers.topic
            , SmpOpenings.topic
            , Smp1CResponses.topic
            , Smp1DResponses.topic
            , Mafia.topic
            , MafiaResponses.topic
            ]

topics :: Map Int Topic
topics = fromList . enumerate $ topicList
  where
    -- I'm surprised this isn't defined in a popular, standard location.
    enumerate :: [a] -> [(Int, a)]
    enumerate = zipWith (,) [0..]

topicNames :: Map Int String
topicNames = fmap (toHtml . topicName) topics


getTopic :: Int -> Either Int Topic
getTopic i = maybeToEither i (topics !? i)


-- If there are any Left results, we'll return all of them, and otherwise we'll
-- return all the Right results.
collectResults :: [Either a b] -> Either [a] [b]
collectResults [] = Right []
collectResults (Left l : rest) = case collectResults rest of
                                 Left ll -> Left (l : ll)
                                 Right _ -> Left [l]
collectResults (Right r : rest) = case collectResults rest of
                                  Left l -> Left l
                                  Right rr -> Right (r : rr)


-- The argument should be a comma-separated list of indices. We return either a
-- description of which indices we don't recognize, or a list of all the
-- corresponding topics.
findTopics :: String -> Either String [Topic]
findTopics indices = let
    results = collectResults . map (getTopic . read) . split "," $ indices
    formatError = ("Unknown indices: " ++) . join "," . map show
  in
    mapLeft formatError results


data MySession = EmptySession
data MyAppState = IoRng (IORef StdGen)


main :: IO ()
main = do
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
    get ("situation" <//> var) $ \requested -> do
        (IoRng ioRng) <- getState
        rng <- liftIO . readIORef $ ioRng
        case findTopics requested of
            Left err -> text . pack $ err
            Right topics -> do
                (sitInstList, rng') <- liftIO $ generate 1 topics rng
                liftIO . writeIORef ioRng $ rng'
                json . head $ sitInstList
