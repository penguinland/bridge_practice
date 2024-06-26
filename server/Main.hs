{-# LANGUAGE OverloadedStrings #-}
module Main where

import Data.Either.Extra(maybeToEither)
import Data.Map(Map, fromList, (!?), map)
import Data.Text(pack)
import Web.Spock(SpockM, text, var, get, root, (<//>), spock, runSpock, json)
import Web.Spock.Config(PoolOrConn(PCNoDatabase), defaultSpockCfg)

import Output(toHtml)
import Topic(Topic, topicName)

import qualified Topics.JacobyTransfers as JacobyTransfers
--import qualified Topics.MinorTransfersScott as MinorTransfers
import qualified Topics.StandardOpeners as StandardOpeners
import qualified Topics.TexasTransfers as TexasTransfers
import qualified Topics.MajorSuitRaises as MajorSuitRaises

{-
import qualified Topics.StandardModernPrecision.OpeningBids as SmpOpenings
import qualified Topics.StandardModernPrecision.OneClubResponses as Smp1CResponses
import qualified Topics.StandardModernPrecision.OneDiamondResponses as Smp1DResponses
import qualified Topics.StandardModernPrecision.Mafia as Mafia
import qualified Topics.StandardModernPrecision.MafiaResponses as MafiaResponses
import qualified Topics.StandardModernPrecision.TwoDiamondOpeners as Smp2DOpen
-}


topicList :: [Topic]
topicList = [ StandardOpeners.topic
            , MajorSuitRaises.topic
            , JacobyTransfers.topic
            , TexasTransfers.topic
            ]

topics :: Map Int Topic
topics = fromList . enumerate $ topicList
  where
    -- I'm surprised this isn't defined in a popular, standard location.
    enumerate :: [a] -> [(Int, a)]
    enumerate = zipWith (,) [0..]

topicNames :: Map Int String
topicNames = Data.Map.map (toHtml . topicName) topics


getTopic :: Int -> Either Int Topic
getTopic i = maybeToEither i (topics !? i)


-- The idea here is that we'll look up each topic index, and get either a topic
-- or an error about it missing, and then we'll collect all that data into
-- either a list of topics or a list of missing indices.
collectResults :: [Either a b] -> Either [a] [b]
collectResults [] = Right []
collectResults (Left l : rest) = case collectResults rest of
                                 Left ll -> Left (l : ll)
                                 Right _ -> Left [l]
collectResults (Right r : rest) = case collectResults rest of
                                  Left l -> Left l
                                  Right rr -> Right (r : rr)


data MySession = EmptySession
data MyAppState = EmptyAppState


main :: IO ()
main = do
    spockCfg <- defaultSpockCfg EmptySession PCNoDatabase EmptyAppState
    runSpock 8080 (spock spockCfg app)


app :: SpockM () MySession MyAppState ()
app = do
    get root $ text "Hello World!"
    get "topics" $ json topicNames
    get ("situation" <//> var) $ \requested ->
        text . pack $ ("requested: " ++ requested)
