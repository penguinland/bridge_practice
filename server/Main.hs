{-# LANGUAGE OverloadedStrings #-}
module Main where

import Data.Either.Extra(maybeToEither, mapLeft)
import Data.List.Utils(join, split)
import Data.Map(Map, fromList, (!?))
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


findTopics :: String -> Either String [Topic]
findTopics indices = let
    results = collectResults . map (getTopic . read) . split "," $ indices
    formatError = ("Unknown indices: " ++) . join "," . map show
  in
    mapLeft formatError results


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
    get ("situation" <//> var) $ \requested -> case findTopics requested of
        Left err -> text . pack $ err
        Right topics -> json . map (toHtml . topicName) $ topics
