{-# LANGUAGE OverloadedStrings #-}
module Main where

import Data.Map(Map, fromList)
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


data MySession = EmptySession
data MyAppState = EmptyAppState


topicList :: [Topic]
topicList = [ StandardOpeners.topic
            , MajorSuitRaises.topic
            , JacobyTransfers.topic
            , TexasTransfers.topic
            ]

topics :: Map Int String
topics = fromList . enumerate . map (toHtml . topicName) $ topicList


-- This is defined in tidal, but I'm surprised it's not more standard.
enumerate :: [a] -> [(Int, a)]
enumerate = zipWith (,) [0..]


main :: IO ()
main = do
    spockCfg <- defaultSpockCfg EmptySession PCNoDatabase EmptyAppState
    runSpock 8080 (spock spockCfg app)


app :: SpockM () MySession MyAppState ()
app = do
    get root $ text "Hello World!"
    get "topics" $ json topics
    get ("situation" <//> var) $ \requested -> text . pack $ ("requested: " ++ requested)
