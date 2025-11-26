-- We use a compile-time assertion that every supported Topic has a unique ID. Doing things at
-- compile time requires using Template Haskell.
{-# LANGUAGE TemplateHaskell #-}

module SupportedTopics(
    topicNames
  , findTopics
  , getNamedTopic
  , topicList  -- Used in Assertions.hs to do compile-time checks
) where

import Data.Aeson(Value, object, (.=))
import Data.Aeson.Key(fromString)
import Data.Either.Extra(maybeToEither, mapLeft)
import Data.List(find)
import Data.List.Utils(join, split)
import Data.Map(Map, fromList, (!?))
import Data.Tuple.Extra((&&&))
import Data.Tuple.Utils(fst3, thd3)

import Output(toHtml)
import Topic(Topic, refName, topicName)

import qualified Topics.StandardOpeners as StandardOpeners
import qualified Topics.TwoOverOne as TwoOverOne
import qualified Topics.MajorSuitRaises as MajorSuitRaises
import qualified Topics.Overcalls as Overcalls
import qualified Topics.TakeoutDoubles as TakeoutDoubles
import qualified Topics.TransfersOver1MX as TransfersOver1MX
import qualified Topics.ForcingOneNotrump as ForcingOneNotrump
import qualified Topics.JacobyTransfers as JacobyTransfers
import qualified Topics.Stayman as Stayman
import qualified Topics.PuppetStayman as PuppetStayman
import qualified Topics.MuppetStayman as MuppetStayman
import qualified Topics.TexasTransfers as TexasTransfers
import qualified Topics.Smolen as Smolen
import qualified Topics.ThreeLevelResponsesTo1N as B1N3X
import qualified Topics.Meckwell as Meckwell
import qualified Topics.DONT as DONT
import qualified Topics.Cappelletti as Cappelletti
import qualified Topics.Woolsey as Woolsey
import qualified Topics.Lebensohl as Lebensohl
import qualified Topics.Jacoby2NT as Jacoby2NT

import qualified Topics.StandardModernPrecision.OpeningBids as SmpOpenings
import qualified Topics.StandardModernPrecision.OneClubResponses as Smp1CResponses
import qualified Topics.StandardModernPrecision.OneDiamondResponses as Smp1DResponses
import qualified Topics.StandardModernPrecision.Mafia as Mafia
import qualified Topics.StandardModernPrecision.MafiaResponses as MafiaResponses
--import qualified Topics.StandardModernPrecision.Lampe as Lampe
import qualified Topics.StandardModernPrecision.TwoDiamondOpeners as TwoDiamondOpeners
import qualified Topics.StandardModernPrecision.TripleFourOne as TripleFourOne

-- I don't think I ever finished making these topics...
--import qualified Topics.MinorTransfersScott as MinorTransfers


-- The list is the order to display topics on the website. The int is a unique
-- ID for the topic when requesting a situation. That way, you can add new
-- topics to the middle of the list without messing up anyone using the website.
-- The boolean is whether this topic should be selected by default. If topic
-- indices aren't unique, we're gonna have really subtle bugs that will be hard
-- to figure out.
topicList :: [(Int, Bool, Topic)]
topicList =
    [ (10, True,  StandardOpeners.topic)
    , (29, True,  TwoOverOne.topic)
    , (11, True,  MajorSuitRaises.topic)
    , (26, True,  Overcalls.topic)
    , (24, True,  TakeoutDoubles.topic)
    , (12, True,  ForcingOneNotrump.topic)
    , (13, True,  JacobyTransfers.topic)
    , (14, True,  Stayman.topic)
    , (15, False, TexasTransfers.topic)
    , (23, False, Smolen.topic)
    , (28, False, B1N3X.topic)
    , (22, False, PuppetStayman.topic)
    , (27, False, MuppetStayman.topic)
    , (16, False, Meckwell.topic)
    , (19, False, DONT.topic)
    , (18, False, Cappelletti.topic)
    , (21, False, Woolsey.topic)
    , (20, False, Lebensohl.topic)
    , (17, True,  Jacoby2NT.topic)
    , (25, False, TransfersOver1MX.topic)
    , (50, False, SmpOpenings.topic)
    , (51, False, Smp1CResponses.topic)
    , (52, False, Smp1CResponses.topicExtras)
    , (53, False, Mafia.topic)
    , (54, False, MafiaResponses.topic)
    , (57, False, TripleFourOne.topic)
    , (55, False, Smp1DResponses.topic)
    --, (70, False, Lampe.topic)
    , (56, False, TwoDiamondOpeners.topic)
    ]


topics :: Map Int Topic
topics = fromList . map (fst3 &&& thd3) $ topicList


topicNames :: [Value]
topicNames = map toObject topicList
  where
    toObject (index, selectByDefault, topic) =
        object [ fromString "index"             .= index
               , fromString "select_by_default" .= selectByDefault
               , fromString "name"              .= (toHtml . topicName $ topic)
               ]


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


getNamedTopic :: String -> Maybe Topic
getNamedTopic name = find ((== name) . refName) . map thd3 $ topicList
