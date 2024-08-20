module SupportedTopics(
    assertUniqueTopicIndices
  , topicNames
  , findTopics
) where

import Control.Monad(when)
import Data.Aeson(Value, object, (.=))
import Data.Aeson.Key(fromString)
import Data.Containers.ListUtils(nubOrd)
import Data.Either.Extra(maybeToEither, mapLeft)
import Data.List.Utils(join, split)
import Data.Map(Map, fromList, (!?))

import Output(toHtml)
import Topic(Topic, topicName)

import qualified Topics.StandardOpeners as StandardOpeners
import qualified Topics.MajorSuitRaises as MajorSuitRaises
import qualified Topics.ForcingOneNotrump as ForcingOneNotrump
import qualified Topics.JacobyTransfers as JacobyTransfers
import qualified Topics.Stayman as Stayman
import qualified Topics.TexasTransfers as TexasTransfers
import qualified Topics.Meckwell as Meckwell
import qualified Topics.Jacoby2NT as Jacoby2NT

import qualified Topics.StandardModernPrecision.OpeningBids as SmpOpenings
import qualified Topics.StandardModernPrecision.OneClubResponses as Smp1CResponses
import qualified Topics.StandardModernPrecision.OneDiamondResponses as Smp1DResponses
import qualified Topics.StandardModernPrecision.Mafia as Mafia
import qualified Topics.StandardModernPrecision.MafiaResponses as MafiaResponses
import qualified Topics.StandardModernPrecision.Lampe as Lampe
import qualified Topics.StandardModernPrecision.TwoDiamondOpeners as TwoDiamondOpeners

-- I don't think I ever finished making these topics...
--import qualified Topics.MinorTransfersScott as MinorTransfers


-- The list is the order to display topics on the website. The int is a unique
-- ID for the topic when requesting a situation. That way, you can add new
-- topics to the middle of the list without messing up anyone using the website.
topicList :: [(Int, Topic)]
topicList = [ (10, StandardOpeners.topic)
            , (11, MajorSuitRaises.topic)
            , (12, ForcingOneNotrump.topic)
            , (13, JacobyTransfers.topic)
            , (14, Stayman.topic)
            , (15, TexasTransfers.topic)
            , (16, Meckwell.topic)
            , (17, Jacoby2NT.topic)
            , (50, SmpOpenings.topic)
            , (51, Smp1CResponses.topic)
            , (52, Smp1CResponses.topicExtras)
            , (53, Mafia.topic)
            , (54, MafiaResponses.topic)
            , (55, Smp1DResponses.topic)
            , (70, Lampe.topic)
            , (56, TwoDiamondOpeners.topic)
            ]


-- If topic indices aren't unique, we're gonna have really subtle bugs that will
-- be hard to figure out.
-- TODO: consider making this a compile-time assertion instead, possibly via
-- https://stackoverflow.com/a/6654903
assertUniqueTopicIndices :: IO()
assertUniqueTopicIndices = let
    indices = map fst topicList
  in
    when (indices /= nubOrd indices) (error "duplicate topic indices!?")


topics :: Map Int Topic
topics = fromList topicList


topicNames :: [Value]
topicNames = map toObject topicList
  where
    toObject (index, topic) =
        object [ fromString "index" .= index
               , fromString "name"  .= (toHtml . topicName $ topic)
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