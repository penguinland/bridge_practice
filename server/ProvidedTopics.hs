module ProvidedTopics(
    topicNames
  , findTopics
) where

import Data.Aeson(Value, object, (.=))
import Data.Aeson.Key(fromString)
import Data.Either.Extra(maybeToEither, mapLeft)
import Data.List.Utils(join, split)
import Data.Map(Map, fromList, (!?))
import Data.Tuple.Extra((&&&))
import Data.Tuple.Utils(fst3, thd3)

import Output(toHtml)
import SupportedTopics(topicList)
import Topic(Topic, topicName)


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
