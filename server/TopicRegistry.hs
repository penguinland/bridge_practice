module TopicRegistry(
    TopicRegistry
  , topicNames
  , makeTopicRegistry
  , findCachers
) where

import Data.Aeson(Value, object, (.=))
import Data.Aeson.Key(fromString)
import Data.Either.Extra(maybeToEither, mapLeft)
import Data.List.Utils(join, split)
import Data.Map(Map, fromList, (!?))
import Data.Tuple.Extra((&&&), second)
import Data.Tuple.Utils(fst3, thd3)

import Output(toHtml)
import SupportedTopics(topicList)
import Topic(Topic, topicName)
import Types(StIO)

import Cacher(Cacher, newCacher)
import ThreadPool(ThreadPool)


type TopicRegistry = Map Int Cacher


topicNames :: [Value]
topicNames = map toObject topicList
  where
    toObject (index, selectByDefault, topic) =
        object [ fromString "index"             .= index
               , fromString "select_by_default" .= selectByDefault
               , fromString "name"              .= (toHtml . topicName $ topic)
               ]


makeTopicRegistry :: ThreadPool -> StIO TopicRegistry
makeTopicRegistry pool =
    sequence (map toCacher topicList) >>= (return . fromList)
  where
    toCacher :: (Int, Bool, Topic) -> StIO (Int, Cacher)
    toCacher = encapsulate . second (newCacher pool) . (fst3 &&& thd3)
    encapsulate :: (Int, StIO Cacher) -> StIO (Int, Cacher)
    encapsulate (a, mb) = mb >>= (\b -> return (a, b))


-- If there are any Left results, we'll return all of them, and otherwise we'll
-- return all the Right results.
collectResults_ :: [Either a b] -> Either [a] [b]
collectResults_ [] = Right []
collectResults_ (Left l : rest) = case collectResults_ rest of
                                  Left ll -> Left (l : ll)
                                  Right _ -> Left [l]
collectResults_ (Right r : rest) = case collectResults_ rest of
                                   Left l -> Left l
                                   Right rr -> Right (r : rr)


-- The String argument should be a comma-separated list of indices. We return
-- either a description of which indices we don't recognize, or a list of all
-- the corresponding Cachers.
findCachers :: TopicRegistry -> String -> Either String [Cacher]
findCachers registry indices = let
    getCacher i = maybeToEither i (registry !? i)
    foundCachers = map (getCacher . read) . split "," $ indices
    formatError = ("Unknown indices: " ++) . join "," . map show
  in
    mapLeft formatError . collectResults_ $ foundCachers
