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


makeTopicRegistry :: ThreadPool -> IO TopicRegistry
makeTopicRegistry pool =
    sequence (map toCacher topicList) >>= (return . fromList)
  where
    toCacher :: (Int, Bool, Topic) -> IO (Int, Cacher)
    toCacher = encapsulate . second (newCacher pool) . (fst3 &&& thd3)
    encapsulate :: (Int, IO Cacher) -> IO (Int, Cacher)
    encapsulate (a, mb) = mb >>= (\b -> return (a, b))


-- The String argument should be a comma-separated list of indices. We return
-- either a description of which indices we don't recognize, or a list of all
-- the corresponding Cachers.
findCachers :: TopicRegistry -> String -> Either String [Cacher]
findCachers registry indices = let
    getCacher i = maybeToEither i (registry !? i)
    foundCachers = map (getCacher . read) . split "," $ indices
    formatError = ("Unknown indices: " ++) . join "," . map show
    -- If there are any Left results, we'll return all of them, and otherwise
    -- we'll return all the Right results.
    collectResults :: [Either a b] -> Either [a] [b]
    collectResults [] = Right []
    collectResults (Left l : rest) = case collectResults rest of
                                     Left ll -> Left (l : ll)
                                     Right _ -> Left [l]
    collectResults (Right r : rest) = case collectResults rest of
                                      Left l -> Left l
                                      Right rr -> Right (r : rr)
  in
    mapLeft formatError . collectResults $ foundCachers
