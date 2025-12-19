{-# LANGUAGE TemplateHaskell #-}
-- These assertions are in their own file because anything used within them
-- (e.g., topicList) must be already compiled in a separate file.
module Assertions where

import Control.Monad.Extra(mconcatMapM)
import Data.Tuple.Utils(fst3, thd3)

import Collection(survey)
import CompileTime(staticAssert, duplicatesOf)
import Output(toMonospace)
import Situation(sitRef)
import SupportedTopics(topicList)
import Topic(Topic(..))


$(let dups = duplicatesOf . map fst3 $ topicList
      errorMessage = "topic list has duplicate IDs: " ++ show dups
  in staticAssert (null dups) errorMessage)


$(let dups = duplicatesOf . map (refName . thd3) $ topicList
      errorMessage = "topics have duplicate debug names: " ++ show dups
  in staticAssert (null dups) errorMessage)


$(let assertUniqueNames topic = let
          dups = duplicatesOf . survey sitRef . topicSituations $ topic
          errorMessage = "duplicate situation names for topic " ++
                         (toMonospace . topicName $ topic) ++ ": " ++ show dups
        in
          staticAssert (null dups) errorMessage
  in mconcatMapM (assertUniqueNames . thd3) topicList)
