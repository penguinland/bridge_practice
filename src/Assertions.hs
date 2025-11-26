{-# LANGUAGE TemplateHaskell #-}
-- These assertions are in their own file because anything used within them
-- (e.g., topicList) must be already compiled in a separate file.
module Assertions where

import Control.Monad.Extra(mconcatMapM)
import Data.Tuple.Utils(fst3, thd3)

import CompileTime(staticAssert, duplicatesOf)
import Output(toMonospace)
import Situation(sitRef)
import SupportedTopics(topicList)
import Topic(collect, topicName)

$(let dups = duplicatesOf . map fst3 $ topicList
  in staticAssert (null dups) ("topic list has duplicate IDs: " ++ show dups))

{-
$(let assertUniqueNames top =
    (let dups = duplicatesOf . collect sitRef . topicSituations $ top
     in staticAssert (null dups) ("topic " ++ topicName top ++ " has duplicate situation names: " ++ show dups))
  in map (assertUniqueNames . thd3) topicList)
-}

$(let assertUniqueNames topic = (let dups = (duplicatesOf . collect sitRef $ topic) in staticAssert (null dups) ("duplicate situation names for topic " ++ (toMonospace . topicName $ topic) ++ ": " ++ show dups))
  in mconcatMapM (assertUniqueNames . thd3) $ topicList)
