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
  in staticAssert (null dups) ("topic list has duplicate IDs: " ++ show dups))


$(let dups = duplicatesOf . map (refName . thd3) $ topicList
  in staticAssert (null dups)
    ("topics have duplicate debug names: " ++ show dups))


-- Something about $(...) seems to mess up indentation parsing, I think? If you
-- try adding newlines where they logically belong, this becomes unparseable.
$(let assertUniqueNames topic = (let dups = (duplicatesOf . survey sitRef . topicSituations $ topic) in staticAssert (null dups) ("duplicate situation names for topic " ++ (toMonospace . topicName $ topic) ++ ": " ++ show dups))
  in mconcatMapM (assertUniqueNames . thd3) $ topicList)
