{-# LANGUAGE TemplateHaskell #-}
-- These assertions are in their own file because anything used within them
-- (e.g., topicList) must be already compiled in a separate file.
module Assertions where

import Data.Tuple.Utils(fst3)

import CompileTime(staticAssert, duplicatesOf)
import SupportedTopics(topicList)

$(let dups = duplicatesOf . map fst3 $ topicList
  in staticAssert (null dups) ("topic list has duplicate IDs: " ++ show dups))
