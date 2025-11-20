{-# LANGUAGE TemplateHaskell #-}
module Assertions where

import CompileTime(staticAssert)
import SupportedTopics(topicList)

$(staticAssert (length topicList >= 0) "topic list has negative length")
