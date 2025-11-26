-- We use Template Haskell to check at compile time whether every Topic ID is
-- unique. This function must be compiled before SupportedTopics.hs, which is
-- why it's a separate file.
{-# LANGUAGE TemplateHaskell #-}

module CompileTime(staticAssert, duplicatesOf) where

import Control.Arrow((&&&))
import Control.Monad(unless)
import Data.List(sort)
import Data.Containers.ListUtils(nubOrd)
import qualified Language.Haskell.TH.Syntax as THS


duplicatesOf :: Ord a => [a] -> [a]
duplicatesOf = nubOrd . map fst . filter (uncurry (==)) . uncurry zip .
               (init &&& tail) . sort


staticAssert :: Bool -> String -> THS.Q [a]
staticAssert condition message = do
    unless condition $ fail ("Compile-time assertion failed: " ++ message)
    return []
