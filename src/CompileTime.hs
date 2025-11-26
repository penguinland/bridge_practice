-- We use Template Haskell to check at compile time whether every Topic ID is
-- unique. This function must be compiled before SupportedTopics.hs, which is
-- why it's a separate file.
{-# LANGUAGE TemplateHaskell #-}

module CompileTime(
    compileTimeAssertUniqueTopicIDs
  , staticAssert
  , duplicatesOf
) where

import Control.Arrow((&&&))
import Control.Monad(unless)
import Data.List(sort)
import Data.Containers.ListUtils(nubOrd)
import qualified Language.Haskell.TH.Syntax as THS


duplicatesOf :: Ord a => [a] -> [a]
duplicatesOf = nubOrd . map fst . filter (uncurry (==)) . uncurry zip .
               (init &&& tail) . sort


-- This is used in SupportedTopics.hs to ensure that every Topic has a unique
-- ID. It takes a list of (Int, Bool, Topic) tuples, and either fails the
-- assertion that all ints are unique, or else returns that same list.
compileTimeAssertUniqueTopicIDs :: THS.Q THS.Exp -> THS.Q THS.Exp
compileTimeAssertUniqueTopicIDs qexp = let
    getId :: THS.Exp -> Int
    getId (THS.TupE [Just (THS.LitE (THS.IntegerL i)), _, _]) = fromInteger i
    getId e = error $ "Invalid element in list: " ++ show e
  in do
    expr <- qexp
    case expr of
        THS.ListE items -> do
            let duplicates = duplicatesOf . map getId $ items
            unless (null duplicates) $
                fail $ "Duplicate Topic IDs found: " ++ show duplicates
            qexp
        _ -> fail "Expected a list of items"

staticAssert :: Bool -> String -> THS.Q [a]
staticAssert condition message = do
    unless condition $ fail ("Compile time assertion failed: " ++ message)
    return []
