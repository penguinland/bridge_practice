-- We use Template Haskell to check at compile time whether every Topic ID is unique. This function
-- must be compiled before SupportedTopics.hs, which is why it's a separate file.
{-# LANGUAGE TemplateHaskell #-}

module CompileTime(compileTimeAssertUniqueTopicIDs) where

import Control.Arrow((&&&))
import Control.Monad(unless)
import Data.List(sort)
import qualified Language.Haskell.TH.Syntax as THS


duplicatesOf_ :: [Int] -> [Int]
duplicatesOf_ = map fst . filter (uncurry (==)) . uncurry zip . (init &&& tail) . sort

compileTimeAssertUniqueTopicIDs :: THS.Q THS.Exp -> THS.Q THS.Exp
compileTimeAssertUniqueTopicIDs qexp = let
    getId :: THS.Exp -> Int
    getId (THS.TupE [Just (THS.LitE (THS.IntegerL i)), _, _]) = fromInteger i
    getId e = error $ "Invalid element in list: " ++ show e
  in do
    expr <- qexp
    case expr of
        THS.ListE items -> do
            let duplicates = duplicatesOf_ . map getId $ items
            unless (null duplicates) $
                fail $ "Duplicate Topic IDs found: " ++ show duplicates
            qexp
        _ -> fail "Expected a list of items"


-- TODO: use `unlines` instead of `join "\n"`
