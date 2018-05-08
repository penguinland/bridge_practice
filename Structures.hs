module Structures (
  Hand(..)
) where

import Data.List.Utils(join, replace)
import System.Process(readProcess)

import Output(Showable, toLatex)
import Terminology


data Hand = Hand String String String String

instance Showable Hand where
  toLatex (Hand s h d c) = "\\hand{" ++
                           (join "}{" $ map (replace " " "\\,") [s, h, d, c])
                           ++ "}"
