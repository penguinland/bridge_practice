module Structures (
  Hand(..)
, Bidding
, startBidding
, (>-)
) where

import Control.Monad(liftM)
import Data.List.Utils(join, replace)
import Data.Maybe(fromMaybe)
import System.Process(readProcess)

import Output(Showable, toLatex)
import qualified Terminology as T


--               spades hearts diams. clubs
data Hand = Hand String String String String

instance Showable Hand where
  toLatex (Hand s h d c) = "\\hand{" ++
                           (join "}{" $ map (replace " " "\\,") [s, h, d, c])
                           ++ "}"


data Bidding = Bidding T.Direction [[Maybe T.Call]]

instance Showable Bidding where
    toLatex (Bidding c b) =
         "  \\begin{bidding}\n    " ++ rows ++ finish c ++
         "??\n  \\end{bidding}"
      where
        newRow = "\\\\\n    "
        rows = join newRow . reverse . map formatRow $ b
        formatRow = join "&" . reverse . map (fromMaybe "" . liftM toLatex)
        finish T.North = newRow
        finish _       = "&"

startBidding :: T.Direction -> Bidding
startBidding T.North = Bidding T.North []
startBidding T.East  = Bidding T.East  [[Nothing]]
startBidding T.South = Bidding T.South [[Nothing, Nothing]]
startBidding T.West  = Bidding T.West  [[Nothing, Nothing, Nothing]]

(>-) :: Bidding -> T.Call -> Bidding
(Bidding T.North (  bs)) >- c = Bidding T.East  ( [Just c]   :bs)
(Bidding T.East  (b:bs)) >- c = Bidding T.South (((Just c):b):bs)
(Bidding T.South (b:bs)) >- c = Bidding T.West  (((Just c):b):bs)
(Bidding T.West  (b:bs)) >- c = Bidding T.North (((Just c):b):bs)
