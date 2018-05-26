module Structures (
  Hand(..)
, Bidding
, currentBidder
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
    toLatex (Hand s h d c) =
       "\\hand{" ++
       (join "}{" $ map (replace "T" "10" . replace " " "\\,") [s, h, d, c])
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

currentBidder :: Bidding -> T.Direction
currentBidder (Bidding d _) = d

startBidding :: T.Direction -> Bidding
startBidding T.West  = Bidding T.West  []
startBidding T.North = Bidding T.North [[Nothing]]
startBidding T.East  = Bidding T.East  [[Nothing, Nothing]]
startBidding T.South = Bidding T.South [[Nothing, Nothing, Nothing]]

(>-) :: Bidding -> T.Call -> Bidding
(Bidding T.West  (  bs)) >- c = Bidding T.North ( [Just c]   :bs)
(Bidding T.North (b:bs)) >- c = Bidding T.East  (((Just c):b):bs)
(Bidding T.East  (b:bs)) >- c = Bidding T.South (((Just c):b):bs)
(Bidding T.South (b:bs)) >- c = Bidding T.West  (((Just c):b):bs)
