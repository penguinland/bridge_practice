module Structures (
  Hand(..)
, Bidding
, currentBidder
, startBidding
, (>-)
, Deal(..)
) where

import Data.Char(toUpper)
import Data.List.Utils(join, replace)

import Output(Showable, toLatex)
import qualified Terminology as T


--               spades hearts diams. clubs
data Hand = Hand String String String String

instance Showable Hand where
    toLatex (Hand s h d c) =
        "\\hand{" ++
        join "}{" (map (replace "T" "10" . replace " " "\\,") [s, h, d, c])
        ++ "}"


-- The direction is the next bidder
data Bidding = Bidding T.Direction [[Maybe T.CompleteCall]]

instance Showable Bidding where
    toLatex (Bidding c b) =
         "  \\begin{bidding}\n    " ++ rows ++ finish c ++
         "??\n  \\end{bidding}"
      where
        newRow = "\\\\\n    "
        rows = join newRow . reverse . map formatRow $ b
        formatRow = join "&" . reverse . map (maybe "" toLatex)
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
(Bidding T.West    bs ) >- c = Bidding T.North    ([Just c]  :bs)
(Bidding d      (b:bs)) >- c = Bidding (T.next d) ((Just c:b):bs)
_                       >- _ = error "Missing bidding for current direction"


--                                           N    E    S    W
data Deal = Deal T.Direction T.Vulnerability Hand Hand Hand Hand

instance Showable Deal where
    toLatex (Deal d v n e s w) =
        "  \\deal{" ++ capitalize (show d) ++ "}{" ++
           join "}%\n    {" (toLatex v : map toLatex [n, e, s, w]) ++
           "%\n  }"
      where
        capitalize (h:t) = toUpper h : t
        capitalize _     = error "Attempt to capitalize empty direction!?"
