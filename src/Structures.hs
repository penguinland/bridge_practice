module Structures (
  Hand(..)
, Bidding
, currentBidder
, startBidding
, (>-)
, lastCall
, Deal(..)
) where

import Data.Char(toUpper)
import Data.List.Utils(join, replace)
import Data.Maybe(fromMaybe)
import Data.Semigroup(First(..), getFirst)

import Output(Showable, toLatex)
import qualified Terminology as T


--               spades hearts diams. clubs
data Hand = Hand String String String String

instance Showable Hand where
    toLatex (Hand s h d c) =
        "\\hand{" ++
        join "}{" (map (replace "-" "--" .
                        replace "T" "10" .
                        replace " " "\\,") [s, h, d, c])
        ++ "}"


-- The direction is the next bidder
data Bidding = Bidding T.Direction [[Maybe T.CompleteCall]]

instance Showable Bidding where
    toLatex (Bidding r b) =
         "  \\begin{bidding}\n    " ++ rows ++ finish r ++
         "??\n  \\end{bidding}"
      where
        newRow = "\\\\\n    "  -- backslash, backslash, newline
        rows = join newRow . reverse . map formatRow $ b
        formatRow = join "&" .
                    zipWith formatMaybeBid (cycle ["oppsalert", "ouralert"]) .
                    reverse
        formatMaybeBid alertMacro = maybe "" $ formatBid alertMacro
        formatBid _          (T.CompleteCall c  Nothing) = toLatex c
        formatBid alertMacro (T.CompleteCall c (Just a)) =
            toLatex c ++ "\\" ++ alertMacro ++ "{" ++ toLatex a ++ "}"
        finish T.North = newRow
        finish _       = "&"


currentBidder :: Bidding -> T.Direction
currentBidder (Bidding d _) = d


startBidding :: T.Direction -> Bidding
startBidding T.West  = Bidding T.West  []
startBidding T.North = Bidding T.North [[Nothing]]
startBidding T.East  = Bidding T.East  [[Nothing, Nothing]]
startBidding T.South = Bidding T.South [[Nothing, Nothing, Nothing]]


(>-) :: Bidding -> T.CompleteCall -> Bidding
(Bidding T.West    bs ) >- c = Bidding T.North    ([Just c]  :bs)
(Bidding d      (b:bs)) >- c = Bidding (T.next d) ((Just c:b):bs)
_                       >- _ = error "Missing bidding for current direction"


lastCall :: Bidding -> T.CompleteCall
lastCall (Bidding _ calls) =
    -- Surely there's a better way to do this, but I couldn't figure it out.
    -- concat flattens the bidding to one long list (most recent bids first!).
    -- We then map the calls to First of these values, and use sequence to turn
    -- a (First Maybe CompleteCall) into a (Maybe First CompleteCall). Then
    -- mconcat bunches them all into a single (Maybe First CompleteCall), which
    -- we unwrap.
    getFirst . fromMaybe (error "Unable to get last call from empty bidding") .
        mconcat . map (sequence . First) . concat $ calls


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
