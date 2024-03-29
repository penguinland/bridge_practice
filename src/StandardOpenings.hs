module StandardOpenings(
  b1c
, b1d
, b1h
, b1s
, b1n
, b2n
, b2c
) where

-- NOTE: none of these include point ranges, because they vary depending on
-- which seat you're in. These Actions are solely based on distribution, and you
-- must specify strength yourself before using them.

import Auction(Action, suitLength, minSuitLength, maxSuitLength, makeCall,
               forbid, longerThan, pointRange, balancedHand, alternatives)
import qualified CommonBids as B
import qualified Terminology as T


b1n :: Action
b1n = B.strong1NT


b2n :: Action
b2n = do
    balancedHand
    pointRange 20 21
    makeCall (T.Bid 2 T.Notrump)


b2c :: Action
b2c = do
    pointRange 22 40
    makeCall (T.Bid 2 T.Clubs)


-- Helper function, not exported
notTooStrong :: Action
notTooStrong = do
    forbid b1n
    forbid b2n
    forbid b2c


-- Helper function, not exported
reverseStrength :: Action
reverseStrength = pointRange 17 40

-- Helper function, not exported
reverseInMajors :: Action
reverseInMajors = do
    minSuitLength T.Hearts 5
    minSuitLength T.Spades 5
    reverseStrength

b1s :: Action
b1s = do
    notTooStrong
    minSuitLength T.Spades 5
    -- If you're equal length in the majors and have enough points to reverse,
    -- open 1H instead.
    forbid reverseInMajors
    makeCall (T.Bid 1 T.Spades)


b1h :: Action
b1h = do
    notTooStrong
    minSuitLength T.Hearts 5
    alternatives [reverseInMajors, maxSuitLength T.Spades 4]
    makeCall (T.Bid 1 T.Hearts)


-- Helper function, not exported
noMajor :: Action
noMajor = do
    maxSuitLength T.Spades 4
    maxSuitLength T.Hearts 4


-- Helper function, not exported
equalMinors :: Int -> Action
equalMinors len = do
    suitLength T.Clubs    len
    suitLength T.Diamonds len


-- Helper function, not exported
-- To bid both minors, we need at least 9 cards in them.
bothMinors :: Action
bothMinors = alternatives .
             map (\(a, b) -> do minSuitLength T.Clubs    a
                                minSuitLength T.Diamonds b) $
             [(4, 5), (5, 4)]


b1d :: Action
b1d = let
  in do
    notTooStrong
    noMajor
    minSuitLength T.Diamonds 3
    alternatives [ bothMinors >> forbid reverseStrength
                 , forbid bothMinors >> T.Diamonds `longerThan` T.Clubs ]
    makeCall (T.Bid 1 T.Diamonds)


b1c :: Action
b1c = do
    notTooStrong
    noMajor
    minSuitLength T.Clubs 3
    alternatives [ reverseStrength >> bothMinors
                 , forbid bothMinors >> T.Clubs `longerThan` T.Diamonds
                 , equalMinors 3
                 , equalMinors 4
                 ]
    makeCall (T.Bid 1 T.Clubs)
