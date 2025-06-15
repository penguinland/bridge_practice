module Bids.StandardOpenings(
  b1C
, b1D
, b1H
, b1S
, b1N  -- re-exported from CommonBids
, b2N
, b2C
) where

-- NOTE: none of these include point ranges, because they vary depending on
-- which seat you're in. These Actions are solely based on distribution, and you
-- must specify strength yourself before using them.

import Action(Action)
import CommonBids(strong1NT)
import EDSL(suitLength, minSuitLength, maxSuitLength, makeCall, impliesThat,
            longerThan, pointRange, balancedHand, alternatives, forbidAll,
            equalLength, atLeastAsLong, forEach, nameAction)
import qualified Terminology as T


b1N :: Action
b1N = strong1NT


b2N :: Action
b2N = nameAction "sayc_b2N" $ do
    balancedHand
    pointRange 20 21
    makeCall (T.Bid 2 T.Notrump)


b2C :: Action
b2C = nameAction "sayc_b2C" $ do
    pointRange 22 40
    makeCall (T.Bid 2 T.Clubs)


b1S :: Action
b1S = nameAction "sayc_b1S" $ do
    forbidAll [b1N, b2N, b2C]
    minSuitLength T.Spades 5
    T.Spades `atLeastAsLong` T.Hearts
    -- If you're equal length in the majors and have enough points to reverse,
    -- open 1H instead.
    (T.Hearts `equalLength` T.Spades) `impliesThat` pointRange 0 16
    -- If you have 5 spades and a 6-card minor, it's unclear what to do. Avoid
    -- those entirely.
    forEach T.minorSuits (`maxSuitLength` 5)
    makeCall (T.Bid 1 T.Spades)


b1H :: Action
b1H = nameAction "sayc_b1H" $ do
    forbidAll [b1N, b2N, b2C]
    minSuitLength T.Hearts 5
    T.Hearts `atLeastAsLong` T.Spades
    -- NOTE: If you've got 6 hearts, 5 spades, and are not strong enough to
    -- reverse, I'd probably bid the spades first even though the hearts are
    -- longer: pretend your hand is 5-5.
    minSuitLength T.Spades 5 `impliesThat` pointRange 17 40
    -- If you've got 5 hearts and 6 diamonds, probably bid the hearts. If you've
    -- got 5 hearts and 6 clubs, it's not as clear-cut. Avoid that for now.
    maxSuitLength T.Clubs 6
    makeCall (T.Bid 1 T.Hearts)


b1D :: Action
b1D = nameAction "sayc_b1D" $ do
    forbidAll [b1N, b2N, b2C]
    forEach T.majorSuits (`maxSuitLength` 4)
    minSuitLength T.Diamonds 3
    alternatives [ T.Diamonds `longerThan` T.Clubs
                 , minSuitLength T.Diamonds 5 -- From the top with 5-5
                 , do suitLength T.Clubs 5  -- Both minors, too weak to reverse
                      suitLength T.Diamonds 4
                      pointRange 0 16
                 ]
    makeCall (T.Bid 1 T.Diamonds)


b1C :: Action
b1C = nameAction "sayc_b1C" $ do
    forbidAll [b1N, b2N, b2C]
    forEach T.majorSuits (`maxSuitLength` 4)
    minSuitLength T.Clubs 3
    alternatives [ T.Clubs `longerThan` T.Diamonds
                   -- Up the line with 3's and 4's
                 , forEach T.minorSuits (`suitLength` 3)
                 , forEach T.minorSuits (`suitLength` 4)
                 ]
    makeCall (T.Bid 1 T.Clubs)
