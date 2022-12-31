module Topics.StandardModernPrecision.Bids1D(
    b1D
  , b1D2C
  , b1D2D
  , b1D2H
  , b1D2S
) where

import Auction(forbid, pointRange, suitLength, minSuitLength, maxSuitLength,
               Action, balancedHand, makeCall, makeAlertableCall,
               {-makePass,-} alternatives)
--import CommonBids(cannotPreempt)
import qualified Terminology as T

import Topics.StandardModernPrecision.BasicBids(b1D)


b1D2C :: Action
b1D2C = do
    pointRange 11 40
    -- Either you've got 5+ clubs, or you've got 9+ cards in the minors and are
    -- game forcing (with an invitational hand, start with 2D and rebid 3C).
    alternatives [ minSuitLength T.Clubs 5
                 , minSuitLength T.Clubs 4 >> minSuitLength T.Diamonds 5 >>
                       pointRange 14 40 ]
    forbid balancedHand
    -- If you've got a major, you must have a 6-card minor.
    alternatives [ minSuitLength T.Clubs 6
                 , mapM_ (`maxSuitLength` 3) T.majorSuits ]
    makeCall $ T.Bid 2 T.Clubs


b1D2D :: Action
b1D2D = do
    pointRange 11 40
    -- Either you've got 5+ diamonds, or you've got 9+ cards in the minors and
    -- are invitational (with a game forcing hand, start with 2C).
    alternatives [ minSuitLength T.Diamonds 5
                 , minSuitLength T.Diamonds 4 >> minSuitLength T.Clubs 5 >>
                       pointRange 11 13 ]
    forbid balancedHand
    -- If you've got a major, you must have a 6-card minor.
    alternatives [ minSuitLength T.Diamonds 6
                 , mapM_ (`maxSuitLength` 3) T.majorSuits ]
    makeAlertableCall (T.Bid 2 T.Diamonds) "invitational or better"


b1D2H :: Action
b1D2H = do
    suitLength T.Spades 5
    minSuitLength T.Hearts 4
    maxSuitLength T.Hearts 5
    pointRange 6 10
    makeAlertableCall (T.Bid 2 T.Hearts)
        "Reverse Flannery: 5 spades, 4-5 hearts, 6-10 HCP"


b1D2S :: Action
b1D2S = do
    suitLength T.Spades 5
    minSuitLength T.Hearts 4
    maxSuitLength T.Hearts 5
    pointRange 11 13
    makeAlertableCall (T.Bid 2 T.Spades)
        "Reverse Flannery: 5 spades, 4-5 hearts, invitational strength"


