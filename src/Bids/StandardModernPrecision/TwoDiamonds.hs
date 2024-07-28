module Bids.StandardModernPrecision.TwoDiamonds(
  b2D  -- Re-exported from BasicBids
, b2D2H
, bP2D2H
, b2D2H2S
) where


import Action(Action)
import Bids.StandardModernPrecision.BasicBids(b2D, lessThanInvitational)
import EDSL(suitLength, minSuitLength, maxSuitLength, makeCall,
            makeAlertableCall, atLeastAsLong)
import qualified Terminology as T


-- unexportedhelper function
heartsSignoff :: Action
heartsSignoff = do
    lessThanInvitational
    minSuitLength T.Hearts 3
    maxSuitLength T.Diamonds 6
    T.Hearts `atLeastAsLong` T.Spades
    maxSuitLength T.Clubs 4

b2D2H :: Action
b2D2H = do
    heartsSignoff
    makeAlertableCall (T.Bid 2 T.Hearts) "nonforcing, likely signoff"

bP2D2H :: Action
bP2D2H = do
    heartsSignoff
    -- When you're a passed hand, bidding a new suit is always nonforcing, so
    -- that's no longer alertable.
    makeCall $ T.Bid 2 T.Hearts


b2D2H2S :: Action
b2D2H2S = do
    suitLength T.Hearts 3
    makeAlertableCall (T.Bid 2 T.Spades) "exactly 4315 shape"
