module Bids.StandardModernPrecision.Lampe(
    b1D  -- Re-exported from BasicBids
  , b1D2C
  , b1D2D
) where


import Auction(pointRange, minSuitLength, Action, makeAlertableCall, forbid,
               balancedHand, alternatives, longerThan)
import Bids.StandardModernPrecision.BasicBids(b1D)
import qualified Terminology as T


b1D2C :: Action
b1D2C = do
    pointRange 11 40
    forbid balancedHand
    -- Your minor must be your longest suit.
    alternatives . flip map T.minorSuits $ (\suit -> do
        minSuitLength suit 5
        suit `longerThan` T.Hearts
        suit `longerThan` T.Spades
        )
    makeAlertableCall (T.Bid 2 T.Clubs) "invitational or better with a minor"


b1D2D :: Action
b1D2D = do
    pointRange 8 13
    minSuitLength T.Spades 5
    minSuitLength T.Hearts 4
    makeAlertableCall (T.Bid 2 T.Diamonds)
        "at most invitational, with 5+ spades and 4+ hearts"
