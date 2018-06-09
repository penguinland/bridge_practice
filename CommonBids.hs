module CommonBids(
  strong1NT
, weak1NT
--, firstSeatOpener
) where

import Auction(Action, pointRange, balancedHand, makeCall)
import DealerProg(addDefn, addReq)
import qualified Terminology as T

strong1NT :: Action
strong1NT = do
    balancedHand
    pointRange 15 17
    makeCall (T.Bid 1 T.Notrump)


weak1NT :: Action
weak1NT = do
    balancedHand
    pointRange 12 14
    makeCall (T.Bid 1 T.Notrump)


{-
get2LongestSuits :: Action


firstSeatOpener :: Action
firstSeatOpener = do
    get2LongestSuits
-}
