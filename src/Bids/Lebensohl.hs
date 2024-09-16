module Bids.Lebensohl(
    b1N  -- re-exported from StandardOpenings
  , b1NoX2C
  , b1NoX2D
  , b1NoX2H
  , b1No2CX
  , b1No2C2D
  , b1No2C2H
) where


import Action(Action)
import qualified Bids.OneNotrump as NT
{-
import EDSL(forbid, pointRange, suitLength, minSuitLength, maxSuitLength,
            makeCall, makeAlertableCall, alternatives, balancedHand,
            longerThan, atLeastAsLong, flatHand, loserCount, minLoserCount,
            forbidAll, impliesThat, forEach, hasTopN)
import qualified Terminology as T
-}


b1N :: Action
b1N = NT.b1N


-- Ignore the opponents' interference if it doesn't actually take up any bidding
-- space.
b1NoX2C :: Action
b1NoX2C = NT.b1N2C

b1NoX2D :: Action
b1NoX2D = NT.b1N2D

b1NoX2H :: Action
b1NoX2H = NT.b1N2H

b1No2CX :: Action
b1No2CX = NT.b1N2C

b1No2C2D :: Action
b1No2C2D = NT.b1N2C2D

b1No2C2H :: Action
b1No2C2H = NT.b1N2C2H
