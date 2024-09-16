module Bids.Lebensohl(
    b1N  -- re-exported from StandardOpenings
  , b1NoX2C
  , b1NoX2D
  , b1NoX2H
  , b1No2CX
  , b1No2C2D
  , b1No2C2H
  , b1No2D2H
  , b1No2D2S
  , b1No2H2S
) where


import Action(Action, withholdBid)
import qualified Bids.OneNotrump as NT
import EDSL(minSuitLength, makeCall, longerThan, pointRange)
import qualified Terminology as T


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
b1No2CX = do
    withholdBid NT.b1N2C
    makeCall T.Double

b1No2C2D :: Action
b1No2C2D = NT.b1N2D

b1No2C2H :: Action
b1No2C2H = NT.b1N2H

-- Signoffs
b1No2D2H :: Action
b1No2D2H = do
    NT.lessThanInvitational
    pointRange 5 40
    minSuitLength T.Hearts 5
    -- If you're 5-5 in the majors, pick the better suit. I'm too lazy to add
    -- something to the EDSL to figure out which suit is better, so avoid it.
    T.Hearts `longerThan` T.Clubs
    T.Hearts `longerThan` T.Diamonds
    T.Hearts `longerThan` T.Spades
    makeCall $ T.Bid 2 T.Hearts


b1No2D2S :: Action
b1No2D2S = do
    NT.lessThanInvitational
    pointRange 5 40
    minSuitLength T.Spades 5
    T.Spades `longerThan` T.Clubs
    T.Spades `longerThan` T.Diamonds
    T.Spades `longerThan` T.Hearts
    makeCall $ T.Bid 2 T.Spades


b1No2H2S :: Action
b1No2H2S = b1No2D2S
