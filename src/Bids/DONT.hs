module Bids.DONT(
    b1N  -- Re-exported from Meckwell
  , b1NoX
  , b1NoX2C
  , b1No2C
  , b1No2C2D
  , b1No2D
  , b1No2D2H
  , b1No2H
  , b1No2S
) where


import Action(Action)
import Bids.Meckwell(singleSuit, twoSuited, b1N)
import EDSL(pointRange, minSuitLength, maxSuitLength, alternatives, forbid,
            makeCall, makeAlertableCall, forEach)
import qualified Terminology as T



-- What's the right minimum strength to bid DONT? It kinda depends on the
-- vulnerability and where in the hand this strength is located. Let's guess 10
-- is a pretty decent minimum, but I'm open to changing it later.
pointsToCompete :: Action
pointsToCompete = pointRange 10 40


b1NoX :: Action
b1NoX = do
    pointsToCompete
    -- If you're single-suited with spades, when would you bid 2S and when X?
    -- Avoid the choice for now.
    alternatives $ map singleSuit [T.Clubs, T.Diamonds, T.Hearts]
    makeAlertableCall T.Double "single-suited hand"


b1NoX2C :: Action
b1NoX2C = makeAlertableCall (T.Bid 2 T.Clubs) "pass or correct"


b1No2C :: Action
b1No2C = do
    pointsToCompete
    alternatives . map (twoSuited T.Clubs) $ [T.Diamonds, T.Hearts, T.Spades]
    makeAlertableCall (T.Bid 2 T.Clubs) "clubs and another suit"


b1No2C2D :: Action
b1No2C2D = do
    -- Never mind our strength: we'd prefer partner's other suit.
    forEach [T.Diamonds, T.Hearts, T.Spades] (`atLeastAsLong` T.Clubs)
    makeAlertableCall (T.Bid 2 T.Diamonds) "pass or correct"


b1No2D :: Action
b1No2D = do
    pointsToCompete
    alternatives . map (twoSuited T.Diamonds) T.majorSuits
    makeAlertableCall (T.Bid 2 T.Diamonds) "diamonds and a major"


b1No2D2H :: Action
b1No2D2H = do
    forEach T.majorSuits (`atLeastAsLong` T.Diamonds)
    makeAlertableCall (T.Bid 2 T.Hearts) "pass or correct"


b1No2H :: Action
b1No2H = do
    pointsToCompete
    twoSuited T.Hearts T.Spades
    makeAlertableCall (T.Bid 2 T.Hearts) "both majors"


b1No2S :: Action
b1No2S = do
    pointsToCompete
    singleSuited T.Spades
    makeCall $ T.Bid 2 T.Spades
