module Bids.DONT(
    b1N  -- Re-exported from StandardOpenings
  , b1NoX
  , b1NoX2C
  , b1No2C
  , b1No2C2D
  , b1No2D
  , b1No2D2H
  , b1No2H
  , b1No2S
  , b1No3C
  , b1No3D
  , b1No3H
  , b1No3S
) where


import Action(Action)
import Bids.StandardOpenings(b1N)
import Bids.NaturalOneNotrumpDefense(singleSuited, twoSuited)
import EDSL(pointRange, alternatives, makeCall, makeAlertableCall, forEach,
            longerThan, minSuitLength, maxSuitLength, soundHolding, forbid,
            forbidAll)
import qualified Terminology as T



-- What's the right minimum strength to bid DONT? It kinda depends on the
-- vulnerability and where in the hand this strength is located. Let's guess 10
-- is a pretty decent minimum, but I'm open to changing it later.
minPointsToCompete :: Int
minPointsToCompete = 10

pointsToCompete :: Action
pointsToCompete = pointRange minPointsToCompete 40


b1NoX :: Action
b1NoX = do
    shouldntPreempt
    pointsToCompete
    -- If you're single-suited with spades, bid 2S with a minimum, and
    -- double-then-bid with extras.
    forbid b1No2S
    alternatives $ map singleSuited T.allSuits
    makeAlertableCall T.Double "single-suited hand"


b1NoX2C :: Action
b1NoX2C = makeAlertableCall (T.Bid 2 T.Clubs) "pass or correct"


b1No2C :: Action
b1No2C = do
    pointsToCompete
    alternatives $ map (twoSuited T.Clubs) [T.Diamonds, T.Hearts, T.Spades]
    makeAlertableCall (T.Bid 2 T.Clubs) "clubs and another suit"


b1No2C2D :: Action
b1No2C2D = do
    -- Never mind our strength: we'd prefer partner's other suit.
    -- TODO: might you ever bid this if, say, 2 suits are longer than clubs and
    -- the third is equal length? I'd relay with 5422, and probably even 4333.
    -- We're practicing the obvious situations, and the less-obvious ones won't
    -- come up.
    forEach [T.Diamonds, T.Hearts, T.Spades] (`longerThan` T.Clubs)
    makeAlertableCall (T.Bid 2 T.Diamonds) "pass or correct"


b1No2D :: Action
b1No2D = do
    pointsToCompete
    alternatives $ map (twoSuited T.Diamonds) T.majorSuits
    makeAlertableCall (T.Bid 2 T.Diamonds) "diamonds and a major"


b1No2D2H :: Action
b1No2D2H = do
    -- Like bidding 2D over2C, we're going to practice the obvious situations,
    -- and avoid the more ambiguous ones where the suits are equal length.
    alternatives [ forEach T.majorSuits (`longerThan` T.Diamonds)
    -- Even if you've got tolerance for diamonds, prefer playing in a major!
                 , forEach T.majorSuits (`minSuitLength` 4)
                 ]
    makeAlertableCall (T.Bid 2 T.Hearts) "pass or correct"


b1No2H :: Action
b1No2H = do
    pointsToCompete
    twoSuited T.Hearts T.Spades
    makeAlertableCall (T.Bid 2 T.Hearts) "both majors"


b1No2S :: Action
b1No2S = do
    pointsToCompete
    shouldntPreempt
    -- If you've got extra strength, double and then bid 2S afterward.
    pointRange 0 (minPointsToCompete + 2)
    singleSuited T.Spades
    makeCall $ T.Bid 2 T.Spades


preempt_ :: T.Suit -> Action
preempt_ suit = do
    pointsToCompete
    minSuitLength suit 7
    soundHolding suit  -- Don't do this with a bad suit, even if you're nonvul.
    -- Some people are reluctant to pre-empt if they've got a side 4-card major.
    forEach (filter (/= suit) T.majorSuits) (`maxSuitLength` 3)
    makeCall $ T.Bid 3 suit

b1No3C :: Action
b1No3C = preempt_ T.Clubs

b1No3D :: Action
b1No3D = preempt_ T.Diamonds

b1No3H :: Action
b1No3H = preempt_ T.Hearts

b1No3S :: Action
b1No3S = preempt_ T.Spades

shouldntPreempt :: Action
shouldntPreempt = forbidAll[b1No3C, b1No3D, b1No3H, b1No3S]
