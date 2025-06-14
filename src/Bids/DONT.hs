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
  , b1No3C  -- These next four are all re-exported from NaturalOneNotrumpDefense
  , b1No3D
  , b1No3H
  , b1No3S
) where


import Action(Action)
import Bids.StandardOpenings(b1N)
import Bids.NaturalOneNotrumpDefense(
    singleSuited, twoSuited, b1No3C, b1No3D, b1No3H, b1No3S, minPointsToCompete)
import EDSL(pointRange, alternatives, makeCall, makeAlertableCall, forEach,
            longerThan, minSuitLength, forbid, nameAction)
import qualified Terminology as T


b1NoX :: Action
b1NoX = nameAction "dont_b1NoX" $ do
    -- If you're single-suited with spades, bid 2S with a minimum, and
    -- double-then-bid with extras.
    forbid b1No2S
    alternatives $ map singleSuited T.allSuits
    makeAlertableCall T.Double "single-suited hand"


b1NoX2C :: Action
b1NoX2C = nameAction "dont_b1NoX2C" $
    makeAlertableCall (T.Bid 2 T.Clubs) "pass or correct"


b1No2C :: Action
b1No2C = nameAction "dont_b1No2C" $ do
    alternatives $ map (twoSuited T.Clubs) [T.Diamonds, T.Hearts, T.Spades]
    makeAlertableCall (T.Bid 2 T.Clubs) "clubs and another suit"


b1No2C2D :: Action
b1No2C2D = nameAction "dont_b1No2C2D" $ do
    -- Never mind our strength: we'd prefer partner's other suit.
    -- TODO: might you ever bid this if, say, 2 suits are longer than clubs and
    -- the third is equal length? I'd relay with 5422, and probably even 4333.
    -- We're practicing the obvious situations, and the less-obvious ones won't
    -- come up.
    forEach [T.Diamonds, T.Hearts, T.Spades] (`longerThan` T.Clubs)
    makeAlertableCall (T.Bid 2 T.Diamonds) "pass or correct"


b1No2D :: Action
b1No2D = nameAction "dont_b1No2D" $ do
    alternatives $ map (twoSuited T.Diamonds) T.majorSuits
    makeAlertableCall (T.Bid 2 T.Diamonds) "diamonds and a major"


b1No2D2H :: Action
b1No2D2H = nameAction "dont_b1No2D2H" $ do
    -- Like bidding 2D over2C, we're going to practice the obvious situations,
    -- and avoid the more ambiguous ones where the suits are equal length.
    alternatives [ forEach T.majorSuits (`longerThan` T.Diamonds)
    -- Even if you've got tolerance for diamonds, prefer playing in a major!
                 , forEach T.majorSuits (`minSuitLength` 4)
                 ]
    makeAlertableCall (T.Bid 2 T.Hearts) "pass or correct"


b1No2H :: Action
b1No2H = nameAction "dont_b1No2H" $ do
    twoSuited T.Hearts T.Spades
    makeAlertableCall (T.Bid 2 T.Hearts) "both majors"


b1No2S :: Action
b1No2S = nameAction "dont_b1No2S" $ do
    -- If you've got extra strength, double and then bid 2S afterward.
    pointRange 0 (minPointsToCompete + 2)
    singleSuited T.Spades
    makeCall $ T.Bid 2 T.Spades
