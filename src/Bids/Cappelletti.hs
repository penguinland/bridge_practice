module Bids.Cappelletti(
    b1N
  , b1NoX
  , b1No2C
  , b1No2C2D
  , b1No2D
  , b1No2H
  , b1No2H2N
  , b1No2S
  , b1No2S2N
  , b1No2N
) where


import Action(Action)
import Bids.Meckwell(singleSuit, twoSuited)
import EDSL(pointRange, minSuitLength, maxSuitLength, alternatives, forbid,
            makeCall, makeAlertableCall, balancedHand)
import Output ((.+), Punct(..))
import qualified Terminology as T


b1N :: Action
b1N = do
    balancedHand
    pointRange 12 14
    makeAlertableCall (T.Bid 1 T.Notrump) ("12" .+ NDash .+ "14")


-- What's the right minimum strength to bid Meckwell? It kinda depends on the
-- vulnerability and where in the hand this strength is located. Let's guess 10
-- is a pretty decent minimum, but I'm open to changing it later.
pointsToCompete :: Action
pointsToCompete = pointRange 10 40


b1NoX :: Action
b1NoX = do
    pointRange 16 40
    makeCall T.Double


-- TODO: If you've got the strength for a penalty double but you also have the
-- shape for one of the other bids, which should you do? Perhaps it depends on
-- the vulnerability.
b1No2C :: Action
b1No2C = do
    pointsToCompete
    forbid b1NoX
    alternatives . map singleSuit $ T.allSuits
    makeAlertableCall (T.Bid 2 T.Clubs) "single-suited hand"


b1No2C2D :: Action
b1No2C2D = makeAlertableCall (T.Bid 2 T.Diamonds) "what is your suit?"


b1No2D :: Action
b1No2D = do
    pointsToCompete
    forbid b1NoX
    twoSuited T.Hearts T.Spades
    makeAlertableCall (T.Bid 2 T.Diamonds) "both majors"


b1No2H :: Action
b1No2H = do
    pointsToCompete
    forbid b1NoX
    alternatives . map (twoSuited T.Hearts) $ T.minorSuits
    makeAlertableCall (T.Bid 2 T.Hearts) "hearts and a minor"


b1No2S :: Action
b1No2S = do
    pointsToCompete
    forbid b1NoX
    alternatives . map (twoSuited T.Spades) $ T.minorSuits
    makeAlertableCall (T.Bid 2 T.Hearts) "spades and a minor"


b1No2N :: Action
b1No2N = do
    pointsToCompete
    forbid b1NoX
    twoSuited T.Clubs T.Diamonds
    makeAlertableCall (T.Bid 2 T.Notrump) "both minors"


b1No2H2N :: Action
b1No2H2N = do
    maxSuitLength T.Hearts 2
    minSuitLength T.Clubs 3
    minSuitLength T.Diamonds 3
    makeAlertableCall (T.Bid 2 T.Notrump) "what is your minor?"


b1No2S2N :: Action
b1No2S2N = do
    maxSuitLength T.Spades 2
    minSuitLength T.Clubs 3
    minSuitLength T.Diamonds 3
    makeAlertableCall (T.Bid 2 T.Notrump) "what is your minor?"
