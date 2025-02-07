module Bids.MultiLandy(
    b1N  -- Re-exported from StandardOpenings
  , b1NoX
  , b1No2C
  , b1No2D
  , b1No2H
  , b1No2S
  , b1No2N
) where


import Action(Action)
import Bids.NaturalOneNotrumpDefense(singleSuited, twoSuited)
import Bids.StandardOpenings(b1N)
import EDSL(minSuitLength, maxSuitLength, makeCall, makeAlertableCall,
            alternatives, forEach)
import Output ((.+))
import qualified Terminology as T


b1NoX :: Action
b1NoX = do
    pointRange 17 40
    -- You don't have a suit you really want to bid
    mapM_ (`maxSuitLength` 4) T.majorSuits
    mapM_ (`maxSuitLength` 5) T.minorSuits
    makeAlertableCall T.Double "penalty-oriented"


b1No2C :: Action
b1No2C = do
    twoSuited T.Hearts T.Spades
    makeAlertableCall (T.Bid 2 T.Clubs) "both majors"


b1No2D :: Action
b1No2D = do
    alternatives [oneSuited T.Hearts, oneSuited T.Spades]
    makeAlertableCall (T.Bid 2 T.Clubs) "one long major"


minorAndMajor :: T.Suit -> Action
minorAndMajor major = do
    alternatives [twoSuited minor T.Clubs, twoSuited minor T.Diamonds]
    makeAlertableCall (T.Bid 2 major) (show major .+ " and a minor")

b1No2H :: Action
b1No2H = minorAndMajor T.Hearts

b1No2S :: Action
b1No2S = minorAndMajor T.Spades


b1No2N :: Action
b1No2N = do
    twoSuited T.Clubs T.Diamonds
    makeAlertableCall (T.Bid 2 T.Notrump) "both minors"
