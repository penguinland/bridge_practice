module Bids.Meckwell(
    b1N  -- Re-exported from StandardOpenings
  , b1NoX
  , b1NoX2C
  , b1NoX2C2H
  , b1No2C
  , b1No2C2H
  , b1No2D
  , b1No2D2H
  , b1No2H
  , b1No2S
  , b1No2N
) where


import Action(Action)
import Bids.NaturalOneNotrumpDefense(singleSuited, twoSuited)
import Bids.StandardOpenings(b1N)
import EDSL(pointRange, minSuitLength, maxSuitLength, makeCall, alternatives,
            makeAlertableCall, forEach)
import Output ((.+))
import qualified Terminology as T


b1NoX :: Action
b1NoX = do
    alternatives [ singleSuited T.Clubs
                 , singleSuited T.Diamonds
                 , twoSuited T.Hearts T.Spades
                 ]
    makeAlertableCall T.Double "one long minor, or both majors"


minorAndMajor :: T.Suit -> Action
minorAndMajor minor = do
    alternatives [twoSuited minor T.Hearts, twoSuited minor T.Spades]
    makeAlertableCall (T.Bid 2 minor) (show minor .+ " and a major")

b1No2C :: Action
b1No2C = minorAndMajor T.Clubs

b1No2D :: Action
b1No2D = minorAndMajor T.Diamonds


b1No2H :: Action
b1No2H = do
    singleSuited T.Hearts
    makeCall $ T.Bid 2 T.Hearts

b1No2S :: Action
b1No2S = do
    singleSuited T.Spades
    makeCall $ T.Bid 2 T.Spades


b1No2N :: Action
b1No2N = do
    twoSuited T.Clubs T.Diamonds
    makeAlertableCall (T.Bid 2 T.Notrump) "both minors"


b1NoX2C :: Action
b1NoX2C = do
    -- If you've got a freak hand, you might be tempted to bid your long suit.
    -- So, forbid those here just to make the bid more obvious.
    forEach T.allSuits (`maxSuitLength` 6)
    makeAlertableCall (T.Bid 2 T.Clubs) "pass or correct"


b1NoX2C2H :: Action
b1NoX2C2H = do
    twoSuited T.Hearts T.Spades
    makeAlertableCall (T.Bid 2 T.Hearts) "both majors: pass or correct"


-- For when partner bids a minor and you want to find their major
findMajor :: T.Suit -> Action
findMajor minor = do
    maxSuitLength minor 2
    minSuitLength T.Hearts 3
    minSuitLength T.Spades 3
    forEach T.allSuits (`maxSuitLength` 6)
    makeAlertableCall (T.Bid 2 T.Hearts) "pass or correct"

b1No2C2H :: Action
b1No2C2H = findMajor T.Clubs

b1No2D2H :: Action
b1No2D2H = findMajor T.Diamonds
