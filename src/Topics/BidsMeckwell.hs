module Topics.BidsMeckwell(
    b1N  -- Re-exported from StandardOpenings
  , b1NoX
  , b1No2C
  , b1No2D
  , b1No2H
  , b1No2S
  , b1No2N
) where


import Auction(pointRange, minSuitLength, maxSuitLength, Action, makeCall,
               alternatives, soundHolding, makeAlertableCall)
import StandardOpenings(b1N)
import qualified Terminology as T


-- What's the right minimum strength to bid Meckwell? It kinda depends on the
-- vulnerability and where in the hand this strength is located. Let's guess 10
-- is a pretty decent minimum, but I'm open to changing it later.
pointsToCompete :: Action
pointsToCompete = pointRange 10 40


singleSuit :: T.Suit -> Action
singleSuit suit = do
    minSuitLength suit 6
    soundHolding suit
    mapM_ (`maxSuitLength` 3) . filter (/= suit) $ T.allSuits


twoSuited :: T.Suit -> T.Suit -> Action
twoSuited a b = do
    minSuitLength a 4
    minSuitLength b 4
    alternatives [minSuitLength a 5, minSuitLength b 5]


b1NoX :: Action
b1NoX = do
    pointsToCompete
    alternatives [ singleSuit T.Clubs
                 , singleSuit T.Diamonds
                 , twoSuited T.Hearts T.Spades
                 ]
    makeAlertableCall T.Double "one long minor, or both majors"


minorAndMajor :: T.Suit -> Action
minorAndMajor minor = do
    pointsToCompete
    minSuitLength minor 4
    alternatives . map (`minSuitLength` 4) $ T.majorSuits
    alternatives . map (`minSuitLength` 5) $ minor : T.majorSuits
    makeAlertableCall (T.Bid 2 minor) (show minor .+ " and a major")

b1No2C :: Action
b1No2C = minorAndMajor T.Clubs

b1No2D :: Action
b1No2D = minorAndMajor T.Diamonds


b1No2H :: Action
b1No2H = do
    pointsToCompete
    singleSuit T.Hearts
    makeCall $ T.Bid 2 T.Hearts


b1No2S :: Action
b1No2S = do
    pointsToCompete
    singleSuit T.Spades
    makeCall $ T.Bid 2 T.Spades


b1No2N :: Action
b1No2N = do
    pointsToCompete
    twoSuited T.Clubs T.Diamonds
    makeAlertableCall (T.Bid 2 T.Notrump) "both minors"
