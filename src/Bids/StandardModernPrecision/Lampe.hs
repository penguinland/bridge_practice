module Bids.StandardModernPrecision.Lampe(
    b1D  -- Re-exported from BasicBids
  , b1D2C
  , b1D2C2D
  , b1D2C2H
  , b1D2C2S
  , b1D2C2N
  , b1D2C3C
  , b1D2C3D
  , b1D2C3H
  , b1D2C3S
  , b1D2D
) where


import Auction(pointRange, suitLength, minSuitLength, maxSuitLength, Action,
               makeCall, makeAlertableCall, balancedHand, alternatives,
               forbid, longerThan)
import Bids.StandardModernPrecision.BasicBids(b1D)
import Output(Punct(..), (.+))
import qualified Terminology as T


-- start with the shape-showing helpers

shapeShower2S :: Action
shapeShower2S = do
    alternatives [ suitLength T.Clubs 5 >> suitLength T.Diamonds 3
                 , suitLength T.Clubs 3 >> suitLength T.Diamonds 5 ]
    makeAlertableCall (T.Bid 2 T.Spades) "5-3 in the minors, either way"


shapeShower2N :: Action
shapeShower2N = do
    maxSuitLength T.Clubs 1
    makeAlertableCall (T.Bid 2 T.Notrump)
        "short clubs, likely either 5 or a bad 6 diamonds, or 4441 shape"


shapeShower3C :: Action
shapeShower3C = do
    mapM_ (`minSuitLength` 4) T.minorSuits
    makeAlertableCall (T.Bid 3 T.Clubs) "at least 4-4 in the minors"


shapeShower3D :: Action
shapeShower3D = do
    minSuitLength T.Diamonds 6
    makeCall $ T.Bid 3 T.Diamonds


shapeShower3H :: Action
shapeShower3H = do
    minSuitLength T.Diamonds 6
    minSuitLength T.Hearts 5
    T.Diamonds `longerThan` T.Hearts
    makeCall $ T.Bid 3 T.Hearts


shapeShower3S :: Action
shapeShower3S = do
    minSuitLength T.Diamonds 6
    minSuitLength T.Spades 5
    T.Diamonds `longerThan` T.Spades
    makeCall $ T.Bid 3 T.Spades


-- Now start the exportable bids


b1D2C :: Action
b1D2C = do
    pointRange 11 40
    forbid balancedHand
    -- Your minor must be your longest suit.
    alternatives . flip map T.minorSuits $ (\suit -> do
        minSuitLength suit 5
        suit `longerThan` T.Hearts
        suit `longerThan` T.Spades
        )
    makeAlertableCall (T.Bid 2 T.Clubs) "invitational or better with a minor"


b1D2C2D :: Action
b1D2C2D = do
    pointRange 11 13
    forbid balancedHand
    makeAlertableCall (T.Bid 2 T.Diamonds)
        ("unbalanced 11" .+ NDash .+ "13 HCP")


b1D2C2H :: Action
b1D2C2H = do
    pointRange 11 13
    balancedHand
    makeAlertableCall (T.Bid 2 T.Hearts)
        ("balanced 11" .+ NDash .+ "13 HCP")


b1D2C2S :: Action
b1D2C2S = do
    pointRange 14 15
    shapeShower2S

b1D2C2N :: Action
b1D2C2N = do
    pointRange 14 15
    shapeShower2N

b1D2C3C :: Action
b1D2C3C = do
    pointRange 14 15
    shapeShower3C

b1D2C3D :: Action
b1D2C3D = do
    pointRange 14 15
    shapeShower3D

b1D2C3H :: Action
b1D2C3H = do
    pointRange 14 15
    shapeShower3H

b1D2C3S :: Action
b1D2C3S = do
    pointRange 14 15
    shapeShower3S


b1D2D :: Action
b1D2D = do
    pointRange 8 13
    minSuitLength T.Spades 5
    minSuitLength T.Hearts 4
    makeAlertableCall (T.Bid 2 T.Diamonds)
        "at most invitational, with 5+ spades and 4+ hearts"
