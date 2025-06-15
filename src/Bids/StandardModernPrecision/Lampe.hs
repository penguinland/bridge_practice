module Bids.StandardModernPrecision.Lampe(
    b1D      -- Re-exported from OneDiamond
  , b1D1H
  , b1D1H1S
  , b1D1H1S2N
  , b1D1H1S3C
  , b1D1H1S3D
  , b1D1H1N
  , b1D1H1N2N
  , b1D1H1N3C
  , b1D1H1N3D
  , b1D1S
  , b1D1S1N
  , b1D1S1N2N
  , b1D1S1N3C
  , b1D1S1N3D
  , b1D2C
  , b1D2C2D
  , b1D2C2D2H
  , b1D2C2D2H2S
  , b1D2C2D2H2S2N
  , b1D2C2D2H2S2N3C
  , b1D2C2D2H2S2N3D
  , b1D2C2D2H2N
  , b1D2C2D2H3C
  , b1D2C2D2H3D
  , b1D2C2H
  , b1D2C2S
  , b1D2C2S2N
  , b1D2C2S2N3C
  , b1D2C2S2N3D
  , b1D2C2N
  , b1D2C3C
  , b1D2C3D
  , b1D2C3H
  , b1D2C3S
  , b1D2D
) where


import Action(Action)
import Bids.StandardModernPrecision.OneDiamond(b1D)
import EDSL(pointRange, suitLength, minSuitLength, maxSuitLength, makeCall,
            makeAlertableCall, balancedHand, alternatives, forbid, longerThan,
            atMostAsLong, forEach, nameAction)
import Output(Punct(..), (.+))
import qualified Terminology as T


-- Adapt the 1D responses from OneDiamond.hs

b1D1H :: Action
b1D1H = nameAction "lampe_b1D1H" $ do
    pointRange 6 40
    minSuitLength T.Hearts 4
    forbid b1D2D  -- Reverse Flannery
    -- With longer spades, bid those first. With equal-length spades, either
    -- you're 4-4 and you should probably bid the hearts first, or you're 5-5
    -- and either you're going to bid Reverse Flannery (forbidden above), or
    -- you're game forcing and can reverse later, so bid the hearts first.
    T.Spades `atMostAsLong` T.Hearts
    makeCall $ T.Bid 1 T.Hearts


b1D1S :: Action
b1D1S = nameAction "lampe_b1D1S" $ do
    pointRange 6 40
    minSuitLength T.Spades 4
    forbid b1D2D  -- Reverse Flannery
    -- Your spades should be your longest major. If your hearts are at least as
    -- long, start with 1H instead.
    T.Spades `longerThan` T.Hearts
    makeCall $ T.Bid 1 T.Spades


b1D1H1S :: Action
b1D1H1S = nameAction "lampe_b1D1H1S" $ do
    maxSuitLength T.Hearts 3
    minSuitLength T.Spades 4
    makeCall $ T.Bid 1 T.Spades


b1D1H1N :: Action
b1D1H1N = nameAction "lampe_b1D1H1N" $ do
    maxSuitLength T.Hearts 3
    -- Stylistic point: prefer rebidding 1S to 1N. Others might disagree.
    maxSuitLength T.Spades 3
    balancedHand
    pointRange 11 13
    makeCall $ T.Bid 1 T.Notrump


b1D1S1N :: Action
b1D1S1N = nameAction "lampe_b1D1S1N" $ do
    maxSuitLength T.Spades 3
    balancedHand
    pointRange 11 13
    makeCall $ T.Bid 1 T.Notrump


-- canape bids

b1D1M1X2N_ :: Action
b1D1M1X2N_ = do
    minSuitLength T.Clubs 5
    makeAlertableCall (T.Bid 2 T.Notrump)
                      ("5+ clubs, forces " .+ T.Bid 3 T.Clubs .+ " from " .+
                       "partner")

b1D1H1S2N :: Action
b1D1H1S2N = nameAction "lampe_b1D1H1S2N" $ b1D1M1X2N_

b1D1H1N2N :: Action
b1D1H1N2N = nameAction "lampe_b1D1H1N2N" $ b1D1M1X2N_

b1D1S1N2N :: Action
b1D1S1N2N = nameAction "lampe_b1D1S1N2N" $ b1D1M1X2N_


b1D1M1X3C_ :: T.Suit -> Action
b1D1M1X3C_ majorSuit = do
    minSuitLength T.Diamonds 5
    pointRange 14 40
    makeAlertableCall (T.Bid 3 T.Clubs)
                      ("5+ diamonds, 4 " .+ show majorSuit .+ ", GF")

b1D1H1S3C :: Action
b1D1H1S3C = nameAction "lampe_b1D1H1S3C" $ b1D1M1X3C_ T.Hearts

b1D1H1N3C :: Action
b1D1H1N3C = nameAction "lampe_b1D1H1N3C" $ b1D1M1X3C_ T.Hearts

b1D1S1N3C :: Action
b1D1S1N3C = nameAction "lampe_b1D1S1N3C" $ b1D1M1X3C_ T.Spades


b1D1M1X3D_ :: T.Suit -> Action
b1D1M1X3D_ majorSuit = do
    minSuitLength T.Diamonds 5
    minSuitLength majorSuit 5
    pointRange 14 40
    makeAlertableCall (T.Bid 3 T.Diamonds)
                      ("5+ diamonds, 5+ " .+ show majorSuit .+ ", GF")

b1D1H1S3D :: Action
b1D1H1S3D = nameAction "lampe_b1D1H1S3D" $ b1D1M1X3D_ T.Hearts

b1D1H1N3D :: Action
b1D1H1N3D = nameAction "lampe_b1D1H1N3D" $ b1D1M1X3D_ T.Hearts

b1D1S1N3D :: Action
b1D1S1N3D = nameAction "lampe_b1D1S1N3D" $ b1D1M1X3D_ T.Spades


-- shape-showing helpers

shapeShower2S_ :: Action
shapeShower2S_ = do
    alternatives [ suitLength T.Clubs 5 >> suitLength T.Diamonds 3
                 , suitLength T.Clubs 3 >> suitLength T.Diamonds 5 ]
    makeAlertableCall (T.Bid 2 T.Spades) "5-3 in the minors, either way"


-- TODO: when would you use this, and when would you not?
shapeShower2S2N_ :: Action
shapeShower2S2N_ = makeAlertableCall (T.Bid 2 T.Notrump) "bid your 5-card minor"


shapeShower2S3C_ :: Action
shapeShower2S3C_ = do
    suitLength T.Clubs 5
    makeCall $ T.Bid 3 T.Clubs


shapeShower2S3D_ :: Action
shapeShower2S3D_ = do
    suitLength T.Diamonds 5
    makeCall $ T.Bid 3 T.Diamonds


shapeShower2N_ :: Action
shapeShower2N_ = do
    maxSuitLength T.Clubs 1
    makeAlertableCall (T.Bid 2 T.Notrump)
        "short clubs, likely either 5 or a bad 6 diamonds, or 4441 shape"


shapeShower3C_ :: Action
shapeShower3C_ = do
    forEach T.minorSuits (`minSuitLength` 4)
    makeAlertableCall (T.Bid 3 T.Clubs) "at least 4-4 in the minors"


shapeShower3D_ :: Action
shapeShower3D_ = do
    minSuitLength T.Diamonds 6
    makeCall $ T.Bid 3 T.Diamonds


shapeShower3H_ :: Action
shapeShower3H_ = do
    minSuitLength T.Diamonds 6
    minSuitLength T.Hearts 5
    T.Diamonds `longerThan` T.Hearts
    makeCall $ T.Bid 3 T.Hearts


shapeShower3S_ :: Action
shapeShower3S_ = do
    minSuitLength T.Diamonds 6
    minSuitLength T.Spades 5
    T.Diamonds `longerThan` T.Spades
    makeCall $ T.Bid 3 T.Spades


-- Now start the exportable bids


b1D2C :: Action
b1D2C = nameAction "lampe_b1D2C" $ do
    pointRange 11 40
    forbid balancedHand
    -- If you've got a 4-card major, bid that first, and use the canape bids to
    -- show a 5-card minor afterwards.
    forEach T.majorSuits (`maxSuitLength` 3)
    alternatives [minSuitLength T.Clubs 5, minSuitLength T.Diamonds 5]
    makeAlertableCall (T.Bid 2 T.Clubs) "inv+ with one or both minors"


b1D2C2D :: Action
b1D2C2D = nameAction "lampe_b1D2C2D" $ do
    pointRange 10 13
    forbid balancedHand
    makeAlertableCall (T.Bid 2 T.Diamonds) "unbalanced minimum"


b1D2C2H :: Action
b1D2C2H = nameAction "lampe_b1D2C2H" $ do
    pointRange 11 13
    balancedHand
    makeAlertableCall (T.Bid 2 T.Hearts) ("balanced 11" .+ NDash .+ "13 HCP")


b1D2C2S :: Action
b1D2C2S = nameAction "lampe_b1D2C2S" $ do
    pointRange 14 15
    shapeShower2S_

b1D2C2S2N :: Action
b1D2C2S2N = nameAction "lampe_b1D2C2S2N" $ shapeShower2S2N_

b1D2C2S2N3C :: Action
b1D2C2S2N3C = nameAction "lampe_b1D2C2S2N3C" $ shapeShower2S3C_

b1D2C2S2N3D :: Action
b1D2C2S2N3D = nameAction "lampe_b1D2C2S2N3D" $ shapeShower2S3D_

b1D2C2N :: Action
b1D2C2N = nameAction "lampe_b1D2C2N" $ do
    pointRange 14 15
    shapeShower2N_

b1D2C3C :: Action
b1D2C3C = nameAction "lampe_b1D2C3C" $ do
    pointRange 14 15
    shapeShower3C_

b1D2C3D :: Action
b1D2C3D = nameAction "lampe_b1D2C3D" $ do
    pointRange 14 15
    shapeShower3D_

b1D2C3H :: Action
b1D2C3H = nameAction "lampe_b1D2C3H" $ do
    pointRange 14 15
    shapeShower3H_

b1D2C3S :: Action
b1D2C3S = nameAction "lampe_b1D2C3S" $ do
    pointRange 14 15
    shapeShower3S_


b1D2C2D2H :: Action
b1D2C2D2H = nameAction "lampe_b1D2C2D2H" $ do
    pointRange 14 40
    makeAlertableCall (T.Bid 2 T.Hearts) "artificial GF, asks about minors"


b1D2C2D2H2S :: Action
b1D2C2D2H2S = nameAction "lampe_b1D2C2D2H2S" $ shapeShower2S_

b1D2C2D2H2S2N :: Action
b1D2C2D2H2S2N = nameAction "lampe_b1D2C2D2H2S2N" $ shapeShower2S2N_

b1D2C2D2H2S2N3C :: Action
b1D2C2D2H2S2N3C = nameAction "lampe_b1D2C2D2H2S2N3C" $ shapeShower2S3C_

b1D2C2D2H2S2N3D :: Action
b1D2C2D2H2S2N3D = nameAction "lampe_b1D2C2D2H2S2N3D" $ shapeShower2S3D_

b1D2C2D2H2N :: Action
b1D2C2D2H2N = nameAction "lampe_b1D2C2D2H2N" $ shapeShower2N_

b1D2C2D2H3C :: Action
b1D2C2D2H3C = nameAction "lampe_b1D2C2D2H3C" $ shapeShower3C_

b1D2C2D2H3D :: Action
b1D2C2D2H3D = nameAction "lampe_b1D2C2D2H3D" $ shapeShower3D_

-- You'd only open 1D with a 5-card major if you were a maximum, and you've
-- already shown that you're a minimum. So, the extra shape-shower bids cannot
-- exist in this sequence.
{-
b1D2C2D2H3H :: Action
b1D2C2D2H3H = nameAction "lampe_b1D2C2D2H3H" $ shapeShower3H_

b1D2C2D2H3S :: Action
b1D2C2D2H3S = nameAction "lampe_b1D2C2D2H3S" $ shapeShower3S_
-}


b1D2D :: Action
b1D2D = nameAction "lampe_b1D2D" $ do
    pointRange 8 13
    minSuitLength T.Spades 5
    minSuitLength T.Hearts 4
    makeAlertableCall (T.Bid 2 T.Diamonds)
        "at most invitational, with 5+ spades and 4+ hearts"
