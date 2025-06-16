module Bids.Woolsey(
    b1NweaoX  -- Double is penalty against weak notrump
  , b1NstroX  -- Double is conventional against strong notrump
--  , b1NoX2C
--  , b1NoX2CP
--  , b1NoX2C2D
--  , b1NoX2D
--  , b1NoX2D2H
--  , b1NoX2D2S
  , b1No2C
  , b1No2C2H
  , b1No2C2S
  , b1No2D
  , b1No2D2H
  , b1No2D2HP
  , b1No2D2H2S
  , b1No2H
  --, b1No2H2N
  --, b1No2H2N3C
  --, b1No2H2N3D
  , b1No2S
  --, b1No2S2N
  --, b1No2S2N3C
  --, b1No2S2N3D
  , b1No2N
  , b1No2N3C
  , b1No2N3D
) where


import Action(Action)
import Bids.NaturalOneNotrumpDefense(singleSuited, twoSuited)
import qualified Bids.Cappelletti as Cappelletti
import EDSL(minSuitLength, maxSuitLength, makeCall, makeAlertableCall,
            alternatives, forEach, nameAction, longerThan, atLeastAsLong)
import qualified Terminology as T


b1NweaoX :: Action
b1NweaoX = Cappelletti.b1NoX


b1NstroX :: Action
b1NstroX = nameAction "wool_b1NstroX" $ do
    alternatives [ twoSuited T.Clubs    T.Hearts >> maxSuitLength T.Hearts 4
                 , twoSuited T.Diamonds T.Hearts >> maxSuitLength T.Hearts 4
                 , twoSuited T.Clubs    T.Spades >> maxSuitLength T.Spades 4
                 , twoSuited T.Diamonds T.Spades >> maxSuitLength T.Spades 4
                 ]
    makeAlertableCall T.Double "a 4-card major and longer minor"


b1No2C :: Action
b1No2C = nameAction "wool_b1No2C" $ do
    twoSuited T.Hearts T.Spades
    makeAlertableCall (T.Bid 2 T.Clubs) "both majors"


b1No2C2H :: Action
b1No2C2H = nameAction "wool_b1No2C2H" $ do
    -- If your major suits are the same length, bid the better one. That's
    -- slightly hard to code up, so just avoid those situations entirely.
    T.Hearts `longerThan` T.Spades
    makeCall $ T.Bid 2 T.Hearts


b1No2C2S :: Action
b1No2C2S = nameAction "wool_b1No2C2S" $ do
    -- If your major suits are the same length, bid the better one. That's
    -- slightly hard to code up, so just avoid those situations entirely.
    T.Spades `longerThan` T.Hearts
    makeCall $ T.Bid 2 T.Spades


b1No2D :: Action
b1No2D = nameAction "wool_b1No2D" $ do
    alternatives [b1No2D2HP, b1No2D2H2S]
    makeAlertableCall (T.Bid 2 T.Diamonds) "one long major"


b1No2D2H :: Action
b1No2D2H = nameAction "wool_b1No2D2H" $ do
    -- If you've got your own long suit, you might bid it. To prevent users from
    -- being tempted to do that, make sure there aren't long suits.
    forEach T.allSuits (`maxSuitLength` 5)
    makeAlertableCall (T.Bid 2 T.Hearts) "pass or correct"


b1No2D2HP :: Action
b1No2D2HP = nameAction "wool_b1No2D2HP" $ do
    singleSuited T.Hearts
    makeCall T.Pass


b1No2D2H2S :: Action
b1No2D2H2S = nameAction "wool_b1No2D2H2S" $ do
    singleSuited T.Spades
    makeCall $ T.Bid 2 T.Spades


b1No2H :: Action
b1No2H = nameAction "wool_b1No2H" $ do
    forEach T.minorSuits (\m ->
        twoSuited T.Hearts m >> minSuitLength T.Hearts 5)
    makeAlertableCall (T.Bid 2 T.Hearts) "5+ hearts and 4+ in a minor"


b1No2S :: Action
b1No2S = nameAction "wool_b1No2S" $ do
    forEach T.minorSuits (\m ->
        twoSuited T.Spades m >> minSuitLength T.Spades 5)
    makeAlertableCall (T.Bid 2 T.Spades) "5+ spades and 4+ in a minor"


b1No2N :: Action
b1No2N = nameAction "wool_b1No2N" $ do
    twoSuited T.Clubs T.Diamonds
    makeAlertableCall (T.Bid 2 T.Notrump) "both minors"


b1No2N3C :: Action
b1No2N3C = nameAction "wool_b1No2N3C" $ do
    T.Clubs `atLeastAsLong` T.Diamonds
    makeCall $ T.Bid 3 T.Clubs


b1No2N3D :: Action
b1No2N3D = nameAction "wool_b1No2N3D" $ do
    T.Diamonds `longerThan` T.Clubs
    makeCall $ T.Bid 3 T.Diamonds
