module Bids.Woolsey(
    b1NweaoX  -- Double is penalty against weak notrump
  , b1NstroX  -- Double is conventional against strong notrump
  , b1NoX2C
  , b1NoX2CP
  , b1NoX2C2D
  , b1NoX2D
  , b1NoX2D2H
  , b1NoX2D2S
  , b1No2C
  , b1No2C2H
  , b1No2C2S
  , b1No2D
  , b1No2D2H
  , b1No2D2HP
  , b1No2D2H2S
  , b1No2H
  , b1No2H2N
  , b1No2H2N3C
  , b1No2H2N3D
  , b1No2S
  , b1No2S2N
  , b1No2S2N3C
  , b1No2S2N3D
  , b1No2N
  , b1No2N3C
  , b1No2N3D
  , b1No3C
  , b1No3D
) where


import Action(Action, define, constrain)
import Bids.NaturalOneNotrumpDefense(singleSuited, twoSuited)
import qualified Bids.Cappelletti as Cappelletti
import EDSL(minSuitLength, maxSuitLength, makeCall, makeAlertableCall,
            alternatives, forEach, nameAction, longerThan, strongerThan,
            pointRange)
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


-- After the conventional double, advancer should assume the worst (overcaller
-- has your shorter minor, and your shorter major). Then, decide which one you
-- like from those.
prepareAdvancer_ :: Action
prepareAdvancer_ = do
    define "longer_minor"
        ["clubs(", ") > diamonds(", ") ? clubs(", ") : diamonds(", ")"]
    define "longer_major"
        ["hearts(", ") > spades(", ") ? hearts(", ") : spades(", ")"]
    -- If you've got your own self-sufficient suit, you might be tempted to bid
    -- that instead. Avoid this ambiguity.
    forEach T.allSuits (`maxSuitLength` 6)

b1NoX2C :: Action
b1NoX2C = nameAction "wool_b1NoX2C" $ do
    prepareAdvancer_
    constrain "prefer_minor" ["longer_minor_", " >= longer_major_", ""]
    makeAlertableCall (T.Bid 2 T.Clubs) "prefer the minor: pass or correct"

b1NoX2D :: Action
b1NoX2D = nameAction "wool_b1NoX2D" $ do
    prepareAdvancer_
    constrain "prefer_minor" ["longer_minor_", " < longer_major_", ""]
    makeAlertableCall (T.Bid 2 T.Diamonds) "bid your major"


b1NoX2CP :: Action
b1NoX2CP = nameAction "wool_b1NoX2CP" $ do
    T.Clubs `longerThan` T.Diamonds
    makeCall T.Pass


b1NoX2C2D :: Action
b1NoX2C2D = nameAction "wool_b1NoX2C2D" $ do
    T.Diamonds `longerThan` T.Clubs
    makeCall $ T.Bid 2 T.Diamonds


b1NoX2D2H :: Action
b1NoX2D2H = nameAction "wool_b1NoX2D2H" $ do
    T.Hearts `longerThan` T.Spades
    makeCall $ T.Bid 2 T.Hearts


b1NoX2D2S :: Action
b1NoX2D2S = nameAction "wool_b1NoX2D2S" $ do
    T.Spades `longerThan` T.Hearts
    makeCall $ T.Bid 2 T.Spades


b1No2C :: Action
b1No2C = nameAction "wool_b1No2C" $ do
    twoSuited T.Hearts T.Spades
    makeAlertableCall (T.Bid 2 T.Clubs) "both majors"


b1No2C2H :: Action
b1No2C2H = nameAction "wool_b1No2C2H" $ do
    T.Hearts `strongerThan` T.Spades
    makeCall $ T.Bid 2 T.Hearts


b1No2C2S :: Action
b1No2C2S = nameAction "wool_b1No2C2S" $ do
    T.Spades `strongerThan` T.Hearts
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


majorAndMinor_ :: T.Suit -> Action
majorAndMinor_ major =
    alternatives [ twoSuited major T.Clubs    >> minSuitLength major 5
                 , twoSuited major T.Diamonds >> minSuitLength major 5
                 ]

b1No2H :: Action
b1No2H = nameAction "wool_b1No2H" $ do
    majorAndMinor_ T.Hearts
    makeAlertableCall (T.Bid 2 T.Hearts) "5+ hearts and 4+ in a minor"

b1No2S :: Action
b1No2S = nameAction "wool_b1No2S" $ do
    majorAndMinor_ T.Spades
    makeAlertableCall (T.Bid 2 T.Spades) "5+ spades and 4+ in a minor"


preferMinor_ :: T.Suit -> Action
preferMinor_ major = do
    -- There are probably other hand shapes that would prefer the minor, but
    -- figuring them all out is harder than just limiting the hand types to the
    -- easy ones. When you encounter the hard ones at the table, hopefully this
    -- practice has helped.
    alternatives [ maxSuitLength major 2 >>
                       forEach T.minorSuits (`minSuitLength` 4)
                 , maxSuitLength major 1 >>
                       forEach T.minorSuits (`minSuitLength` 3)
                 , maxSuitLength major 0 >>
                       forEach T.minorSuits (`minSuitLength` 2) >>
                       forEach T.majorSuits (`maxSuitLength` 4)
                 ]
    -- If we had our own long suit, we might be tempted to bid that instead.
    forEach T.allSuits (`maxSuitLength` 5)
    makeAlertableCall (T.Bid 2 T.Notrump) "bid your minor"

b1No2H2N :: Action
b1No2H2N = nameAction "wool_b1No2H2N" (preferMinor_ T.Hearts)

b1No2S2N :: Action
b1No2S2N = nameAction "wool_b1No2S2N" (preferMinor_ T.Spades)


b1No2H2N3C :: Action
b1No2H2N3C = nameAction "wool_b1No2M2N3C" $ do
    T.Clubs `longerThan` T.Diamonds
    makeCall (T.Bid 3 T.Clubs)

b1No2H2N3D :: Action
b1No2H2N3D = nameAction "wool_b1No2M2N3D" $ do
    T.Diamonds `longerThan` T.Clubs
    makeCall (T.Bid 3 T.Diamonds)

-- Rebidding the minors are identical regardless of which major you have.
b1No2S2N3C :: Action
b1No2S2N3C = b1No2H2N3C

b1No2S2N3D :: Action
b1No2S2N3D = b1No2H2N3D


b1No2N :: Action
b1No2N = nameAction "wool_b1No2N" $ do
    twoSuited T.Clubs T.Diamonds
    makeAlertableCall (T.Bid 2 T.Notrump) "both minors"


b1No2N3C :: Action
b1No2N3C = nameAction "wool_b1No2N3C" $ do
    T.Clubs `strongerThan` T.Diamonds
    makeCall $ T.Bid 3 T.Clubs


b1No2N3D :: Action
b1No2N3D = nameAction "wool_b1No2N3D" $ do
    T.Diamonds `strongerThan` T.Clubs
    makeCall $ T.Bid 3 T.Diamonds


-- Single-suited minors need to overcall at the 3 level. You should be either
-- stronger or shapelier than usual to do this.
singleMinor_ :: T.Suit -> Action
singleMinor_ suit = do
    singleSuited suit
    alternatives [ minSuitLength suit 6 >> pointRange 14 40
                 , minSuitLength suit 7
                 ]
    makeCall $ T.Bid 3 suit

b1No3C :: Action
b1No3C = nameAction "wool_b1No3C" $ singleMinor_ T.Clubs

b1No3D :: Action
b1No3D = nameAction "wool_b1No3D" $ singleMinor_ T.Diamonds
