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
import Bids.StandardOpenings(b1N)
import EDSL(pointRange, minSuitLength, maxSuitLength, makeCall, alternatives,
            soundHolding, makeAlertableCall, forEach)
import Output ((.+))
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
    forEach (filter (/= suit) T.allSuits) (`maxSuitLength` 3)


-- TODO: there should probably be more constraints on suit quality, rather than
-- just length. Should both suits be sound? At least one be sound? Maybe
-- neither? Maybe it depends on the vulnerability? Can the suits be 5-4 either
-- way, or must one always be at least 5? I need opinions from someone who has
-- better fundamentals on this stuff.
twoSuited :: T.Suit -> T.Suit -> Action
twoSuited a b = do
    -- With a 6-card suit, you might be better off treating your hand as
    -- single-suited, depending on the suit quality. Rather than program all
    -- that nuance in, we limit ourselves to either 5-4 or 5-5 shapes.
    forEach [a, b] (`minSuitLength` 4)
    forEach [a, b] (`maxSuitLength` 5)
    alternatives [minSuitLength a 5, minSuitLength b 5]
    -- For simplicity, we also forbid having any type of 3-suited hand. It's
    -- easy enough to say "don't show both minors if you've got a major," but
    -- deciding which minor to show requires nuance that I don't have the
    -- patience to program in right now.
    forEach (filter (/= a) . filter (/= b) $ T.allSuits) (`maxSuitLength` 3)


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
    alternatives [twoSuited minor T.Hearts, twoSuited minor T.Spades]
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
