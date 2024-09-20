module Bids.NaturalOneNotrumpDefense(
    b1No2C
  , b1No2D
  , b1No2H
  , b1No2S
  , b1No3C
  , b1No3D
  , b1No3H
  , b1No3S
  -- Export these helpers for other defenses against 1N, too
  , singleSuited
  , twoSuited
) where


import Action(Action)
import EDSL(pointRange, minSuitLength, maxSuitLength, makeCall, alternatives,
            soundHolding, forEach, forbidAll)
import qualified Terminology as T


-- What's the right minimum strength to overcall? It kinda depends on the
-- vulnerability and where in the hand this strength is located. Let's guess 10
-- is a pretty decent minimum, but I'm open to changing it later.
pointsToCompete :: Action
pointsToCompete = pointRange 10 40


singleSuited :: T.Suit -> Action
singleSuited suit = do
    minSuitLength suit 6
    shouldntPreempt
    soundHolding suit
    forEach (filter (/= suit) T.allSuits) (`maxSuitLength` 3)


-- TODO: there should probably be more constraints on suit quality, rather than
-- just length. Should both suits be sound? At least one be sound? Maybe
-- neither? Maybe it depends on the vulnerability? Can the suits be 5-4 either
-- way, or must a certain one always be at least 5? Maybe it depends on the
-- convention, and Meckwell has a different answer than DONT or something. I
-- need opinions from someone who has better fundamentals on this stuff.
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


-- unexported helper function
naturalSingleSuit_ :: T.Suit -> Action
naturalSingleSuit_ suit = do
    pointsToCompete
    singleSuited suit
    makeCall $ T.Bid 2 suit


b1No2C :: Action
b1No2C = naturalSingleSuit_ T.Clubs


b1No2D :: Action
b1No2D = naturalSingleSuit_ T.Diamonds


b1No2H :: Action
b1No2H = naturalSingleSuit_ T.Hearts


b1No2S :: Action
b1No2S = naturalSingleSuit_ T.Spades


preempt_ :: T.Suit -> Action
preempt_ suit = do
    pointsToCompete
    -- TODO: Would you still bid at the 4 level with an 8-card suit?
    minSuitLength suit 7
    soundHolding suit  -- Don't do this with a bad suit, even if you're nonvul.
    -- Some people are reluctant to pre-empt if they've got a side 4-card major.
    forEach (filter (/= suit) T.majorSuits) (`maxSuitLength` 3)
    makeCall $ T.Bid 3 suit

b1No3C :: Action
b1No3C = preempt_ T.Clubs

b1No3D :: Action
b1No3D = preempt_ T.Diamonds

b1No3H :: Action
b1No3H = preempt_ T.Hearts

b1No3S :: Action
b1No3S = preempt_ T.Spades

shouldntPreempt :: Action
shouldntPreempt = forbidAll[b1No3C, b1No3D, b1No3H, b1No3S]
