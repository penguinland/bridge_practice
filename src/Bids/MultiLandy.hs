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
import EDSL(makeAlertableCall, alternatives, pointRange, balancedHand)
import Output ((.+))
import qualified Terminology as T


b1NoX :: Action
b1NoX = do
    pointRange 17 40
    -- This is probably the wrong approach: if you're 4441, you might bid this,
    -- too. and if you're 5521 with good singletons and doubletons and bad
    -- 5-card suits, you might double, too. but certainly if you've got a
    -- balanced hand, this is the right call.
    -- TODO: broaden this to more possible hand types when it's clear what they
    -- are.
    balancedHand
    makeAlertableCall T.Double "penalty-oriented"


b1No2C :: Action
b1No2C = do
    twoSuited T.Hearts T.Spades
    makeAlertableCall (T.Bid 2 T.Clubs) "both majors"


b1No2D :: Action
b1No2D = do
    alternatives [singleSuited T.Hearts, singleSuited T.Spades]
    makeAlertableCall (T.Bid 2 T.Clubs) "one long major"


minorAndMajor :: T.Suit -> Action
minorAndMajor major = do
    alternatives [twoSuited major T.Clubs, twoSuited major T.Diamonds]
    makeAlertableCall (T.Bid 2 major) (show major .+ " and a minor")

b1No2H :: Action
b1No2H = minorAndMajor T.Hearts

b1No2S :: Action
b1No2S = minorAndMajor T.Spades


b1No2N :: Action
b1No2N = do
    twoSuited T.Clubs T.Diamonds
    makeAlertableCall (T.Bid 2 T.Notrump) "both minors"
