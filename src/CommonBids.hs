module CommonBids(
  strong1NT
, weak1NT
, preempt4
, preempt3
, weak2
, cannotPreempt
, cannotPreempt2H
, firstSeatOpener
, secondSeatOpener
, thirdSeatOpener
, fourthSeatOpener
, setOpener
, takeoutDouble
, noInterference
) where

import Control.Monad.Trans.State.Strict(get)

import Action(Action, constrain, define)
import EDSL(forbid, pointRange, balancedHand, makeCall, makeAlertableCall,
            makePass, suitLength, minSuitLength, maxSuitLength, alternatives,
            forEach)
import Output(Punct(..), (.+))
import Structures(currentBidder)
import qualified Terminology as T


strong1NT :: Action
strong1NT = do
    balancedHand
    pointRange 15 17
    makeAlertableCall (T.Bid 1 T.Notrump) ("15" .+ NDash .+ "17 HCP")


weak1NT :: Action
weak1NT = do
    balancedHand
    pointRange 12 14
    makeAlertableCall (T.Bid 1 T.Notrump) ("12" .+ NDash .+ "14 HCP")


preempt4 :: T.Suit -> Action
preempt4 suit = do
    minSuitLength suit 8
    pointRange 5 13  -- TODO: figure out the correct point range
    makeCall (T.Bid 4 suit)

preempt3 :: T.Suit -> Action
preempt3 T.Clubs = do  -- Compensate for 2C being a strong bid
    forbid (preempt4 T.Clubs)
    minSuitLength T.Clubs 6
    maxSuitLength T.Clubs 7
    pointRange 5 11
    -- TODO: Clarify the nuance of opening 3C.
    makeCall (T.Bid 3 T.Clubs)
preempt3 suit = do
    forbid (preempt4 suit)
    suitLength suit 7
    pointRange 5 9  -- TODO: figure out this point range, too.
    makeCall (T.Bid 3 suit)

weak2 :: T.Suit -> Action
weak2 T.Clubs = error "Don't bid a weak 2C."
weak2 suit = do
    forbid (preempt4 suit)
    forbid (preempt3 suit)
    suitLength suit 6
    pointRange 5 11
    -- TODO: clarify hands with 11 HCP and a 6-card suit that should open at the
    -- 1 level from hands that should open at the 2 level.
    makeCall (T.Bid 2 suit)


cannotPreempt :: Action
cannotPreempt = do
    forEach [T.Diamonds, T.Hearts, T.Spades] (forbid . weak2)
    forEach T.allSuits (forbid . preempt3)
    forEach T.allSuits (forbid . preempt4)


-- Variant in which 2D has some artificial meaning
cannotPreempt2H :: Action
cannotPreempt2H = do
    forEach [T.Hearts, T.Spades] (forbid . weak2)
    forEach T.allSuits (forbid . preempt3)
    forEach T.allSuits (forbid . preempt4)


get2LongestSuits :: Action
get2LongestSuits = do
    define "longer_major"
           ["hearts(", ") > spades(", ") ? hearts(", ") : spades(", ")"]
    define "shorter_major"
           ["hearts(", ") > spades(", ") ? spades(", ") : hearts(", ")"]
    define "longer_minor"
           ["clubs(", ") > diamonds(", ") ? clubs(", ") : diamonds(", ")"]
    define "shorter_minor"
           ["clubs(", ") > diamonds(", ") ? diamonds(", ") : clubs(", ")"]
    define "longest_suit" ["longer_major_", " > longer_minor_", " ?\
                          \ longer_major_", " : longer_minor_", ""]
    define "second_from_long_major"
           ["shorter_major_", " > longer_minor_", " ?\
           \ shorter_major_", " : longer_minor_", ""]
    define "second_from_long_minor"
           ["shorter_minor_", " > longer_major_", " ?\
           \ shorter_minor_", " : longer_major_", ""]
    define "second_longest_suit"
           ["longer_major_", " > longer_minor_", " ?\
           \ second_from_long_major_", " : second_from_long_minor_", ""]
    define "two_longest_suits"
           ["longest_suit_", " + second_longest_suit_", ""]


firstSeatOpener :: Action
firstSeatOpener = do
    get2LongestSuits
    constrain "rule_of_20" ["hcp(", ") + two_longest_suits_", " >= 20"]

secondSeatOpener :: Action
secondSeatOpener = do
    get2LongestSuits
    constrain "rule_of_20" ["hcp(", ") + two_longest_suits_", " >= 20"]

thirdSeatOpener :: Action
thirdSeatOpener = do
    get2LongestSuits
    constrain "rule_of_18" ["hcp(", ") + two_longest_suits_", " >= 18"]

fourthSeatOpener :: Action
fourthSeatOpener =
    constrain "rule_of_15" ["hcp(", ") + spades(", ") >= 15"]


setOpener :: T.Direction -> Action
setOpener opener = do
    (bidding, _) <- get
    helper openingRules (currentBidder bidding)
  where
    openingRules = [firstSeatOpener, secondSeatOpener,
                    thirdSeatOpener, fourthSeatOpener]
    helper (action:actions) caller
      | caller == opener = action
      | otherwise        = do forbid action
                              cannotPreempt
                              makePass
                              helper actions (T.next caller)
    helper [] _          = error "Specified a fifth-seat opener!?"


takeoutDouble :: T.Suit -> Action
takeoutDouble shortSuit = do
    pointRange 11 40
    forEach T.allSuits setSuitLength
  where
    setSuitLength suit =
        if suit == shortSuit
        then maxSuitLength suit 2
        else minSuitLength suit 3


noInterference :: T.Suit -> Action
noInterference suit = do
    cannotPreempt
    -- Building out the entire overcall structure just so we can forbid any of
    -- it here is too complicated for now. Let's just say the opponent has at
    -- most 14 HCP, and either doesn't have a 5-card suit or has at most 7 HCP.
    pointRange 0 14
    forbid $ takeoutDouble suit
    alternatives [
        constrain "no_five_card_suit" [
            "shape(", ", any 4441, any 4333, any 4432)"]
      , pointRange 0 7]
    makePass
