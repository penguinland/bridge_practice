module CommonBids(
  strong1NT
, weak1NT
, preempt4
, preempt3
, weak2
, cannotPreempt
, firstSeatOpener
, secondSeatOpener
, thirdSeatOpener
, fourthSeatOpener
, setOpener
, takeoutDouble
) where

import Auction(Action, constrain, define, forbid, pointRange, balancedHand, makeCall, makePass, suitLength, minSuitLength, maxSuitLength)
import Structures(currentBidder)
import Control.Monad.Trans.State.Strict(get)
-- Not currently needed:
--import DealerProg(addDefn, addReq)
import qualified Terminology as T

strong1NT :: Action
strong1NT = do
    balancedHand
    pointRange 15 17
    makeCall (T.Bid 1 T.Notrump)


weak1NT :: Action
weak1NT = do
    balancedHand
    pointRange 12 14
    makeCall (T.Bid 1 T.Notrump)


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
    sequence_ $ map (\s -> forbid (weak2 s)) [T.Diamonds, T.Hearts, T.Spades]
    sequence_ $ map (\s -> forbid (preempt3 s)) T.allSuits
    sequence_ $ map (\s -> forbid (preempt4 s)) T.allSuits


get2LongestSuits :: Action
get2LongestSuits = do
    -- The dealer program is not smart enough to figure it out when a variable
    -- at the top of the program is defined in terms of a variable at the
    -- bottom. Consequently, we need to impose an ordering on these, so that the
    -- predecessors are defined before the successors. We do that with
    -- one-letter prefixes.
    define "a_longer_major"
           ["hearts(", ") > spades(", ") ? hearts(", ") : spades(", ")"]
    define "a_shorter_major"
           ["hearts(", ") > spades(", ") ? spades(", ") : hearts(", ")"]
    define "a_longer_minor"
           ["clubs(", ") > diamonds(", ") ? clubs(", ") : diamonds(", ")"]
    define "a_shorter_minor"
           ["clubs(", ") > diamonds(", ") ? diamonds(", ") : clubs(", ")"]
    define "b_longest_suit" ["a_longer_major_", " > a_longer_minor_", " ?\
                            \ a_longer_major_", " : a_longer_minor_", ""]
    define "b_second_from_long_major"
           ["a_shorter_major_", " > a_longer_minor_", " ?\
           \ a_shorter_major_", " : a_longer_minor_", ""]
    define "b_second_from_long_minor"
           ["a_shorter_minor_", " > a_longer_major_", " ?\
           \ a_shorter_minor_", " : a_longer_major_", ""]
    define "c_second_longest_suit"
           ["a_longer_major_", " > a_longer_minor_", " ?\
           \ b_second_from_long_major_", " : b_second_from_long_minor_", ""]
    define "d_two_longest_suits"
           ["b_longest_suit_", " + c_second_longest_suit_", ""]


firstSeatOpener :: Action
firstSeatOpener = do
    get2LongestSuits
    constrain "rule_of_20" ["hcp(", ") + d_two_longest_suits_", " >= 20"]

secondSeatOpener :: Action
secondSeatOpener = do
    get2LongestSuits
    constrain "rule_of_20" ["hcp(", ") + d_two_longest_suits_", " >= 20"]

thirdSeatOpener :: Action
thirdSeatOpener = do
    get2LongestSuits
    constrain "rule_of_18" ["hcp(", ") + d_two_longest_suits_", " >= 18"]

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
    sequence_ $ map setSuitLength T.allSuits
  where
    setSuitLength suit =
        if suit == shortSuit
        then maxSuitLength suit 2
        else minSuitLength suit 3
