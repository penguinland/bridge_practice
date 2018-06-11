module CommonBids(
  strong1NT
, weak1NT
, firstSeatOpener
, secondSeatOpener
, thirdSeatOpener
, fourthSeatOpener
, setDealerAndOpener
) where

import Auction(Action, constrain, define, forbid, pointRange, balancedHand, makeCall, makePass)
import DealerProg(addDefn, addReq)
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


setDealerAndOpener :: T.Direction -> T.Direction -> Action
setDealerAndOpener = helper [firstSeatOpener, secondSeatOpener,
                             thirdSeatOpener, fourthSeatOpener]
  where
    helper (action:actions) caller opener
      | caller == opener = action
      | otherwise        = forbid action >> makePass >>
                           helper actions (T.next caller) opener
