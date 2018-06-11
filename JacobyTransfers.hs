module JacobyTransfers(topic) where

import Data.List.Utils(join)

import Output(output)
import Topic(Topic(..), base, (<~), wrap)
import Auction(forbid, makeCall, makePass, pointRange, suitLength,
               minSuitLength, maxSuitLength, Action, balancedHand, withholdBid,
               constrain)
import Situation(situation, Situation)
import qualified Terminology as T
import qualified CommonBids as B


-- syntactic sugar
oneNT :: T.Call
oneNT = T.Bid 1 T.Notrump


topic :: Topic
topic = Topic "Jacoby transfers" situations
  where
    prepare s = wrap $ s <~ T.allVulnerabilities
    situations = wrap [
        prepare $ base initiateTransfer <~ T.allDirections <~ T.majorSuits
      , prepare $ base completeTransfer <~ T.allDirections <~ T.majorSuits
      -- Note that with 5-5 and game-going strength, you would have opened the
      -- bidding if you had a chance.
      , wrap . map prepare $ [ base majors55gf <~ [T.West, T.North]
                             , base majors55inv <~ T.allDirections]
      ]

-- TODO: Add separate commentary for 5-4 non-gf hands. Alternately, forbid 5-4
-- non-gf hands, and add that situation into the Smolen topic.


no2LevelOvercall :: [T.Suit] -> Action
no2LevelOvercall suits =
    mapM_ (forbid . condition) suits >> makePass
  where
    condition s = pointRange 11 40 >> minSuitLength s 5


transferSuit :: T.Suit -> T.Suit
transferSuit T.Hearts = T.Diamonds
transferSuit T.Spades = T.Hearts
transferSuit _        = error "Jacoby-like transfer of non-major suit!"

otherMajor :: T.Suit -> T.Suit
otherMajor T.Hearts = T.Spades
otherMajor T.Spades = T.Hearts
otherMajor _        = error "Other major of non-major suit!"


-- Although this topic is about Jacoby transfers, we exclude auctions that would
-- be better served by a Texas transfer so as not to confuse the learner.
texasTransfer :: T.Suit -> Action
texasTransfer suit = do
    minSuitLength suit 6
    pointRange 10 15
    makeCall (T.Bid 4 $ transferSuit suit)


equalMajors :: Action
equalMajors = constrain "equal_majors" ["hearts(", ") == spades(", ")"]


smolen :: T.Suit -> Action  -- The suit is the longer major.
smolen suit = do
    minSuitLength suit 5
    minSuitLength (otherMajor suit) 4
    pointRange 10 40
    -- With 5-5 in the majors, make a Jacoby transfer then bid the other suit.
    -- With 6-6, I guess you do the same? but it never comes up.
    forbid equalMajors


jacobyTransfer :: T.Suit -> Action
jacobyTransfer suit = do
    minSuitLength suit 5
    forbid $ texasTransfer suit
    forbid $ smolen suit
    -- Make this simple by leaving out 5-5 hands. They go in another situation.
    forbid equalMajors
    makeCall (T.Bid 2 $ transferSuit suit)


initiateTransfer :: T.Direction -> T.Suit -> T.Vulnerability -> Situation
initiateTransfer dealer suit vul = let
    action = do
        B.setDealerAndOpener dealer T.North
        B.strong1NT
        no2LevelOvercall T.allSuits
        withholdBid $ jacobyTransfer suit
    explanation fmt =
        "Partner has opened a strong " ++ output fmt oneNT ++ ". You have a\
      \ 5-card major, and would like to choose that as the trump suit. Make a\
      \ Jacoby transfer by bidding the suit directly below your own. That\
      \ way, you get to pick the trump suit, but your partner will be\
      \ declarer so his strong hand will stay hidden."
  in
    situation dealer vul action (T.Bid 2 $ transferSuit suit) explanation


completeTransfer :: T.Direction -> T.Suit -> T.Vulnerability -> Situation
completeTransfer dealer suit vul = let
    higherSuits = if suit == T.Spades then [] else [T.Spades]
    action = do
        B.setDealerAndOpener dealer T.South
        B.strong1NT
        no2LevelOvercall T.allSuits
        jacobyTransfer suit
        no2LevelOvercall higherSuits
    explanation fmt =
        "You have opened a strong " ++ output fmt oneNT ++ ", and partner has\
      \ made a Jacoby transfer. Complete the transfer by bidding the next\
      \ higher suit. Partner promises at least 5 cards in that major, but\
      \ wants you to be declarer so your stronger hand stays hidden."
  in
    situation dealer vul action (T.Bid 2 suit) explanation


majors55inv :: T.Direction -> T.Vulnerability -> Situation
majors55inv dealer vul = let
    action = do
        B.setDealerAndOpener dealer T.North
        B.strong1NT
        no2LevelOvercall T.allSuits
        suitLength T.Hearts 5
        suitLength T.Spades 5
        pointRange 7 9
    explanation fmt =
        "Partner has opened a strong " ++ output fmt oneNT ++ ". With 5-5 in\
      \ the majors and invitational strength, first make a Jacoby transfer\
      \ into hearts, and then bid " ++ output fmt (T.Bid 2 T.Spades) ++ "\
      \ afterwards. Partner will then have the options of passing " ++
        output fmt (T.Bid 2 T.Spades) ++ " with a minimum hand and a spade\
      \ fit, bidding " ++ output fmt (T.Bid 3 T.Hearts) ++ "with a minimum\
      \ hand and no spade fit (in which case a heart fit is guaranteed), or\
      \ bidding one of the majors at the 4 level with a maximum. This\
      \ wrong-sides the contract when the " ++ output fmt oneNT ++ " bidder\
      \ has a doubleton heart."
  in
    situation dealer vul action (T.Bid 2 T.Diamonds) explanation


majors55gf :: T.Direction -> T.Vulnerability -> Situation
majors55gf dealer vul = let
    action = do
        B.setDealerAndOpener dealer T.North
        B.strong1NT
        no2LevelOvercall T.allSuits
        suitLength T.Hearts 5
        suitLength T.Spades 5
        pointRange 10 14
    explanation fmt =
        "Partner has opened a strong " ++ output fmt oneNT ++ ". With 5-5 in\
        \ the majors and\
        \ game-forcing strength, first make a Jacoby transfer into spades,\
        \ and then bid " ++ output fmt (T.Bid 3 T.Hearts) ++ " afterwards.\
        \ Partner will then have the options of which game to bid.\
        \ This wrong-sides the contract when the " ++ output fmt oneNT ++ "\
        \ bidder has a doubleton spade."
  in
    situation dealer vul action (T.Bid 2 T.Hearts) explanation


-- TODO: Make more detailed situations for when responder has no interest in
-- game and an unbalanced hand, invitational strength and a balanced hand, and
-- game-forcing strength with a balanced hand. Make situations for all of
-- these from the perspective of both opener and responder.
