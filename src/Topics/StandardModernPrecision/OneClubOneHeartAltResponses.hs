module Topics.StandardModernPrecision.OneClubOneHeartAltResponses(
  topic) where

import Auction({-forbid, maxSuitLength, makePass,-} pointRange)
--import CommonBids(cannotPreempt)
import Output(Punct(..), (.+))
import Situation(situation, (<~))
import qualified Terminology as T
import Topic(Topic, wrap, Situations, makeTopic)
import Topics.StandardModernPrecision.BasicBids(firstSeatOpener, oppsPass,
                                                {-smpWrapN,-} smpWrapS)
import qualified Topics.StandardModernPrecision.Bids1C1Halt as B


notrumpRebid :: Situations
notrumpRebid = let
    action = do
        firstSeatOpener
        B.b1C
        oppsPass
        B.b1C1H
        oppsPass
    explanation =
        "Partner's " .+ T.Bid 1 T.Hearts .+ " shows game-forcing strength. " .+
        "You've got a balanced hand with no 5-card suit, so show it by " .+
        "bidding notrump. Systems are on, so partner should have an easy " .+
        "time placing the contract."
  in
    smpWrapS . return $ situation "1N" action B.b1C1H1N explanation


strongNotrumpRebid :: Situations
strongNotrumpRebid = let
    action = do
        firstSeatOpener
        B.b1C
        oppsPass
        B.b1C1H
        oppsPass
        pointRange 21 40
    explanation =
        "Partner's " .+ T.Bid 1 T.Hearts .+ " shows game-forcing strength. " .+
        "You've got a balanced hand with no 5-card suit, so show it by " .+
        "bidding " .+ T.Bid 1 T.Notrump .+ ", even with a hand that would " .+
        "have jumped over a " .+ T.Bid 1 T.Diamonds .+ " response! You're " .+
        "in a game-forcing auction, so there is no need to tell partner " .+
        "you've got extra strength until partner tries signing off (at " .+
        "which point you keep going to show your strength). Systems are " .+
        "on, so partner should have an easy time placing the contract."
  in
    smpWrapS . return $ situation "1Nstr" action B.b1C1H1N explanation


naturalSuitRebid :: Situations
naturalSuitRebid = let
    sit bid = let
        action = do
            firstSeatOpener
            B.b1C
            oppsPass
            B.b1C1H
            oppsPass
        explanation =
            "You've got an unbalanced hand, and are already in a " .+
            "game-forcing auction. No need to do anything fancy; just bid " .+
            "your longest suit. If you are 5" .+ NDash .+ "5, prefer the " .+
            "major, or bid the higher one and rebid the lower one afterwards."
      in
        situation "NatBid" action bid explanation
  in
    smpWrapS $ return sit <~ [B.b1C1H1S, B.b1C1H2C, B.b1C1H2D, B.b1C1H2H]


tripleFourOne :: Situations
tripleFourOne = let
    action = do
        firstSeatOpener
        B.b1C
        oppsPass
        B.b1C1H
        oppsPass
    explanation =
        "Partner's " .+ T.Bid 1 T.Hearts .+ " shows game-forcing strength. " .+
        "You've got a 4441 hand, so rebid " .+ T.Bid 2 T.Spades .+ ". " .+
        "Partner will relay to " .+ T.Bid 2 T.Notrump .+ " to ask what your " .+
        "singleton is, which you can then bid at the 3 level. Partner will " .+
        "then set trump at the 3 level if they can (thereby asking you to " .+
        "control bid for a round), or else use " .+
        T.Bid 4 T.Clubs .+ "/" .+ T.Bid 4 T.Diamonds .+ "/RKC to tell you " .+
        "what suit is trumps and some sense of how high to go. They should " .+
        "never drop you in " .+ T.Bid 4 T.Hearts .+ ", since you are still " .+
        "unlimited, despite partner limiting their own hand."
  in
    smpWrapS . return $ situation "1C1H2S" action B.b1C1H2S explanation


topic :: Topic
topic = makeTopic description "SMP1C1Hnos" situations
  where
    description = "SMP modified continuations after " .+ T.Bid 1 T.Clubs .+
                  NDash .+ T.Bid 1 T.Hearts .+ " auctions"
    situations = wrap [ wrap [notrumpRebid, tripleFourOne, strongNotrumpRebid]
                      , naturalSuitRebid
                      ]
