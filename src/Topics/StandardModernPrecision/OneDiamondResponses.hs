module Topics.StandardModernPrecision.OneDiamondResponses(topic) where

import Output(output, Punct(..))
import Topic(Topic(..), wrap, Situations)
import Auction(withholdBid, {-forbid, makePass,-} maxSuitLength, minSuitLength, {-suitLength,-}
               {-Action, balancedHand,-} pointRange{-, SuitLengthComparator(..), compareSuitLength-}, displayLastCall)
import Situation(situation, (<~))
--import CommonBids(cannotPreempt)
import qualified Terminology as T
import Topics.StandardModernPrecision.BasicBids(oppsPass, b1D, smpWrapN)
import qualified Topics.StandardModernPrecision.Bids1D as B


oneMajor :: Situations
oneMajor = let
    sit (suit, bid) = let
        action = do
            b1D
            oppsPass
            withholdBid bid
        explanation fmt =
            "Let's start with a natural " ++ output fmt (T.Bid 1 suit) ++ "\
           \ bid, and see where things go from there."
      in
        situation "1M" action bid explanation
  in
    smpWrapN $ return sit <~ [(T.Hearts, B.b1D1H), (T.Spades, B.b1D1S)]


twoMinor6M :: Situations
twoMinor6M = let
    sit (minor, bid) major = let
        action = do
            b1D
            oppsPass
            minSuitLength minor 6
            minSuitLength major 4
            maxSuitLength major 5
            pointRange 14 40
            withholdBid bid
        explanation _ =
            "With game-forcing strength, a 4- or 5-card major, but a 6+ card\
           \ minor, start by bidding 2 of the minor. There will be time to\
           \ show the major afterward."
      in
        situation "6m" action bid explanation
  in
    smpWrapN $ return sit <~ [(T.Clubs, B.b1D2C), (T.Diamonds, B.b1D2D)]
                          <~ [T.Hearts, T.Spades]


reverseFlannery :: Situations
reverseFlannery = let
    sit (bid, isInvite) = let
        action = do
            b1D
            oppsPass
            withholdBid bid
        explanation fmt =
            "With 5 spades, 4 or 5 hearts, and " ++
            if isInvite then "" else "less than " ++
            "invitational strength, bid a Reverse Flannery " ++
            displayLastCall fmt bid ++ ". Partner can then place the final\
          \ contract, bid " ++ output fmt (T.Bid 3 T.Clubs) ++ " (pass or\
          \ correct) with both minors, or bid " ++
            output fmt (T.Bid 2 T.Notrump) ++ " to ask for\
          \ more information. Note that if you were game forcing, you would\
          \ have started at the 1 level instead."
      in
        situation "RevFl" action bid explanation
  in
    smpWrapN $ return sit <~ [(B.b1D2H, False), (B.b1D2S, True)]


weakMinors54 :: Situations
weakMinors54 = let
    sit = let
        action = do
            b1D
            oppsPass
            withholdBid B.b1D3C
        explanation fmt =
            "With 5-4 in the minors, no 4-card major, and less than\
           \ invitational strength, bid a pre-emptive-like " ++
             output fmt (T.Bid 3 T.Clubs) ++ ". Partner can pass or correct, or\
           \ even continue the pre-empt if relevant. We might get unlucky and\
           \ end up in a 7-card fit, but most of the time we'll have a decent\
           \ fit in opener's favorite minor."
      in
        situation "54min" action B.b1D3C explanation
  in
    smpWrapN $ return sit


weakMinors55 :: Situations
weakMinors55 = let
    sit = let
        action = do
            b1D
            oppsPass
            withholdBid B.b1D4C
        explanation fmt =
            "With at least 5-5 in the minors and less than invitational\
           \ strength,\
           \ bid a pre-emptive-like " ++ output fmt (T.Bid 4 T.Clubs) ++ ".\
           \ Partner can pass or correct, or even continue the pre-empt if\
           \ relevant. We're guaranteed at least an 8-card fit in opener's\
           \ favorite minor."
      in
        situation "55min" action B.b1D4C explanation
  in
    smpWrapN $ return sit


notrump1 :: Situations
notrump1 = let
    sit = let
        action = do
            minSuitLength T.Hearts 5
            b1D
            oppsPass
            withholdBid B.b1D1N
        explanation fmt =
            "With a balanced hand, no 4-card major, and less than invitational\
           \ strength, bid " ++ output fmt (T.Bid 1 T.Notrump) ++ ". Partner\
           \ will likely pass, but could bid 2 of a major with 6-5 and a\
           \ maximum, which you can pass or correct."
      in
        situation "1nt" action B.b1D1N explanation
  in
    smpWrapN $ return sit


notrump2 :: Situations
notrump2 = let
    sit = let
        action = do
            minSuitLength T.Hearts 5
            b1D
            oppsPass
            withholdBid B.b1D2N
        explanation fmt =
            "With a balanced hand, no 4-card major, and 11" ++
             output fmt NDash ++ "12 HCP, bid " ++
             output fmt (T.Bid 2 T.Notrump) ++ ". Note that 13 HCP hands should\
           \ bid game even though they're usually considered invitational!\
           \ Partner can pass, bid " ++ output fmt (T.Bid 3 T.Clubs) ++ "\
           \ (pass or correct) with both minors and a minimum (with a maximum,\
           \ prefer playing in " ++ output fmt (T.Bid 2 T.Notrump) ++ "), " ++
             output fmt (T.Bid 3 T.Diamonds) ++ " with a long suit and minimum\
           \ strength, 3 of a major with 4 cards in that major and shortness in\
           \ the other one (angling for " ++ output fmt (T.Bid 3 T.Notrump) ++
            " or 4 of a minor if you don't have a stopper in the other major),\
           \ or 4 of a major with 6-5 and a maximum, which you can pass or\
           \ correct."
      in
        situation "2nt" action B.b1D2N explanation
  in
    smpWrapN $ return sit


notrump3 :: Situations
notrump3 = let
    sit = let
        action = do
            minSuitLength T.Hearts 5
            b1D
            oppsPass
            withholdBid B.b1D3N
        explanation fmt =
            "With a balanced hand, no 4-card major, and 13" ++
            output fmt NDash ++ "16 HCP, bid " ++
            output fmt (T.Bid 3 T.Notrump) ++ ". Partner\
           \ will likely pass, but could bid game in a major with 6-5 shape,\
           \ which you can pass or correct to " ++
             output fmt (T.Bid 4 T.Notrump) ++ " or " ++
             output fmt (T.Bid 5 T.Diamonds) ++ "."
      in
        situation "3nt" action B.b1D3N explanation
  in
    smpWrapN $ return sit


topic :: Topic
topic = Topic "responses to SMP 1D openings" "smp1d" situations
  where
    situations = wrap [ twoMinor6M
                      , oneMajor
                      , reverseFlannery
                      , wrap [weakMinors54, weakMinors55]
                      , wrap [notrump1, notrump2, notrump3]
                      ]
