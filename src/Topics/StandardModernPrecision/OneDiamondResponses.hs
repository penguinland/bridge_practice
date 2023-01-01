module Topics.StandardModernPrecision.OneDiamondResponses(topic) where

import Output(output)
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


topic :: Topic
topic = Topic "responses to SMP 1D openings" "smp1d" situations
  where
    situations = wrap [ twoMinor6M
                      , oneMajor
                      , reverseFlannery
                      ]
