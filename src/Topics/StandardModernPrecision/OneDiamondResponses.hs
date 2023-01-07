module Topics.StandardModernPrecision.OneDiamondResponses(topic) where

import Output(output, Punct(..))
import Topic(Topic(..), wrap, Situations)
import Auction(withholdBid, maxSuitLength, minSuitLength, pointRange,
               displayLastCall, alternatives)
import Situation(situation, (<~))
import qualified Terminology as T
import Topics.StandardModernPrecision.BasicBids(oppsPass, b1D, smpWrapN)
import qualified Topics.StandardModernPrecision.Bids1D as B


oneMajor :: Situations
oneMajor = let
    sit bid = let
        action = do
            b1D
            oppsPass
            withholdBid bid
        explanation fmt =
            "Let's start with a natural " ++ displayLastCall fmt bid ++ "\
           \ bid, and see where things go from there."
      in
        situation "1M" action bid explanation
  in
    smpWrapN $ return sit <~ [B.b1D1H, B.b1D1S]


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
        situation "6m4M" action bid explanation
  in
    smpWrapN $ return sit <~ [(T.Clubs, B.b1D2C), (T.Diamonds, B.b1D2D)]
                          <~ [T.Hearts, T.Spades]


twoMinorLongInv :: Situations
twoMinorLongInv = let
    sit (minor, bid) = let
        action = do
            b1D
            oppsPass
            minSuitLength minor 6
            maxSuitLength T.Hearts 3
            maxSuitLength T.Spades 3
            pointRange 11 13
            withholdBid bid
        explanation _ =
            "With invitational strength and a 6-card minor, bid naturally,\
           \ planning to rebid your suit at the 3 level if partner doesn't\
           \ show a maximum hand."
      in
        situation "6mw" action bid explanation
  in
    smpWrapN $ return sit <~ [(T.Clubs, B.b1D2C), (T.Diamonds, B.b1D2D)]


twoMinorBothInv :: Situations
twoMinorBothInv = let
    sit = let
        action = do
            b1D
            oppsPass
            minSuitLength T.Clubs 4
            minSuitLength T.Diamonds 4
            alternatives . map (`minSuitLength` 5) $ T.minorSuits
            maxSuitLength T.Hearts 3
            maxSuitLength T.Spades 3
            pointRange 11 13
            withholdBid B.b1D2D
        explanation fmt =
            "With invitational strength and both minors, start with " ++
            output fmt (T.Bid 2 T.Diamonds) ++ ", planning to rebid " ++
            output fmt (T.Bid 3 T.Clubs) ++ " to give partner a choice of\
           \ minors. Be prepared to go to game if partner shows a maximum, but\
           \ they're more likely to have a minimum which will pass or correct\
           \ at the 3 level."
      in
        situation "9m" action B.b1D2D explanation
  in
    smpWrapN $ return sit


reverseFlannery :: Situations
reverseFlannery = let
    sit (bid, isInvite) = let
        action = do
            b1D
            oppsPass
            withholdBid bid
        explanation fmt =
            "With 5 spades, 4 or 5 hearts, and " ++
            (if isInvite then "" else "less than ") ++
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
            b1D
            oppsPass
            withholdBid B.b1D1N
        explanation fmt =
            "With a balanced hand, no 4-card major, and less than invitational\
           \ strength, bid " ++ output fmt (T.Bid 1 T.Notrump) ++ ". Partner\
           \ will likely pass, but could bid 2 of a major with 6-5 and a\
           \ maximum, which you can pass or correct."
      in
        situation "1N" action B.b1D1N explanation
  in
    smpWrapN $ return sit


notrump2 :: Situations
notrump2 = let
    sit = let
        action = do
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
        situation "2N" action B.b1D2N explanation
  in
    smpWrapN $ return sit


notrump3 :: Situations
notrump3 = let
    sit = let
        action = do
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
        situation "3N" action B.b1D3N explanation
  in
    smpWrapN $ return sit


invertedMinors :: Situations
invertedMinors = let
    sit = let
        action = do
            b1D
            oppsPass
            withholdBid B.b1D3D
        explanation fmt =
            "With less than invitational strength, no 4-card major, but a long\
           \ diamond suit, jump to " ++ displayLastCall fmt B.b1D3D ++ ", like\
           \ an inverted minor. Note that you need 6+ cards in the suit to do\
           \ this, since opener might only have a doubleton."
      in
        situation "3d" action B.b1D3D explanation
  in
    smpWrapN $ return sit


preempt3M :: Situations
preempt3M = let
    sit bid = let
        action = do
            b1D
            oppsPass
            withholdBid bid
        explanation _ =
            "With a weak hand and a 7-card major, jump to 3 of that suit.\
           \ Opener should pretend that you opened with a pre-empt: they will\
           \ likely pass, even if we don't have a great fit, but can continue\
           \ the pre-empt with a fit if the opponents try to enter the auction."
      in
        situation "3M" action bid explanation
  in
    smpWrapN $ return sit <~ [B.b1D3H, B.b1D3S]


-- TODO: uncomment this and get it right when you're more confident of the
-- definition of a 4D response.
{-
preempt4D :: Situations
preempt4D = let
    sit = let
        action = do
            b1D
            oppsPass
            withholdBid B.b1D4D
        explanation _ =
            "With a weak hand and 8-card support for partner's diamonds, jump\
           \ to 4 of that suit. Opener has at least a doubleton, so on the LoTT\
           \ you're probably safe to the 4 level."
        -- TODO: not sure that's right. What if you've got a running diamond
        -- suit between the two of you and you belong in a gambling-like 3N?
      in
        situation "4D" action bid explanation
  in
    smpWrapN $ return sit
-}


majorGame :: Situations
majorGame = let
    sit bid = let
        action = do
            b1D
            oppsPass
            withholdBid bid
        explanation _ =
            "With an 8-card major, jump to game. This bid has a very wide\
           \ point range, which makes it difficult for the opponents because\
           \ maybe you're pre-empting and maybe you're game forcing with no\
           \ interest in slam. Regardless, game in your major is the right\
           \ place to stop, so bid it immediately without giving the opponents\
           \ a chance to jump into the auction."
      in
        situation "4M" action bid explanation
  in
    smpWrapN $ return sit <~ [B.b1D4H, B.b1D4S]



topic :: Topic
topic = Topic "SMP immediate responses to 1D openings" "smp1d" situations
  where
    situations = wrap [ wrap [twoMinor6M, twoMinorLongInv, twoMinorBothInv]
                      , oneMajor
                      , reverseFlannery
                      , wrap [weakMinors54, weakMinors55]
                      , wrap [notrump1, notrump2, notrump3]
                      -- TODO: Add preempt4D to this line when it's implemented.
                      , wrap [invertedMinors, preempt3M, majorGame]
                      ]
