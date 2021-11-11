module Topics.StandardModernPrecision.Mafia(topic) where

import Output(output)
import Topic(Topic(..), wrap, Situations)
import Auction(withholdBid, forbid, {-makePass, maxSuitLength, -} minSuitLength, suitLength,
               {-Action,-} balancedHand, pointRange)
import Situation(situation)--, base, (<~))
--import CommonBids(cannotPreempt)
import qualified Terminology as T
import qualified Topics.StandardModernPrecision.Bids as B


oneNotrump :: Situations
oneNotrump = let
    action = do
        B.startOfMafia
        balancedHand
        pointRange 17 18
    explanation fmt =
        "With 17-18 HCP and a balanced hand, rebid " ++
        output fmt (T.Bid 1 T.Notrump) ++ ". Even if you have a 5-card major,\
      \ the comfort of knowing that systems are on is preferable."
  in
    B.smpWrapS $ situation "1N" action (T.Bid 1 T.Notrump) explanation


oneHeart :: Situations
oneHeart = let
    sit = let
        action = do
            B.startOfMafia
            forbid balancedHand
            withholdBid B.b1C1D1H
        explanation _ =
            "With an unbalanced hand that isn't game forcing, bid a 4-card\
           \ heart suit if you have one."
      in
        situation "1H" action (T.Bid 1 T.Hearts) explanation
  in
    B.smpWrapS sit


oneHeartMinor :: Situations
oneHeartMinor = let
    sit minorSuit = let
        action = do
            B.startOfMafia
            forbid balancedHand
            minSuitLength minorSuit 5
            suitLength T.Hearts 5
            withholdBid B.b1C1D1H
        explanation _ =
            "With an unbalanced hand that isn't game forcing, bid a 4-card\
           \ heart suit if you have one. This holds even if you've got a\
           \ longer minor."
      in
        situation "1Hm" action (T.Bid 1 T.Hearts) explanation
  in
    B.smpWrapS $ sit T.Clubs


topic :: Topic
topic = Topic "SMP immediate responses to 1C" "SMP1C" situations
  where
    situations = wrap [ oneNotrump
                      , wrap [oneHeartMinor, oneHeart]--, oneSpadeMinor, oneSpade]
{-
                      , wrap [twoMinorSingle, twoMinorMinors]
                      , twoNotrump
                      , jumpBid
-}
                      ]
