module Topics.StandardModernPrecision.Mafia(topic) where

import Output(output)
import Topic(Topic(..), wrap, Situations)
import Auction({-withholdBid, forbid, makePass, maxSuitLength, -}
               Action, balancedHand, pointRange)
import Situation(situation)--, base, (<~))
--import CommonBids(cannotPreempt)
import qualified Terminology as T
import qualified Topics.StandardModernPrecision.Bids as B


startOfMafia :: Action
startOfMafia = do
    B.firstSeatOpener
    B.b1C
    B.oppsPass
    B.b1C1D
    B.oppsPass

oneNotrump :: Situations
oneNotrump = let
    action = do
        startOfMafia
        balancedHand
        pointRange 17 18
    explanation fmt =
        "With 17-18 HCP and a balanced hand, rebid " ++
        output fmt (T.Bid 1 T.Notrump) ++ ". Even if you have a 5-card major,\
      \ the comfort of knowing that systems are on is preferable."
  in
    B.smpWrapS $ situation "1N" action (T.Bid 1 T.Notrump) explanation


topic :: Topic
topic = Topic "SMP immediate responses to 1C" "SMP1C" situations
  where
    situations = wrap [ oneNotrump
{-
                      , wrap [oneMajorMinor, oneMajor]
                      , wrap [twoMinorSingle, twoMinorMinors]
                      , twoNotrump
                      , jumpBid
-}
                      ]
