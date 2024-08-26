module Topics.Cappelletti(topic) where

import qualified Bids.Cappelletti as B
import CommonBids(setOpener)
import Output((.+))
import Situation(situation, (<~))
import qualified Terminology as T
import Topic(Topic, wrap, Situations, makeTopic)


{-
responderCannotBid :: Action
responderCannotBid = do
    pointRange 0 7                           -- No jumping to 3N, etc.
    forEach T.majorSuits (`maxSuitLength` 4) -- No transfers
    minSuitLength T.Clubs 2                  -- Avoid Garbage Stayman
    makePass
-}


penaltyDouble :: Situations
penaltyDouble = let
    sit = let
        action = do
            setOpener T.East
            B.b1N
        explanation =
            "RHO has opened a weak notrump, and we got a fair amount " .+
            "of strength behind them. Let's make a penalty double."
        in situation "majpoc" action B.b1NoX explanation
  in
    -- Ensure we're not dealer: we'd have opened the bidding.
    wrap $ return sit <~ T.allVulnerabilities <~ [T.West, T.North, T.East]


topic :: Topic
topic = makeTopic "Cappelletti over weak notrump" "Cap1N" situations
  where
    situations = wrap [ penaltyDouble
                      ]
