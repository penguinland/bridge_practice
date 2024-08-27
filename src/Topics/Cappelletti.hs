module Topics.Cappelletti(topic) where

import Action(Action)
import qualified Bids.Cappelletti as B
import CommonBids(setOpener)
import EDSL(pointRange, forEach, minSuitLength, maxSuitLength, makePass)
import Output((.+))
import Situation(situation, (<~))
import qualified Terminology as T
import Topic(Topic, wrap, Situations, makeTopic)


responderCannotBid :: Action
responderCannotBid = do
    pointRange 0 7                           -- No jumping to 3N, etc.
    forEach T.majorSuits (`maxSuitLength` 4) -- No transfers
    minSuitLength T.Clubs 2                  -- Avoid Garbage Stayman
    makePass


penaltyDouble :: Situations
penaltyDouble = let
    sit = let
        action = do
            setOpener T.East
            B.b1N
        explanation =
            "RHO has opened a weak notrump, and we've got a fair amount " .+
            "of strength behind them. Let's make a penalty double."
        in situation "pdbl" action B.b1NoX explanation
  in
    -- Ensure we're not dealer: we'd have opened the bidding.
    wrap $ return sit <~ T.allVulnerabilities <~ [T.West, T.North, T.East]


singleSuited :: Situations
singleSuited = let
    sit = let
        action = do
            setOpener T.East
            B.b1N
        explanation =
            "RHO has opened a weak notrump. Bid " .+ B.b1No2C .+ " to show " .+
            "a single-suited hand. Partner will relay to " .+
            T.Bid 2 T.Diamonds .+ ", which you can pass or correct to your " .+
            "suit."
        in situation "sing" action B.b1No2C explanation
  in
    -- Although it's possible to make this bid as a passed hand, it is extremely
    -- unlikely. As a performance optimization, let's just practice this when
    -- we're an unpassed hand.
    wrap $ return sit <~ T.allVulnerabilities <~ [T.West, T.North, T.East]


singleSuitedRelay :: Situations
singleSuitedRelay = let
    sit = let
        action = do
            setOpener T.West
            B.b1N
            B.b1No2C
            responderCannotBid
        explanation =
            "LHO has opened a weak notrump, and partner showed a " .+
            "single-suited hand. Relay to " .+ B.b1No2C2D .+ " to ask " .+
            "what partner's suit is: they will pass or correct to it."
        in situation "2dRel" action B.b1No2C2D explanation
  in
    -- We're going to continue with the optimization that the Cappelletti bidder
    -- should be an unpassed hand.
    wrap $ return sit <~ T.allVulnerabilities <~ [T.West, T.South, T.East]


bothMajors :: Situations
bothMajors = let
    sit = let
        action = do
            setOpener T.East
            B.b1N
        explanation =
            "RHO has opened a weak notrump. We have both majors, and can " .+
            "show that by bidding " .+ B.b1No2D .+ ". Partner will bid " .+
            "their favorite major, and we'll play there."
        in situation "bmaj" action B.b1No2D explanation
  in
    wrap $ return sit <~ T.allVulnerabilities <~ [T.West, T.North, T.East]


majorMinor :: Situations
majorMinor = let
    sit bid = let
        action = do
            setOpener T.East
            B.b1N
        explanation =
            "RHO has opened a weak notrump. We have a two-suited hand with " .+
            "a major and a minor. Bid our major suit. If partner wants to " .+
            "learn our minor, they can bid " .+ T.Bid 2 T.Notrump .+ " to " .+
            "prompt us to bid it."
        in situation "Mm" action bid explanation
  in
    wrap $ return sit <~ [B.b1No2H, B.b1No2S]
                      <~ T.allVulnerabilities
                      <~ [T.West, T.North, T.East]


bothMinors :: Situations
bothMinors = let
    sit = let
        action = do
            setOpener T.East
            B.b1N
        explanation =
            "RHO has opened a weak notrump. We have both minors, and can " .+
            "show that by bidding an Unusual-like " .+ B.b1No2N .+ ". " .+
            "Partner will bid their favorite minor, and we'll play there."
        in situation "bmin" action B.b1No2N explanation
  in
    wrap $ return sit <~ T.allVulnerabilities <~ [T.West, T.North, T.East]


topic :: Topic
topic = makeTopic "Cappelletti over weak notrump" "Cap1N" situations
  where
    situations = wrap [ penaltyDouble
                      , singleSuited
                      , singleSuitedRelay
                      , bothMajors
                      , majorMinor
                      , bothMinors
                      ]
