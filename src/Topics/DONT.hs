module Topics.DONT(topic) where

import Action(Action)
import qualified Bids.DONT as B
import CommonBids(setOpener)
import EDSL(pointRange, minSuitLength, maxSuitLength, makePass, forEach)
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


doubleNoSpades :: Situations
doubleNoSpades = let
    sit = let
        action = do
            setOpener T.East
            B.b1N
            maxSuitLength T.Spades 5
        explanation =
            "We've got a single-suited hand. Double to show this. Partner " .+
            "will relay to " .+ T.Bid 2 T.Clubs .+ ", which we can pass or " .+
            "correct to whatever our suit is."
        in situation "dbl" action B.b1NoX explanation
  in
    -- It is tremendously unlikely that you'd be a passed hand and want to make
    -- this bid: you probably should have opened the bidding (either at the 1
    -- level or bidding a weak 2). So, make sure we're an unpassed hand.
    wrap $ return sit <~ T.allVulnerabilities <~ [T.West, T.North, T.East]


doubleSpades :: Situations
doubleSpades = let
    sit = let
        action = do
            setOpener T.East
            B.b1N
            minSuitLength T.Spades 6
        explanation =
            "We've got a single-suited hand with spades, and a little " .+
            "more than the bare minimum to bid over LHO's notrump. Double " .+
            "to show a single-suited hand, and when partner finds out our " .+
            "suit is spades, they'll know we're not a minimum because we " .+
            "didn't merely bid " .+ T.Bid 2 T.Spades .+ " right away."
        in situation "dblS" action B.b1NoX explanation
  in
    wrap $ return sit <~ T.allVulnerabilities <~ [T.West, T.North, T.East]


spades :: Situations
spades = let
    sit = let
        action = do
            setOpener T.East
            B.b1N
        explanation =
            "We've got a single-suited hand with spades, and just the bare " .+
            "minimum to interfere over the opponents' notrump auction. " .+
            "Instead of doubling to show a single-suited hand, bid the " .+
            "spades naturally."
        in situation "2S" action B.b1No2S explanation
  in
    wrap $ return sit <~ T.allVulnerabilities <~ [T.West, T.North, T.East]


poc2C :: Situations
poc2C = let
    sit = let
        action = do
            setOpener T.West
            B.b1N
            B.b1NoX
            responderCannotBid
        explanation =
            "Partner has shown a single-suited hand. Bid " .+ B.b1NoX2C .+
            "to ask partner what their suit is: they will pass with clubs " .+
            "and bid correct to their suit otherwise."
        in situation "poc2C" action B.b1NoX2C explanation
  in
    -- It is tremendously unlikely that West would open in pass-out seat and
    -- North will want to come in over that with a single-suited hand: North
    -- should probably have opened a weak two. So, ensure they're an unpassed
    -- hand.
    wrap $ return sit <~ T.allVulnerabilities <~ [T.West, T.South, T.East]


twoSuited :: Situations
twoSuited = let
    sit bid = let
        action = do
            setOpener T.East
            B.b1N
        explanation =
            "We've got a two-suited hand. Bid our lower suit: partner " .+
            "can pass if we've got a fit, or else they'll bid the " .+
            "next suit up, which we can pass or correct to our second suit."
        in situation "2suit" action bid explanation
  in
    wrap $ return sit <~ [B.b1No2C, B.b1No2D, B.b1No2H]
                      <~ T.allVulnerabilities
                      <~ [T.West, T.North, T.East]


preempt :: Situations
preempt = let
    sit bid = let
        action = do
            setOpener T.East
            B.b1N
        explanation =
            "We've got a single-suited hand. However, it's such a long " .+
            "suit that we should pre-empt at the 3 level, rather than " .+
            "doubling with the intention of signing off at the 2 level. " .+
            "This makes it harder for the opponents to recover from our " .+
            "interference."
        in situation "pre" action bid explanation
  in
    -- If we're going to pre-empt, we must not have had a chance to open the
    -- bidding ourselves (since we would have already pre-empted, given the
    -- chance).
    wrap $ return sit <~ [B.b1No3C, B.b1No3D, B.b1No3H, B.b1No3S]
                      <~ T.allVulnerabilities
                      <~ [T.West, T.North, T.East]


findSecondSuit :: Situations
findSecondSuit = let
    sit (overcall, advance) = let
        action = do
            setOpener T.West
            B.b1N
            _ <- overcall
            responderCannotBid
        explanation =
            "Partner has shown a two-suited hand. However, we don't have " .+
            "support for their lower suit. Relay to the next suit up: " .+
            "they'll pass or correct to their second suit, which is likely " .+
            "to have a better fit with us than the first one did."
        in situation "2ndS" action advance explanation
  in
    wrap $ return sit <~ [(B.b1No2C, B.b1No2C2D), (B.b1No2D, B.b1No2D2H)]
                      <~ T.allVulnerabilities
                      <~ [T.West, T.South, T.East]


breakRelayDouble :: Situations
breakRelayDouble = let
    sit advance = let
        action = do
            setOpener T.West
            B.b1N
            _ <- B.b1NoX
            responderCannotBid
        explanation =
            "Partner has shown a single-suited hand. Normally, we'd relay " .+
            T.Bid 2 T.Clubs .+ " to ask what their suit is, but we also " .+
            "have a single-suited hand. Bid our suit instead, and partner " .+
            "can pass with tolerance for our suit, or bid their own suit " .+
            "without tolerance for ours. This gives us 2 chances to find " .+
            "a good contract."
        in situation "brX" action advance explanation
  in
    wrap $ return sit <~ [B.b1NoX2D, B.b1NoX2H]
                      <~ T.allVulnerabilities
                      <~ [T.West, T.South, T.East]


topic :: Topic
topic = makeTopic "DONT over strong notrump" "MW1N" situations
  where
    situations = wrap [ doubleNoSpades
                      , twoSuited
                      , poc2C
                      , findSecondSuit
                      , wrap [preempt, doubleSpades, spades]
                      , wrap [breakRelayDouble]
                      ]
