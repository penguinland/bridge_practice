module Topics.Meckwell(topic) where

import Action(Action)
import qualified Bids.Meckwell as B
import CommonBids(setOpener)
import EDSL(pointRange, minSuitLength, maxSuitLength, makePass, forEach,
            nameAction)
import Output((.+))
import Situation(situation, (<~))
import qualified Terminology as T
import Topic(Topic, wrap, Situations, makeTopic)


responderCannotBid :: Action
responderCannotBid = nameAction "responder_pass" $ do
    pointRange 0 7                           -- No jumping to 3N, etc.
    forEach T.majorSuits (`maxSuitLength` 4) -- No transfers
    minSuitLength T.Clubs 2                  -- Avoid Garbage Stayman
    makePass


majorSuit :: Situations
majorSuit = let
    sit (suit, bid) = let
        action = do
            setOpener T.East
            B.b1N
        explanation =
            "With a single-suited hand with " .+ show suit .+ ", make a " .+
            "natural overcall."
        in situation "maj" action bid explanation
  in
    -- Ensure we're not dealer: it's too rare to find a hand where we'd want to
    -- overcall after 1N but not open the bidding ourselves.
    wrap $ return sit <~ [(T.Hearts, B.b1No2H), (T.Spades, B.b1No2S)]
                      <~ [T.West, T.North, T.East]
                      <~ T.allVulnerabilities


minorAndMajor :: Situations
minorAndMajor = let
    sit (suit, bid) = let
        action = do
            setOpener T.East
            B.b1N
        explanation =
            "With a two-suited hand with " .+ show suit .+ " and a major, " .+
            "start by bidding the minor. Partner will pass with a fit, or " .+
            "bid " .+ T.Bid 2 T.Hearts .+ " with the majors, which we can " .+
            "pass or correct to " .+ T.Bid 2 T.Spades .+ "."
        in situation "min" action bid explanation
  in
    -- Ensure we're not dealer: it's too rare to find a hand where we'd want to
    -- overcall after 1N but not open the bidding ourselves.
    wrap $ return sit <~ [(T.Clubs, B.b1No2C), (T.Diamonds, B.b1No2D)]
                      <~ [T.West, T.North, T.East]
                      <~ T.allVulnerabilities


double :: Situations
double = let
    sit = let
        action = do
            setOpener T.East
            B.b1N
        explanation =
            "A double shows either a hand with 1 long minor, or else a " .+
            "hand with both majors. Partner will relay to " .+
            T.Bid 2 T.Clubs .+ ", which we can either pass or correct. " .+
            "When we have both majors, correct to " .+ T.Bid 2 T.Hearts .+
            ", which partner can then either pass or correct to spades."
        in situation "dbl" action B.b1NoX explanation
  in
    -- Ensure we're not dealer: it's too rare to find a hand where we'd want to
    -- overcall after 1N but not open the bidding ourselves.
    wrap $ return sit <~ [T.West, T.North, T.East] <~ T.allVulnerabilities


relayAfterDouble :: Situations
relayAfterDouble = let
    sit = let
        action = do
            setOpener T.West
            B.b1N
            B.b1NoX
            responderCannotBid
        explanation =
            "Partner has either one long minor or both majors. No matter " .+
            "what we have, relay to " .+ T.Bid 2 T.Clubs .+ " to find out " .+
            "what kind of hand partner has. Do this even if we have " .+
            "absolutely no strength: leaving the opponents in " .+
            T.Bid 1 T.Notrump .+ " doubled is never the right choice."
        in situation "Xrelay" action B.b1NoX2C explanation
  in
    -- Ensure partner is not dealer.
    wrap $ return sit <~ [T.West, T.South, T.East] <~ T.allVulnerabilities


doubleBothMajors :: Situations
doubleBothMajors = let
    sit = let
        action = do
            setOpener T.East
            B.b1N
            B.b1NoX
            responderCannotBid
            B.b1NoX2C
            makePass
        explanation =
            "Now that partner has relayed to ask what our hand shape is, " .+
            "bid " .+ T.Bid 2 T.Hearts .+ " to show both majors. Partner " .+
            "will pass or correct to spades."
        in situation "bmaj" action B.b1NoX2C2H explanation
  in
    -- Ensure we're not dealer.
    wrap $ return sit <~ [T.West, T.North, T.East] <~ T.allVulnerabilities


bothMinors :: Situations
bothMinors = let
    sit = let
        action = do
            setOpener T.East
            B.b1N
        explanation =
            "With both minors, bid an unusual-like " .+ T.Bid 2 T.Notrump .+
            ". Partner will bid their favorite minor, and we'll pass."
        in situation "bmin" action B.b1No2N explanation
  in
    -- Ensure we're not dealer.
    wrap $ return sit <~ [T.West, T.North, T.East] <~ T.allVulnerabilities


findMajor :: Situations
findMajor = let
    sit (bid, response, suit) = let
        action = do
            setOpener T.West
            B.b1N
            _ <- bid  -- Make the compiler happy
            responderCannotBid
        explanation =
            "Partner has shown a two-suited hand with " .+ show suit .+
            " and a major. We don't have a " .+ (init . show $ suit) .+
            " fit, so bid " .+ T.Bid 2 T.Hearts .+ ", which partner can " .+
            "pass or correct. It's possible we'll end up in only a 7-card " .+
            "fit, if our hands are particularly mismatched."
        in situation "majpoc" action response explanation
  in
    -- Ensure partner is not dealer.
    wrap $ return sit <~ [ (B.b1No2C, B.b1No2C2H, T.Clubs)
                         , (B.b1No2D, B.b1No2D2H, T.Diamonds)
                         ]
                      <~ [T.West, T.South, T.East]
                      <~ T.allVulnerabilities


topic :: Topic
topic = makeTopic "Meckwell over strong notrump" "MW1N" situations
  where
    situations = wrap [ majorSuit
                      , minorAndMajor
                      , double
                      , wrap [relayAfterDouble, doubleBothMajors]
                      , bothMinors
                      , findMajor
                      ]
