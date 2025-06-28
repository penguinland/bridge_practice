module Topics.Woolsey(topic) where

import Action(Action)
import qualified Bids.Woolsey as W
import CommonBids(setOpener, strong1NT, weak1NT)
import EDSL(pointRange, minSuitLength, maxSuitLength, makePass, forEach, forbid)
import Output((.+), Punct(..))
import Situation(situation, (<~))
import qualified Terminology as T
import Topic(Topic, wrap, Situations, makeTopic)


-- TODO: maybe remove this
responderCannotBid :: Action
responderCannotBid = do
    pointRange 0 7                           -- No jumping to 3N, etc.
    forEach T.majorSuits (`maxSuitLength` 4) -- No transfers
    minSuitLength T.Clubs 2                  -- Avoid Garbage Stayman
    makePass


-- Syntactic sugar: prefer a penalty double against weak notrump when available,
-- so use this when giving the options of "opener bids a strong 1N, or they bid
-- a weak 1N and we can't make a penalty double."
eitherNotrumpNoDouble :: [Action]
eitherNotrumpNoDouble = [strong1NT, weak1NT >> forbid W.b1NweaoX]


twoClubs :: Situations
twoClubs = let
    sit openingBid = let
        action = do
            setOpener T.East
            openingBid
        explanation =
            "Our opponent has opened " .+ T.Bid 1 T.Notrump .+ ". With " .+
            "both majors, overcall " .+ T.Bid 2 T.Clubs .+ ". This is the " .+
            OpenQuote .+ "Landy" .+ CloseQuote .+ " part of Multi-Landy."
        in situation "2C" action W.b1No2C explanation
  in
    -- Ensure we're not dealer: it's too rare to find a hand where we'd want to
    -- overcall after 1N but not open the bidding ourselves.
    wrap $ return sit <~ eitherNotrumpNoDouble
                      <~ [T.West, T.North, T.East]
                      <~ T.allVulnerabilities


twoClubsResponse :: Situations
twoClubsResponse = let
    sit openingBid ourBid = let
        action = do
            setOpener T.West
            _ <- openingBid
            W.b1No2C
            responderCannotBid
        explanation =
            "Partner has shown both majors. Bid our favorite major, and " .+
            "we'll play there."
        in situation "2CR" action ourBid explanation
  in
    -- Ensure partner isn't dealer: it's too rare to find a hand where they'd
    -- want to overcall after 1N but not open the bidding themselves.
    wrap $ return sit <~ eitherNotrumpNoDouble
                      <~ [W.b1No2C2H, W.b1No2C2S]
                      <~ [T.West, T.South, T.East]
                      <~ T.allVulnerabilities


twoDiamonds :: Situations
twoDiamonds = let
    sit openingBid = let
        action = do
            setOpener T.East
            openingBid
        explanation =
            "RHO has opened " .+ T.Bid 1 T.Notrump .+ ", and we have a " .+
            "single-suited hand with a major. Bid " .+ W.b1No2D .+ " to " .+
            "show this. This bid is the " .+ OpenQuote .+ "Multi" .+
            CloseQuote .+ " part of Multi-Landy."
        in situation "2D" action W.b1No2D explanation
  in
    wrap $ return sit <~ eitherNotrumpNoDouble
                      <~ [T.West, T.North, T.East]
                      <~ T.allVulnerabilities


twoDiamondsResponse :: Situations
twoDiamondsResponse = let
    sit openingBid = let
        action = do
            setOpener T.West
            _ <- openingBid
            W.b1No2D
            responderCannotBid
        explanation =
            "Partner has shown a major. Bid " .+ T.Bid 2 T.Hearts .+
            ", pass or correct."
        in situation "2DR" action W.b1No2D2H explanation
  in
    wrap $ return sit <~ eitherNotrumpNoDouble
                      <~ [T.West, T.South, T.East]
                      <~ T.allVulnerabilities


twoDiamondsWithHearts :: Situations
twoDiamondsWithHearts = let
    sit openingBid = let
        action = do
            setOpener T.East
            _ <- openingBid
            W.b1No2D
            _ <- responderCannotBid
            W.b1No2D2H
            makePass
        explanation =
            "RHO has opened " .+ T.Bid 1 T.Notrump .+ ", and we showed a " .+
            "single-suited hand with a major. Partner has bid hearts, pass " .+
            "or correct. It's time to pass."
        in situation "2DH" action W.b1No2D2HP explanation
  in
    wrap $ return sit <~ eitherNotrumpNoDouble
                      <~ [T.West, T.North, T.East]
                      <~ T.allVulnerabilities


twoDiamondsWithSpades :: Situations
twoDiamondsWithSpades = let
    sit openingBid = let
        action = do
            setOpener T.East
            _ <- openingBid
            W.b1No2D
            _ <- responderCannotBid
            W.b1No2D2H
            makePass
        explanation =
            "RHO has opened " .+ T.Bid 1 T.Notrump .+ ", and we showed a " .+
            "single-suited hand with a major. Partner has bid hearts, pass " .+
            "or correct. Correct to spades."
        in situation "2DS" action W.b1No2D2H2S explanation
  in
    wrap $ return sit <~ eitherNotrumpNoDouble
                      <~ [T.West, T.North, T.East]
                      <~ T.allVulnerabilities


twoMajor :: Situations
twoMajor = let
    sit openingBid overcall = let
        action = do
            setOpener T.East
            openingBid
        explanation =
            "RHO has opened " .+ T.Bid 1 T.Notrump .+ ", and we have a " .+
            "two-suited hand with a 5-card major and a minor. Bid the " .+
            "major to show this shape."
        in situation "2M" action overcall explanation
  in
    wrap $ return sit <~ eitherNotrumpNoDouble
                      <~ [W.b1No2H, W.b1No2S]
                      <~ [T.West, T.North, T.East]
                      <~ T.allVulnerabilities


twoMajorResponse :: Situations
twoMajorResponse = let
    sit openingBid (overcall, response) = let
        action = do
            setOpener T.West
            _ <- openingBid
            _ <- overcall
            responderCannotBid
        explanation =
            "Partner has shown a two-suited hand with that major and a " .+
            "minor. We prefer the minor, so bid " .+ response .+ " to " .+
            "prompt them to bid it."
        in situation "2MR" action response explanation
  in
    wrap $ return sit <~ eitherNotrumpNoDouble
                      <~ [(W.b1No2H, W.b1No2H2N), (W.b1No2S, W.b1No2S2N)]
                      <~ [T.West, T.South, T.East]
                      <~ T.allVulnerabilities


twoMajorWithMinor :: Situations
twoMajorWithMinor = let
    sit openingBid (overcall, response, rebid) = let
        action = do
            setOpener T.East
            _ <- openingBid
            _ <- overcall
            _ <- responderCannotBid
            _ <- response
            makePass
        explanation =
            "RHO opened " .+ T.Bid 1 T.Notrump .+ ", and we showed a " .+
            "two-suited hand with a minor and a 5-card major. Partner has " .+
            "asked us to bid our minor."
        in situation "2Mm" action rebid explanation
  in
    wrap $ return sit <~ eitherNotrumpNoDouble
                      <~ [ (W.b1No2H, W.b1No2H2N, W.b1No2H2N3C)
                         , (W.b1No2S, W.b1No2H2N, W.b1No2H2N3D)
                         , (W.b1No2S, W.b1No2S2N, W.b1No2S2N3C)
                         , (W.b1No2S, W.b1No2S2N, W.b1No2S2N3D)]
                      <~ [T.West, T.North, T.East]
                      <~ T.allVulnerabilities


twoNotrump :: Situations
twoNotrump = let
    sit openingBid = let
        action = do
            setOpener T.East
            openingBid
        explanation =
            "RHO has opened " .+ T.Bid 1 T.Notrump .+ ", and we have a " .+
            "two-suited hand with both minors. Bid an unusual-like " .+
            W.b1No2N .+ ". Partner will then bid their favorite minor, and " .+
            "we'll play there."
        in situation "2N" action W.b1No2N explanation
  in
    wrap $ return sit <~ eitherNotrumpNoDouble
                      <~ [T.West, T.North, T.East]
                      <~ T.allVulnerabilities


twoNotrumpResponse :: Situations
twoNotrumpResponse = let
    sit openingBid response = let
        action = do
            setOpener T.West
            _ <- openingBid
            W.b1No2N
            responderCannotBid
        explanation =
            "Partner has shown a two-suited hand with both minors. Bid " .+
            "our favorite minor, and partner will have support for it."
        in situation "2NR" action response explanation
  in
    wrap $ return sit <~ eitherNotrumpNoDouble
                      <~ [W.b1No2N3C, W.b1No2N3D]
                      <~ [T.West, T.South, T.East]
                      <~ T.allVulnerabilities


threeMinor :: Situations
threeMinor = let
    sit openingBid overcall = let
        action = do
            setOpener T.East
            openingBid
        explanation =
            "RHO has opened " .+ T.Bid 1 T.Notrump .+ ", and we have one " .+
            "long minor. Bid naturally at the 3 level. Be a little more " .+
            "conservative when making this bid compared to others, because " .+
            "the 3 level can be uncomfortably high."
        in situation "3m" action overcall explanation
  in
    wrap $ return sit <~ eitherNotrumpNoDouble
                      <~ [W.b1No3C, W.b1No3D]
                      <~ [T.West, T.North, T.East]
                      <~ T.allVulnerabilities


penaltyDouble :: Situations
penaltyDouble = let
    sit = let
        action = do
            setOpener T.East
            weak1NT
        explanation =
            "RHO has opened a weak " .+ T.Bid 1 T.Notrump .+ ". Double " .+
            "should be penalty here: the opponents might not have a place " .+
            "to run to, and we can punish them for stepping out."
        in situation "wkX" action W.b1NweaoX explanation
  in
    wrap $ return sit <~ [T.West, T.North, T.East]
                      <~ T.allVulnerabilities


convDouble :: Situations
convDouble = let
    sit = let
        action = do
            setOpener T.East
            strong1NT
        explanation =
            "RHO has opened a strong " .+ T.Bid 1 T.Notrump .+ ", and we " .+
            "have a two-suited hand with a major and a minor. If the major " .+
            "were at least 5 cards long, we'd bid it, but it's only 4 " .+
            "cards, so we should double instead."
        in situation "strX" action W.b1NstroX explanation
  in
    wrap $ return sit <~ [T.West, T.North, T.East]
                      <~ T.allVulnerabilities


convDoubleResponseMinor :: Situations
convDoubleResponseMinor = let
    sit = let
        action = do
            setOpener T.West
            strong1NT
            W.b1NstroX
            responderCannotBid
        explanation =
            "Partner has shown a 4-card major and at least a 5-card minor. " .+
            "If they have our least favorite major and our least favorite " .+
            "minor, we prefer the minor. Bid " .+ W.b1NoX2C .+ ", pass or " .+
            "correct."
        in situation "sXMin" action W.b1NoX2C explanation
  in
    wrap $ return sit <~ [T.West, T.South, T.East]
                      <~ T.allVulnerabilities


convDoubleResponseMajor :: Situations
convDoubleResponseMajor = let
    sit = let
        action = do
            setOpener T.West
            strong1NT
            W.b1NstroX
            responderCannotBid
        explanation =
            "Partner has shown a 4-card major and at least a 5-card minor. " .+
            "If they have our least favorite major and our least favorite " .+
            "minor, we prefer the major. Bid " .+ W.b1NoX2D .+ ", asking " .+
            "partner to bid their major."
        in situation "sXMaj" action W.b1NoX2D explanation
  in
    wrap $ return sit <~ [T.West, T.South, T.East]
                      <~ T.allVulnerabilities


penaltyDoubleResponse :: Situations
penaltyDoubleResponse = let
    sit = let
        action = do
            setOpener T.West
            weak1NT
            W.b1NweaoX
            responderCannotBid
        explanation =
            "Partner has made a penalty double of the opponents' weak " .+
            "notrump bid. This is not conventional! Pass, don't pull it."
        in situation "wXP" action makePass explanation
  in
    wrap $ return sit <~ [T.West, T.South, T.East]
                      <~ T.allVulnerabilities


-- double, partner prefers minor, PoC
-- double, partner prefers major
-- Add Woolsey into the lebensohl topic. Consider adding a gotcha to Stayman


topic :: Topic
topic = makeTopic "Woolsey (Multi-Landy) over all notrump" "wool" situations
  where
    situations = wrap [ twoClubs
                      , twoClubsResponse
                      , twoDiamonds
                      , twoDiamondsResponse
                      , wrap [twoDiamondsWithHearts, twoDiamondsWithSpades]
                      , twoMajor
                      , twoMajorResponse
                      , twoMajorWithMinor
                      , twoNotrump
                      , twoNotrumpResponse
                      , threeMinor
                      , penaltyDouble
                      , convDouble
                      , wrap [ convDoubleResponseMinor
                             , convDoubleResponseMinor
                             , convDoubleResponseMajor
                             , convDoubleResponseMajor
                             , penaltyDoubleResponse
                             ]
                      ]
