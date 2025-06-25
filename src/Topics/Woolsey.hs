module Topics.Woolsey(topic) where

import Action(Action)
import qualified Bids.Woolsey as W
import CommonBids(setOpener, strong1NT, weak1NT)
import EDSL(pointRange, minSuitLength, maxSuitLength, makePass, forEach)
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
    wrap $ return sit <~ [weak1NT, strong1NT]
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
    wrap $ return sit <~ [weak1NT, strong1NT]
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
    wrap $ return sit <~ [weak1NT, strong1NT]
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
    wrap $ return sit <~ [weak1NT, strong1NT]
                      <~ [T.West, T.South, T.East]
                      <~ T.allVulnerabilities


twoDiamondsWithHearts :: Situations
twoDiamondsWithHearts= let
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
    wrap $ return sit <~ [weak1NT, strong1NT]
                      <~ [T.West, T.North, T.East]
                      <~ T.allVulnerabilities


twoDiamondsWithSpades :: Situations
twoDiamondsWithSpades= let
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
    wrap $ return sit <~ [weak1NT, strong1NT]
                      <~ [T.West, T.North, T.East]
                      <~ T.allVulnerabilities


-- overcall 2M
-- partner overcalls 2M, you prefer minor
-- overcall 2M, partner prefers minor
-- overcall 2N with both minors
-- partner overcalls 2N, pick better minor
-- overcall 3m
-- double is penalty against weak notrump
-- partner makes penalty double against weak notrump
-- double is conventional against strong notrump
-- partner makes conventional double, prefer minor/major
-- double, partner prefers minor, PoC
-- double, partner prefers major


topic :: Topic
topic = makeTopic "Woolsey (Multi-Landy)" "wool" situations
  where
    situations = wrap [ twoClubs
                      , twoClubsResponse
                      , twoDiamonds
                      , twoDiamondsResponse
                      , wrap [twoDiamondsWithHearts, twoDiamondsWithSpades]
                      ]
