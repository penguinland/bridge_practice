module Topics.ForcingOneNotrump(topic) where

import Output((.+), Punct(..))
import Topic(Topic, wrap, stdWrap, wrapVulDlr, Situations, makeTopic)
--import Auction(forbid)
import Situation(situation, (<~))
import qualified Terminology as T
import qualified Bids.ForcingOneNotrump as B
import CommonBids(setOpener, noInterference)


bid1NHearts :: Situations
bid1NHearts = let
    sit = let
        action = do
            setOpener T.North
            B.b1H
            noInterference T.Hearts
        explanation =
            "Partner has opened " .+ T.Bid 1 T.Hearts .+ ". You don't have " .+
            "a heart fit, don't have 4 spades, and aren't strong enough to " .+
            "force to game. Bid a forcing " .+ T.Bid 1 T.Notrump .+ ". " .+
            "Partner will rebid naturally, and you'll likely stop in some " .+
            "partscore."
      in
        situation "H1N" action B.b1H1N explanation
  in
    stdWrap sit


bid1NSpades :: Situations
bid1NSpades = let
    sit = let
        action = do
            setOpener T.North
            B.b1S
            noInterference T.Hearts
        explanation =
            "Partner has opened " .+ T.Bid 1 T.Spades .+ ". You don't have " .+
            "a spade fit and aren't strong enough to force to game. Bid a " .+
            "forcing " .+ T.Bid 1 T.Notrump .+ ". Partner will rebid " .+
            "naturally, and you'll likely stop in some partscore."
      in
        situation "S1N" action B.b1S1N explanation
  in
    stdWrap sit

rebid2N :: Situations
rebid2N = let
    sit (opening, response, suit) = let
        action = do
            setOpener T.South
            _ <- opening
            noInterference suit
            _ <- response
            noInterference suit
        explanation =
            "We opened our major, which partner hasn't (yet?) supported. " .+
            "With a balanced 18" .+ NDash .+ "19 count, now bid " .+
            T.Bid 2 T.Notrump .+ ". Partner now knows almost exactly what " .+
            "we have, and can place the final contract."
      in
        situation "rb2N" action B.b1M1N2N explanation
  in
    wrapVulDlr $ return sit <~ [ (B.b1H, B.b1H1N, T.Hearts)
                               , (B.b1S, B.b1S1N, T.Spades) ]


jumpShift :: Situations
jumpShift = let
    sit (opening, response, rebid, firstSuit) = let
        action = do
            setOpener T.South
            _ <- opening
            noInterference firstSuit
            _ <- response
            noInterference firstSuit
        explanation =
            "We opened our major, which partner hasn't (yet?) supported. " .+
            "With 18+ HCP and a two-suited hand whose second suit is lower " .+
            "than the first, jump in our second suit. This almost certainly " .+
            "is game forcing, even if partner is a minimum. They'll place " .+
            "the contract from here."
      in
        situation "js" action rebid explanation
  in
    wrapVulDlr $ return sit <~ [ (B.b1H, B.b1H1N, B.b1H1N3C, T.Hearts)
                               , (B.b1H, B.b1H1N, B.b1H1N3D, T.Hearts)
                               , (B.b1S, B.b1S1N, B.b1S1N3C, T.Spades)
                               , (B.b1S, B.b1S1N, B.b1S1N3D, T.Spades)
                               , (B.b1S, B.b1S1N, B.b1S1N3H, T.Spades)
                               ]


majorReverse :: Situations
majorReverse = let
    sit = let
        action = do
            setOpener T.South
            B.b1H
            noInterference T.Hearts
            B.b1H1N
            noInterference T.Hearts
        explanation =
            "With 5-4 in the majors and 17+ HCP, we opened " .+
            T.Bid 1 T.Hearts .+ ", and can now reverse to " .+
            T.Bid 2 T.Spades .+ ". We're strong enough that even if " .+
            "we don't have a fit and partner is a minimum, they can " .+
            "reluctantly sign off in some 7-card fit, but if they've got a " .+
            "non-minimum, they know to guide us into the right game."
      in
        situation "rev" action B.b1H1N2S explanation
  in
    stdWrap sit


jumpRebid :: Situations
jumpRebid = let
    sit (bid, response, rebid, suit) = let
        action = do
            setOpener T.South
            _ <- bid
            noInterference T.Hearts
            _ <- response
            noInterference T.Hearts
        explanation =
            "We've got a single-suited hand with at least 6 " .+ show suit .+
            " and 18+ HCP. Rebid our suit to show the extra length, and " .+
            "jump to show our extra strength."
      in
        situation "jrb" action rebid explanation
  in
    wrapVulDlr $ return sit <~ [ (B.b1H, B.b1H1N, B.b1H1N3H, T.Hearts)
                               , (B.b1S, B.b1S1N, B.b1S1N3S, T.Spades) ]


topic :: Topic
topic = makeTopic ("forcing " .+ T.Bid 1 T.Notrump) "F1N" situations
  where
    situations = wrap [ bid1NHearts
                      , bid1NSpades
                      , wrap [ wrap jumpShift
                             , wrap [jumpRebid, rebid2N, majorReverse]]
                      ]
