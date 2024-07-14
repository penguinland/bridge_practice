module Topics.ForcingOneNotrump(topic) where

import Output((.+), Punct(..))
import Topic(Topic, wrap, Situations, makeTopic)
import Auction(suitLength, maxSuitLength, pointRange)
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
    -- For us to bid a forcing 1N, we must be an unpassed hand.
    wrap $ return sit <~ T.allVulnerabilities <~ [T.North, T.West]


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
    -- For us to bid a forcing 1N, we must be an unpassed hand.
    wrap $ return sit <~ T.allVulnerabilities <~ [T.North, T.West]

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
    wrap $ return sit
        <~ [(B.b1H, B.b1H1N, T.Hearts), (B.b1S, B.b1S1N, T.Spades)]
        <~ T.allVulnerabilities
        <~ [T.South, T.East]


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
    wrap $ return sit <~ [ (B.b1H, B.b1H1N, B.b1H1N3C, T.Hearts)
                         , (B.b1H, B.b1H1N, B.b1H1N3D, T.Hearts)
                         , (B.b1S, B.b1S1N, B.b1S1N3C, T.Spades)
                         , (B.b1S, B.b1S1N, B.b1S1N3D, T.Spades)
                         , (B.b1S, B.b1S1N, B.b1S1N3H, T.Spades)
                         ]
                      <~ T.allVulnerabilities
                      <~ [T.South, T.East]


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
    wrap $ return sit <~ T.allVulnerabilities <~ [T.South, T.East]


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
    wrap $ return sit <~ [ (B.b1H, B.b1H1N, B.b1H1N3H, T.Hearts)
                         , (B.b1S, B.b1S1N, B.b1S1N3S, T.Spades) ]
                      <~ T.allVulnerabilities
                      <~ [T.South, T.East]


limitRaise3 :: Situations
limitRaise3 = let
    sit (opening, response, suit) = let
        action = do
            setOpener T.North
            _ <- opening
            noInterference T.Hearts
            suitLength suit 3
            pointRange 10 12
        explanation =
            "We've got 3-card support for partner's major, and strength for " .+
            "a limit raise. Start with a forcing " .+ T.Bid 1 T.Notrump .+
            ", planning to then jump to 3 of partner's major. If they've " .+
            "got a minimum, they'll pass your second bid, and if they've " .+
            "got a little extra, they'll bid game."
      in
        situation "lr3" action response explanation
  in
    -- For us to bid a forcing 1N, we must be an unpassed hand.
    wrap $ return sit <~ [ (B.b1H, B.b1H1N, T.Hearts)
                         , (B.b1S, B.b1S1N, T.Spades) ]
                      <~ T.allVulnerabilities
                      <~ [T.North, T.West]


raise2 :: Situations
raise2 = let
    sit (opening, response, suit) = let
        action = do
            setOpener T.North
            _ <- opening
            noInterference T.Hearts
            suitLength suit 2
            pointRange 6 9
            mapM_ (`maxSuitLength` 5) . filter (/= suit) $ T.allSuits
        explanation =
            "We're not even strong enough to invite to game, we only have " .+
            "2-card support for partner's major, and we don't have our own " .+
            "long suit to suggest. Bid a forcing " .+ T.Bid 1 T.Notrump .+
            ", planning to then rebid 2 of partner's major, which will " .+
            "likely be the final contract. but if we've got a 5-card suit " .+
            "and partner bids that on their second turn, we might pass it " .+
            "instead! If partner rebids their major, showing at least 6 " .+
            "cards in it, we'll be delighted to pass in an 8-card fit. " .+
            "If partner jumps or reverses, game might still be available, " .+
            "and we'll continue bidding naturally over that."
      in
        situation "mr2" action response explanation
  in
    -- For us to bid a forcing 1N, we must be an unpassed hand.
    wrap $ return sit <~ [ (B.b1H, B.b1H1N, T.Hearts)
                         , (B.b1S, B.b1S1N, T.Spades) ]
                      <~ T.allVulnerabilities
                      <~ [T.North, T.West]


-- more situations:
--                  opener rebids without jumping/reversing
--                  opener accepts/rejects invite
--                  responder weak with long suit,


topic :: Topic
topic = makeTopic ("forcing " .+ T.Bid 1 T.Notrump) "F1N" situations
  where
    situations = wrap [ wrap [bid1NHearts, bid1NSpades]
                      , wrap [ wrap jumpShift
                             , wrap [jumpRebid, rebid2N, majorReverse]]
                      , limitRaise3
                      , raise2
                      ]
