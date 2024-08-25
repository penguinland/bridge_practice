module Topics.ForcingOneNotrump(topic) where

import qualified Bids.ForcingOneNotrump as B
import CommonBids(setOpener, noInterference)
import EDSL(suitLength, maxSuitLength, pointRange, makeCall, forEach)
import Output((.+), Punct(..))
import Situation(situation, (<~))
import qualified Terminology as T
import Topic(Topic, wrap, Situations, makeTopic, stdWrapNW, stdWrapSE,
             wrapVulNW, wrapVulSE)


bid1NHearts :: Situations
bid1NHearts = let
    sit = let
        action = do
            setOpener T.North
            B.b1H
            noInterference T.Hearts
        explanation =
            "Partner has opened " .+ T.Bid 1 T.Hearts .+ ". We don't have " .+
            "a heart fit, don't have 4 spades, and aren't strong enough to " .+
            "force to game. Bid a forcing " .+ T.Bid 1 T.Notrump .+ ". " .+
            "Partner will rebid naturally, and we'll likely stop in some " .+
            "partscore."
      in
        situation "H1N" action B.b1H1N explanation
  in
    stdWrapNW sit  -- For us to bid a forcing 1N, we must be an unpassed hand.


bid1NSpades :: Situations
bid1NSpades = let
    sit = let
        action = do
            setOpener T.North
            B.b1S
            noInterference T.Spades
        explanation =
            "Partner has opened " .+ T.Bid 1 T.Spades .+ ". We don't have " .+
            "a spade fit and aren't strong enough to force to game. Bid a " .+
            "forcing " .+ T.Bid 1 T.Notrump .+ ". Partner will rebid " .+
            "naturally, and we'll likely stop in some partscore."
      in
        situation "S1N" action B.b1S1N explanation
  in
    stdWrapNW sit  -- For us to bid a forcing 1N, we must be an unpassed hand.


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
            T.Bid 2 T.Notrump .+ ". Partner will know almost exactly what " .+
            "we have, and can place the final contract."
      in
        situation "rb2N" action B.b1M1N2N explanation
  in
    wrapVulSE $ return sit
        <~ [(B.b1H, B.b1H1N, T.Hearts), (B.b1S, B.b1S1N, T.Spades)]


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
    wrapVulSE $ return sit <~ [ (B.b1H, B.b1H1N, B.b1H1N3C, T.Hearts)
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
    stdWrapSE sit


-- TODO: there's something wrong in here, where we sometimes are 6-4 in the
-- majors and should rebid the second one to show more of our hand. I don't have
-- time to figure it out right now, though.
jumpRebid :: Situations
jumpRebid = let
    sit (bid, response, rebid, suit) = let
        action = do
            setOpener T.South
            _ <- bid
            noInterference suit
            _ <- response
            noInterference suit
        explanation =
            "We've got a single-suited hand with at least 6 " .+ show suit .+
            " and 16" .+ NDash .+ "18 HCP. That's enough to invite to " .+
            "game, but not enough to outright force to it. Rebid our suit " .+
            "to show the extra length, and jump to show our extra " .+
            "strength. Parter might pass with a minimum, raise to game " .+
            "with extras and 2-card support, or try " .+ T.Bid 3 T.Notrump .+
            " with extras, support for our suit, but having the other suits."
      in
        situation "jrb" action rebid explanation
  in
    wrapVulSE $ return sit <~ [ (B.b1H, B.b1H1N, B.b1H1N3H, T.Hearts)
                              , (B.b1S, B.b1S1N, B.b1S1N3S, T.Spades) ]


limitRaise3 :: Situations
limitRaise3 = let
    sit (opening, response, suit) = let
        action = do
            setOpener T.North
            _ <- opening
            noInterference suit
            suitLength suit 3
            pointRange 10 12
        explanation =
            "We've got 3-card support for partner's major, and strength for " .+
            "a limit raise. Start with a forcing " .+ T.Bid 1 T.Notrump .+
            ", planning to then jump to 3 of partner's major. If they've " .+
            "got a minimum, they'll pass our second bid, and if they've " .+
            "got a little extra, they'll bid game."
      in
        situation "lr3" action response explanation
  in
    -- For us to bid a forcing 1N, we must be an unpassed hand.
    wrapVulNW $ return sit <~ [ (B.b1H, B.b1H1N, T.Hearts)
                              , (B.b1S, B.b1S1N, T.Spades) ]


raise2 :: Situations
raise2 = let
    sit (opening, response, suit) = let
        action = do
            setOpener T.North
            _ <- opening
            noInterference suit
            suitLength suit 2
            pointRange 6 9
            forEach (filter (/= suit) T.allSuits) (`maxSuitLength` 5)
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
    wrapVulNW $ return sit <~ [ (B.b1H, B.b1H1N, T.Hearts)
                              , (B.b1S, B.b1S1N, T.Spades) ]


nonjumpRebid :: Situations
nonjumpRebid = let
    sit (bid, response, rebid, suit) = let
        action = do
            setOpener T.South
            _ <- bid
            noInterference suit
            _ <- response
            noInterference suit
        explanation =
            "We've got a single-suited hand with at least 6 " .+ show suit .+
            " but not monster strength. Rebid our suit to show the extra " .+
            "length. Partner might pass with a doubleton " .+
            (init $ show suit) .+ ", raise to the 3 level to invite to " .+
            "game (we'd accept with a good 14 or more HCP, and pass " .+
            "otherwise), or bid their own long suit (which we would almost " .+
            "certainly pass)."
      in
        situation "nrb" action rebid explanation
  in
    wrapVulSE $ return sit <~ [ (B.b1H, B.b1H1N, B.b1H1N2H, T.Hearts)
                              , (B.b1S, B.b1S1N, B.b1S1N2S, T.Spades) ]


secondSuitRebid :: Situations
secondSuitRebid = let
    sit (bid, response, rebid, firstSuit) = let
        action = do
            setOpener T.South
            _ <- bid
            noInterference firstSuit
            _ <- response
            noInterference firstSuit
            maxSuitLength firstSuit 5  -- Don't worry about 6-5 or 6-4 shapes
        explanation =
            "We've opened our major, and partner has bid a forcing " .+
            T.Bid 1 T.Notrump .+ ". We're too weak (or too unbalanced) to " .+
            "rebid " .+ T.Bid 2 T.Notrump .+ ", and can't rebid " .+
            show firstSuit .+ " with only a 5-card suit. Bid our longest " .+
            "other suit. Sometimes it might only be a 3-card suit."
      in
        situation "b2nd" action rebid explanation
  in
    wrapVulSE $ return sit <~ [ (B.b1S, B.b1S1N, B.b1S1N2H, T.Spades)
                              , (B.b1S, B.b1S1N, B.b1S1N2D, T.Spades)
                              , (B.b1S, B.b1S1N, B.b1S1N2C, T.Spades)
                              , (B.b1H, B.b1H1N, B.b1H1N2C, T.Hearts)
                              , (B.b1H, B.b1H1N, B.b1H1N2D, T.Hearts)
                              ]


heartsSpadesThird :: Situations
heartsSpadesThird = let
    sit (bid, response, rebid) = let
        action = do
            setOpener T.South
            _ <- bid
            noInterference T.Hearts
            _ <- response
            noInterference T.Hearts
            suitLength T.Spades 4
        explanation =
            "We've opened " .+ T.Bid 1 T.Hearts .+ ", and partner has bid " .+
            "a forcing " .+ T.Bid 1 T.Notrump .+ ". Even though we have " .+
            "4 spades, we're too weak to reverse to " .+
            T.Bid 2 T.Spades .+ " (and we already know we don't have a " .+
            "spade fit because partner didn't bid " .+ T.Bid 1 T.Spades .+
            "). Bid our longest minor suit instead. "
      in
        situation "b2nd4" action rebid explanation
  in
    wrapVulSE $ return sit <~ [ (B.b1H, B.b1H1N, B.b1H1N2C)
                              , (B.b1H, B.b1H1N, B.b1H1N2D)
                              ]


wantFlannery :: Situations
wantFlannery = let
    sit = let
        action = do
            setOpener T.South
            B.b1H
            noInterference T.Hearts
            B.b1H1N
            noInterference T.Hearts
            suitLength T.Spades 4
            suitLength T.Hearts 5
            suitLength T.Diamonds 2
            suitLength T.Clubs 2
            pointRange 0 16
        explanation =
            "This is a super awkward shape that presents a challenge to " .+
            "basic 2/1 bidding. Partner's " .+T.Bid 1 T.Notrump .+ " is " .+
            "forcing, but we're not strong enough to reverse to " .+
            T.Bid 2 T.Spades .+ " or bid " .+ T.Bid 2 T.Notrump .+ ", we " .+
            "don't have a three-card minor to bid, and we don't have a " .+
            "sixth heart to rebid. The least bad lie is probably to rebid " .+
            "hearts anyway, but this hand shape is the exact problem that " .+
            "Flannery " .+ T.Bid 2 T.Diamonds .+ " or Semiforcing " .+
            T.Bid 1 T.Notrump .+ " are intended to fix."
      in
        situation "flan" action (makeCall $ T.Bid 2 T.Hearts) explanation
  in
    stdWrapSE sit


-- TODO:
--   - opener accepts/rejects invite
--   - responder weak with long suit
--   - responder is a passed hand so 1N is natural (and opener's rebid here)


topic :: Topic
topic = makeTopic ("forcing " .+ T.Bid 1 T.Notrump) "F1N" situations
  where
    situations = wrap [ wrap [bid1NHearts, bid1NSpades]
                      , limitRaise3
                      , raise2
                      , nonjumpRebid
                      , secondSuitRebid
                      -- Rarer situations
                      , wrap [ wrap jumpShift
                             , wrap [jumpRebid, rebid2N, majorReverse]
                             , wrap [wantFlannery, heartsSpadesThird]
                             ]
                      ]
