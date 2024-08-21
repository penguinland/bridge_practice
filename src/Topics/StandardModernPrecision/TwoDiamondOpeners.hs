module Topics.StandardModernPrecision.TwoDiamondOpeners(topic) where

import Action(Action)
import Bids.StandardModernPrecision.BasicBids(setOpener)
import qualified Bids.StandardModernPrecision.TwoDiamonds as B
import CommonBids(takeoutDouble)
import EDSL(forbid, suitLength, makePass)
import Output((.+))
import Situation(situation, (<~))
import qualified Terminology as T
import Topic(Topic, wrap, stdWrap, wrapVulDlr, Situations, makeTopic,
             wrapVulNW, wrapVulSE, stdWrapNW, stdWrapSE)


-- When trying to sign off with less than invitational strength, a new suit
-- being nonforcing is alertable only if you're an unpassed hand, so we have
-- different bids depending on who the dealer is.
nwOrSeBid :: Action -> Action -> [(Action, T.Direction)]
nwOrSeBid nw se = [(nw, T.North), (nw, T.West), (se, T.South), (se, T.East)]


open :: Situations
open = let
    action = do
        setOpener T.South
    explanation =
        "With an opening hand too weak to bid " .+ T.Bid 1 T.Clubs .+ ", " .+
        "open " .+ T.Bid 2 T.Diamonds .+ " with no 5-card major, " .+
        "no 6-card club suit, and at most 1 diamond."
  in
    stdWrap $ situation "Open" action B.b2D explanation


immediateSignoffSpades34 :: Situations
immediateSignoffSpades34 = let
    sit (bid, direction) spadeLength vul = let
        action = do
            setOpener T.North
            suitLength T.Spades spadeLength
            B.b2D
            B.noDirectOvercall
        explanation =
            "Without the strength for a game contract, sign off in " .+
            T.Bid 2 T.Spades .+ " with a likely fit. " .+
            if spadeLength == 3
                then "If you're stuck playing a 4-3 fit, oh well."
                else ""
      in
        situation "S43" action bid explanation vul direction
  in
    wrap $ return sit <~ nwOrSeBid B.b2D2S B.bP2D2S
                      <~ [3, 4]
                      <~ T.allVulnerabilities


immediateSignoffSpades5 :: Situations
immediateSignoffSpades5 = let
    sit (bid, direction) vul = let
        action = do
            setOpener T.North
            B.b2D
            B.noDirectOvercall
        explanation =
            "Without the strength for a game contract, sign off in " .+
            T.Bid 2 T.Spades .+ " with a guaranteed fit."
      in
        situation "Sfit" action bid explanation vul direction
  in
    -- Although this can happen when anyone is dealer, it is very rare when East
    -- deals, and generating those hands is difficult for dealer (often
    -- requiring over 1,000,000 hands to be generated). Consequently, we limit
    -- these situations to times when East isn't dealer.
    wrap $ return sit <~ [ (B.bP2D2S, T.South)
                         , (B.bP2D2S, T.West)
                         , (B.b2D2S, T.North)]
                      <~ T.allVulnerabilities


immediateSignoffClubs :: Situations
immediateSignoffClubs = let
    sit (bid, direction) vul = let
        action = do
            setOpener T.North
            B.b2D
            B.noDirectOvercall
        explanation =
            "Without the strength to invite to game, sign off in a club " .+
            "partscore."
      in
        situation "3C" action bid explanation vul direction
  in
    wrap $ return sit <~ nwOrSeBid B.b2D3C B.bP2D3C
                      <~ T.allVulnerabilities


passBlackSignoff :: Situations
passBlackSignoff = let
    sit (bid, suit, direction) vul = let
        action = do
            setOpener T.South
            B.b2D
            B.noDirectOvercall
            _ <- bid
            forbid $ takeoutDouble suit
            B.noDirectOvercall
        explanation =
            "Partner has less-than-invitational values and is signing off. " .+
            "Just pass. It's possible that sometimes you'll end up in a " .+
            "7-card fit."
      in
        situation "3CP" action (makePass) explanation vul direction
  in
    wrap $ return sit <~ [ (B.bP2D2S, T.Spades, T.West)
                         , (B.bP2D2S, T.Spades, T.North)
                         , (B.b2D2S, T.Spades, T.East)
                         , (B.b2D2S, T.Spades, T.South)
                         , (B.bP2D3C, T.Clubs, T.West)
                         , (B.bP2D3C, T.Clubs, T.North)
                         , (B.b2D3C, T.Clubs, T.East)
                         , (B.b2D3C, T.Clubs, T.South)
                         ]
                      <~ T.allVulnerabilities


immediateSignoffHearts :: Situations
immediateSignoffHearts = let
    sit (bid, direction) vul = let
        action = do
            setOpener T.North
            B.b2D
            B.noDirectOvercall
        explanation =
            "Without the strength to invite to game, sign off in " .+ bid .+
            ". Remember that opener might " .+
            "pull the bid to " .+ T.Bid 2 T.Spades .+ " with " .+
            "exactly 4315 shape."
      in
        situation "2H" action bid explanation vul direction
  in
    wrap $ return sit <~ nwOrSeBid B.b2D2H B.bP2D2H <~ T.allVulnerabilities


passSignoffHearts :: Situations
passSignoffHearts = let
    sit (bid, direction) vul = let
        action = do
            setOpener T.South
            B.b2D
            B.noDirectOvercall
            _ <- bid
            B.noDirectOvercall
            forbid B.b2D2H2S
        explanation =
            "Partner has less-than-invitational values and is signing off. " .+
            "Given that you have 4 hearts, pass."
      in
        situation "2H2S" action (makePass) explanation vul direction
  in
    wrap $ return sit <~ nwOrSeBid B.bP2D2H B.b2D2H <~ T.allVulnerabilities


correctSignoffHearts :: Situations
correctSignoffHearts = let
    sit (bid, direction) responderHeartLength vul = let
        action = do
            setOpener T.South
            B.b2D
            B.noDirectOvercall
            suitLength T.Hearts responderHeartLength
            _ <- bid
            B.noDirectOvercall
        explanation =
            "Partner has less-than-invitational values and is signing off. " .+
            "With 4315 shape, though, pull this to " .+
            T.Bid 2 T.Spades .+ " in case partner only had 3 " .+
            "hearts (e.g., 3343 or 3352 shape). Partner, might further " .+
            "pull this bid to the final contract, now that they know our " .+
            "exact shape."
      in
        situation "2H2S" action B.b2D2H2S explanation vul direction
  in
    wrap $ return sit <~ nwOrSeBid B.bP2D2H B.b2D2H
                      -- We want to commonly show times when responder has just
                      -- 3 hearts (and we're avoiding a 3-3 fit) and times when
                      -- responder has a real heart suit.
                      <~ [3, 5]
                      <~ T.allVulnerabilities


mixedRaise :: Situations
mixedRaise = let
    sit bid = let
        action = do
            setOpener T.North
            B.b2D
            B.noDirectOvercall
        explanation =
            "We've got just barely less than invitational values and a " .+
            "likely 9-card major-suit fit. Bid a mixed raise. Partner " .+
            "will almost certainly pass, and we've found our fit while " .+
            "making it harder for the opponents to compete."
      in
        situation "mixed" action bid explanation
  in
    wrapVulDlr $ return sit <~ [B.b2D3H, B.b2D3S]


immediateGameSignoff :: Situations
immediateGameSignoff = let
    sit bid = let
        action = do
            setOpener T.North
            B.b2D
            B.noDirectOvercall
        explanation =
            "We're game-forcing with no interest in slam. When we know the " .+
            "final contract, sign off in it immediately."
      in
        situation "gfso" action bid explanation
  in
    wrapVulNW $ return sit <~ [B.b2D3N, B.b2D4H, B.b2D4S]


bid2N :: Situations
bid2N = let
    sit = let
        action = do
            setOpener T.North
            B.b2D
            B.noDirectOvercall
        explanation =
            "We're at least invitational, and don't know what the final " .+
            "contract should be yet. Bid " .+ T.Bid 2 T.Notrump .+ " to " .+
            "ask partner to describe their hand further."
      in
        situation "b2N" action B.b2D2N explanation
  in
    stdWrapNW sit  -- We're both unpassed hands


minimumResponse :: Situations
minimumResponse = let
    sit = let
        action = do
            setOpener T.South
            B.b2D
            B.noDirectOvercall
            B.b2D2N
            B.noDirectOvercall
        explanation =
            "Partner has asked us to describe our strength more. We're in " .+
            "the bottom half of our range, so bid " .+ T.Bid 3 T.Clubs .+
            ". If partner was only invitational, they'll sign off after " .+
            "this (possibly by passing!), and if they're game forcing, " .+
            "they'll relay to ask us about our majors."
      in
        situation "min" action B.b2D2N3C explanation
  in
    stdWrapSE sit  -- We're both unpassed hands


maximumResponse :: Situations
maximumResponse = let
    sit bid = let
        action = do
            setOpener T.South
            B.b2D
            B.noDirectOvercall
            B.b2D2N
            B.noDirectOvercall
        explanation =
            "Partner has asked us to describe our strength and majors " .+
            "more. We're in the top half of our range (we'd accept a game " .+
            "invite). Bid similar to Smolen: bid our shorter major if we " .+
            "have one, or " .+ T.Bid 3 T.Diamonds .+ " if they're of equal " .+
            "length. Partner's next bid will either be setting trump at " .+
            "the 3 level (showing slam interest and triggering a round of " .+
            "control bidding), signing off in " .+ T.Bid 3 T.Notrump .+
            ", or " .+ B.name44Rkc .+ "."
      in
        situation "max" action bid explanation
  in
    wrapVulSE $ return sit <~ [B.b2D2N3D, B.b2D2N3H, B.b2D2N3S]


gfAnyway :: Situations
gfAnyway = let
    sit = let
        action = do
            setOpener T.North
            B.b2D
            B.noDirectOvercall
            B.b2D2N
            B.noDirectOvercall
            B.b2D2N3C
            B.noDirectOvercall
        explanation =
            "Partner has shown a minimum, but we're game forcing anyway. " .+
            "Bid " .+ T.Bid 3 T.Diamonds .+ " to re-ask partner about " .+
            "their major suits. When they reply, you'll know their exact " .+
            "shape and their strength to within 1 HCP. You'll then know " .+
            "what the final contract should be, and can use " .+ B.name44Rkc .+
            " to place it."
      in
        situation "reask" action B.b2D2N3C3D explanation
  in
    stdWrapNW sit


gfAnywayResponses :: Situations
gfAnywayResponses = let
    sit bid = let
        action = do
            setOpener T.South
            B.b2D
            B.noDirectOvercall
            B.b2D2N
            B.noDirectOvercall
            B.b2D2N3C
            B.noDirectOvercall
            B.b2D2N3C3D
            B.noDirectOvercall
        explanation =
            "We have shown a minimum, but partner is game forcing anyway. " .+
            "Use Smolen-like bids to show partner our major-suit shape: " .+
            "bid a 3-card major, or " .+ T.Bid 3 T.Notrump .+ " to show " .+
            "both majors. Partner now knows our strength and shape, and " .+
            "can use " .+ B.name44Rkc .+ " to place the final contract " .+
            "(or sign off in " .+ T.Bid 3 T.Notrump .+ ")."
      in
        situation "reans" action bid explanation
  in
    wrapVulSE $ return sit <~ [B.b2D2N3C3D3H, B.b2D2N3C3D3S, B.b2D2N3C3D3N]


invSignoff :: Situations
invSignoff = let
    sit bid = let
        action = do
            setOpener T.North
            B.b2D
            B.noDirectOvercall
            B.b2D2N
            B.noDirectOvercall
            B.b2D2N3C
            B.noDirectOvercall
        explanation =
            "We were invitational, but partner has shown a minimum, " .+
            "indicating that they would not accept an invite to game. " .+
            "Sign off in partscore."
      in
        situation "invso" action bid explanation
  in
    wrapVulNW $ return sit <~ [B.b2D2N3CP, B.b2D2N3C3H, B.b2D2N3C3S]


-- TODO:
--   - Responder immediately bids a major-suit game (see immediateGameSignoff)
--   - Responder signs off in 3N over opener's 3C (they had a slam invite)
--     (would other game-level rebids be signoff? Probably, but it's much easier
--     to try 3D before signing off).
--   - Rework setOpener, wrapVulDlr, stdWrap, stdWrapNW, stdWrapSE
--   - DON'T DO 4C/4D/RKC: that should be a separate topic


topic :: Topic
topic = makeTopic description "SMP2D" situations
  where
    description = ("SMP " .+ T.Bid 2 T.Diamonds .+ " auctions")
    situations = wrap [ open
                      -- Responder signs off (equiprobable in all suits)
                      , wrap [ wrap [ immediateSignoffSpades5
                                    -- Equiprobable in all spade lengths
                                    , immediateSignoffSpades34
                                    , immediateSignoffSpades34
                                    ]
                             , immediateSignoffClubs
                             , immediateSignoffHearts
                             ]
                      -- Opener passes responder's signoff (equiprobable in all
                      -- suits)
                      , wrap [ passBlackSignoff
                             , passBlackSignoff
                             , passBlackSignoff
                             , passBlackSignoff
                             , passSignoffHearts
                             , correctSignoffHearts
                             ]
                      , mixedRaise
                      , immediateGameSignoff
                      , bid2N
                      , minimumResponse
                      , maximumResponse
                      , gfAnyway
                      , gfAnywayResponses
                      , invSignoff
                      ]
