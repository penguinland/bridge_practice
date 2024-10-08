module Topics.Stayman(topic) where

import qualified Bids.OneNotrump as B
import CommonBids(setOpener)
-- TODO: replace makePass with something more intelligent
import EDSL(makePass, pointRange, suitLength, maxSuitLength, forEach)
import Output((.+), Punct(..))
import Situation(situation, (<~))
import qualified Terminology as T
import Topic(Topic, stdWrap, wrap, wrapDlr, wrapNW, wrapSE, Situations,
             makeTopic)


garbageStayman :: Situations
garbageStayman = let
    sit = let
        action = do
            setOpener T.North
            B.b1N
            makePass
            pointRange 0 7
        explanation =
            "Partner has opened a strong " .+ T.Bid 1 T.Notrump .+ ". We " .+
            "have less than invitational strength, but we have both majors " .+
            "and diamonds. Bid Garbage Stayman! Partner will think it's " .+
            OpenQuote .+ "normal" .+ CloseQuote .+ " Stayman, and bid " .+
            "accordingly. Pass whatever partner does: if we find an 8-card " .+
            "major fit, that will likely play better than stopping in " .+
            T.Bid 1 T.Notrump .+ ". and if partner has neither major, " .+
            "they're likely to have diamonds for us. It's possible partner " .+
            "has 3325 shape and gets stuck without a trump fit, but that's " .+
            "unlikely. On average, this will be better than passing " .+
            T.Bid 1 T.Notrump .+ "."
      in situation "garb" action B.b1N2C explanation
  in
    stdWrap sit


nongarbageStayman :: Situations
nongarbageStayman = let
    sit = let
        action = do
            setOpener T.North
            B.b1N
            makePass
            pointRange 8 40
        explanation =
            "Partner has opened a strong " .+ T.Bid 1 T.Notrump .+ ". We " .+
            "have a 4-card major and at least invitational strength. Bid " .+
            "Stayman, and see if we've got a major-suit fit."
      in situation "stmn" action B.b1N2C explanation
  in
    stdWrap sit


noMajor :: Situations
noMajor = let
    sit = let
        action = do
            setOpener T.South
            B.b1N
            makePass
            B.b1N2C
            makePass
        explanation =
            "We opened a strong " .+ T.Bid 1 T.Notrump .+ ", and partner " .+
            "has bid Stayman. We don't have a 4-card major, so bid " .+
            T.Bid 2 T.Diamonds .+ " to convey that. Partner is captain of " .+
            "the auction; they'll know what to do next."
      in situation "noMaj" action B.b1N2C2D explanation
  in
    stdWrap sit


oneMajor :: Situations
oneMajor = let
    sit bid = let
        action = do
            setOpener T.South
            B.b1N
            makePass
            B.b1N2C
            makePass
            maxSuitLength (T.otherMajor . T.suitBid $ bid) 3
        explanation =
            "We opened a strong " .+ T.Bid 1 T.Notrump .+ ", and partner " .+
            "has bid Stayman, asking whether we have any major suits. " .+
            "We've got one, so bid it. Partner is captain of " .+
            "the auction; they'll know what to do next."
      in situation "1Maj" action bid explanation
  in
    wrapDlr $ return sit <~ [B.b1N2C2H, B.b1N2C2S]


bothMajors :: Situations
bothMajors = let
    sit = let
        action = do
            setOpener T.South
            B.b1N
            makePass
            B.b1N2C
            makePass
            forEach T.majorSuits (`suitLength` 4)
        explanation =
            "We opened a strong " .+ T.Bid 1 T.Notrump .+ ", and partner " .+
            "has bid Stayman, asking whether we have any major suits. " .+
            "We've got both, so bid the cheaper one, hearts. If partner " .+
            "doesn't like our suit, they must have spades, and we'll " .+
            "be able to bid those later."
      in situation "2Maj" action B.b1N2C2H explanation
  in
    stdWrap sit


noFitBalancedInv :: Situations
noFitBalancedInv = let
    sit = let
        action = do
            setOpener T.North
            B.b1N
            makePass
            B.b1N2C
            makePass
            B.b1N2C2D
            makePass
        explanation =
            "We bid Stayman, hoping for a fit with partner, but they " .+
            "don't have a 4-card major. With no major-suit fit, bid " .+
            T.Bid 2 T.Notrump .+ " to show our balanced invite. Partner " .+
            "will pass with a minimum, and raise to game with a maximum."
      in situation "invNoF" action B.b1N2C2D2N explanation
  in
    stdWrap sit


noFitBalancedGf :: Situations
noFitBalancedGf = let
    sit = let
        action = do
            setOpener T.North
            B.b1N
            makePass
            B.b1N2C
            makePass
            B.b1N2C2D
            makePass
        explanation =
            "We bid Stayman, hoping for a major-suit fit, but partner " .+
            "doesn't have a 4-card major. With game-forcing strength but " .+
            "no slam interest, sign off in " .+ T.Bid 3 T.Notrump .+ "."
      in situation "GfNoF" action B.b1N2C2D3N explanation
  in
    stdWrap sit



noFitBalancedSlamInv :: Situations
noFitBalancedSlamInv = let
    sit = let
        action = do
            setOpener T.North
            B.b1N
            makePass
            B.b1N2C
            makePass
            B.b1N2C2D
            makePass
        explanation =
            "We bid Stayman, hoping for a major-suit fit, but partner " .+
            "doesn't have a 4-card major. With slam-invitational strength, " .+
            "invite with a quantitative " .+ T.Bid 4 T.Notrump .+ ". " .+
            "Partner will pass with a minimum and bid 6 with a maximum."
      in situation "SlINoF" action B.b1N2C2D4N explanation
  in
    -- We're slam invitational; we can't be a passed hand.
    wrapNW $ return sit


fitInvite :: Situations
fitInvite = let
    sit (openerRebid, responderRebid) = let
        action = do
            setOpener T.North
            B.b1N
            makePass
            B.b1N2C
            makePass
            _ <- openerRebid
            makePass
        explanation =
            "Partner opened a strong " .+ T.Bid 1 T.Notrump .+ ", we bid " .+
            "Stayman, and found a major-suit fit. With invitational " .+
            "strength, invite to game. Partner will pass with a minimum " .+
            "and bid 4 with a maximum."
      in situation "fitInv" action responderRebid explanation
  in
    wrapDlr $ return sit <~ [ (B.b1N2C2H, B.b1N2C2H3H)
                            , (B.b1N2C2S, B.b1N2C2S3S) ]


fitGF :: Situations
fitGF = let
    sit (openerRebid, responderRebid) = let
        action = do
            setOpener T.North
            B.b1N
            makePass
            B.b1N2C
            makePass
            _ <- openerRebid
            makePass
        explanation =
            "Partner opened a strong " .+ T.Bid 1 T.Notrump .+ ", we bid " .+
            "Stayman, and found a major-suit fit. With no slam interest, " .+
            "sign off in game."
      in situation "fitGF" action responderRebid explanation
  in
    wrapDlr $ return sit <~ [ (B.b1N2C2H, B.b1N2C2H4H)
                            , (B.b1N2C2S, B.b1N2C2S4S) ]


fitSlam :: Situations
fitSlam = let
    sit (openerRebid, responderRebid) = let
        action = do
            setOpener T.North
            B.b1N
            makePass
            B.b1N2C
            makePass
            _ <- openerRebid
            makePass
        explanation =
            "Partner opened a strong " .+ T.Bid 1 T.Notrump .+ ", we bid " .+
            "Stayman, and found a major-suit fit. Show at least mild slam " .+
            "interest by bidding the other major at the 3 level! Partner " .+
            "will start control bidding. If you play Serious or Frivolous " .+
            T.Bid 3 T.Notrump .+ ", partner might also bid that."
      in situation "fitSl" action responderRebid explanation
  in
    -- We're slam invitational; we can't be a passed hand.
    wrapNW $ return sit <~ [ (B.b1N2C2H, B.b1N2C2H3S)
                           , (B.b1N2C2S, B.b1N2C2S3H) ]


wrongMajorGFH :: Situations
wrongMajorGFH = let
    sit = let
        action = do
            setOpener T.North
            B.b1N
            makePass
            B.b1N2C
            makePass
            B.b1N2C2H
            makePass
        explanation =
            "Partner opened a strong " .+ T.Bid 1 T.Notrump .+ ", and we " .+
            "bid Stayman. Our only major is spades, though, and partner's " .+
            "major is hearts, so we don't seem to have a fit. We're game " .+
            "forcing with a balanced hand, so try signing off in " .+
            T.Bid 3 T.Notrump .+ ". It's possible partner actually has " .+
            "both majors, and will correct to " .+ T.Bid 4 T.Spades .+ "."
      in situation "Wr3NH" action B.b1N2C2H3N explanation
  in
    stdWrap sit


wrongMajorGFS :: Situations
wrongMajorGFS = let
    sit = let
        action = do
            setOpener T.North
            B.b1N
            makePass
            B.b1N2C
            makePass
            B.b1N2C2S
            makePass
            maxSuitLength T.Spades 2
        explanation =
            "Partner opened a strong " .+ T.Bid 1 T.Notrump .+ ", and we " .+
            "bid Stayman. Partner has 4+ spades, but does not have " .+
            "4 hearts, so we definitely don't have a fit. We're " .+
            "game forcing with a balanced hand, though, so sign off in " .+
            T.Bid 3 T.Notrump .+ "."
      in situation "Wr3NS" action B.b1N2C2S3N explanation
  in
    stdWrap sit


-- Note: the following situation sounds plausible, until you realize that
-- responder must be 4-3 in the majors. In some systems, they might have bid
-- Puppet Stayman instead of regular Stayman. So, these hands are excluded from
-- the regular Stayman topic to avoid overlap!
{-
wrongMajorGFSAmb :: Situations
wrongMajorGFSAmb = let
    sit = let
        action = do
            setOpener T.North
            B.b1N
            makePass
            B.b1N2C
            makePass
            B.b1N2C2S
            makePass
            suitLength T.Spades 3
        explanation =
            "Partner opened a strong " .+ T.Bid 1 T.Notrump .+ ", and we " .+
            "bid Stayman. Partner has 4+ spades, but does not have " .+
            "4 hearts, so we don't obviously have a fit. We're " .+
            "game forcing with a balanced hand, though, so sign off in " .+
            T.Bid 3 T.Notrump .+ ". It's possible we have missed a 5" .+
            NDash .+ "3 spade fit, but there's not much we can do about " .+
            "that, and playing in notrump is probably alright."
      in situation "Wr3NSo" action B.b1N2C2S3N explanation
  in
    stdWrap sit
-}


bothMajorsGF :: Situations
bothMajorsGF = let
    sit = let
        action = do
            setOpener T.South
            B.b1N
            makePass
            B.b1N2C
            makePass
            B.b1N2C2H
            makePass
            B.b1N2C2H3N
            makePass
        explanation =
            "We opened a strong " .+ T.Bid 1 T.Notrump .+ " with both " .+
            "majors, and partner bid Stayman. We showed our cheapest one, " .+
            "and partner jumped to " .+ T.Bid 3 T.Notrump .+ ", showing " .+
            "game-forcing strength with no slam interest, and no heart fit. " .+
            "To bid Stayman, they must have had a 4-card major, so they " .+
            "must have spades! We've got a fit: let's play in that instead " .+
            "of notrump."
      in situation "3N4S" action B.b1N2C2H3N4S explanation
  in
    stdWrap sit


inv54 :: Situations
inv54 = let
    sit bid = let
        action = do
            setOpener T.North
            B.b1N
            makePass
            B.b1N2C
            makePass
            B.b1N2C2D
            makePass
        explanation =
            "Partner opened a strong " .+ T.Bid 1 T.Notrump .+ ". With " .+
            "an invitational hand and 5-4 in the majors, we bid Stayman, " .+
            "hoping to find a fit with partner, but they don't have a " .+
            "4-card major. Bid our 5-card major to show our hand. If " .+
            "partner has a minimum, they can pass with 3 " .+
            (show . T.suitBid $ bid) .+ " or bid " .+ T.Bid 2 T.Notrump .+
            " if we still don't have a fit, and if they have a maximum " .+
            "they can either raise us to game with a fit or " .+
            T.Bid 3 T.Notrump .+ " without one."
      in situation "inv54" action bid explanation
  in
    wrapDlr $ return sit <~ [B.b1N2C2D2H, B.b1N2C2D2S]


noFitUnbalancedGF :: Situations
noFitUnbalancedGF = let
    sit (openerBid, responderRebid) = let
        action = do
            setOpener T.North
            B.b1N
            makePass
            B.b1N2C
            makePass
            _ <- openerBid
            makePass
        explanation =
            "Partner opened a strong " .+ T.Bid 1 T.Notrump .+ ". With " .+
            "a game-forcing hand and a 4-card major, we bid Stayman, " .+
            "hoping to find a fit with partner, but they have the other " .+
            "major. We've got an unbalanced hand, so let's bid our second " .+
            "suit. If partner has the fourth suit covered, they'll bid " .+
            T.Bid 3 T.Notrump .+ ", and if they don't, they'll continue " .+
            "to describe their hand. Our bid is game-forcing, so we don't " .+
            "need to worry about partner passing us out too low."
      in situation "nfubgf" action responderRebid explanation
  in
    wrapDlr $ return sit <~ [ (B.b1N2C2H, B.b1N2C2H3C)
                            , (B.b1N2C2H, B.b1N2C2H3D)
                            , (B.b1N2C2S, B.b1N2C2S3C)
                            , (B.b1N2C2S, B.b1N2C2S3D)
                            ]


bothMajorsUnbalancedPassed :: Situations
bothMajorsUnbalancedPassed = let
    sit (openerBid, responderBid, opener4S) = let
        action = do
            setOpener T.South
            B.b1N
            makePass
            B.b1N2C
            makePass
            _ <- openerBid
            makePass
            _ <- responderBid
            makePass
        explanation =
            "We opened a strong " .+ T.Bid 1 T.Notrump .+ ", and partner " .+
            "bid Stayman. We've got both majors and showed our lower suit, " .+
            "but didn't find a fit there. Instead, partner showed an " .+
            "unbalanced, game-forcing hand. They must have 4 spades! " .+
            "Furthermore, partner is a passed hand, so they can't be " .+
            "interested in slam. On the Principle of Fast Arrival, sign " .+
            "off in our major-suit game."
      in situation "bmubup" action opener4S explanation
  in
    -- This version is only for when responder is a passed hand
    wrapNW $ return sit <~ [ (B.b1N2C2H, B.b1N2C2H3C, B.b1N2C2H3C4S)
                           , (B.b1N2C2H, B.b1N2C2H3D, B.b1N2C2H3D4S)
                           ]


bothMajorsUnbalancedUnpassed :: Situations
bothMajorsUnbalancedUnpassed = let
    sit (openerBid, responderBid, opener4S) = let
        action = do
            setOpener T.South
            B.b1N
            makePass
            B.b1N2C
            makePass
            _ <- openerBid
            makePass
            _ <- responderBid
            makePass
        explanation =
            "We opened a strong " .+ T.Bid 1 T.Notrump .+ ", and partner " .+
            "bid Stayman. We've got both majors and showed our lower suit, " .+
            "but didn't find a fit there. Instead, partner showed an " .+
            "unbalanced, game-forcing hand. They must have 4 spades! " .+
            "We should show our major-suit fit. However, partner is an " .+
            "unpassed hand, so we should conserve bidding space by " .+
            "responding as cheaply as possible. Partner will probably " .+
            "just sign off in " .+ T.Bid 4 T.Spades .+ " themselves, but " .+
            "give them a chance to control bid in case they have slam interest."
      in situation "bmubp" action opener4S explanation
  in
    -- This version is only for when responder is a passed hand
    wrapSE $ return sit <~ [ (B.b1N2C2H, B.b1N2C2H3C, B.b1N2C2H3C3S)
                           , (B.b1N2C2H, B.b1N2C2H3D, B.b1N2C2H3D3S)
                           ]


-- TODO eventually, but maybe in separate topics:
--   - responder bids Texas over 1N-2C-2D
--   - opener completes Texas
--   - Smolen
--   - DON'T DO opener having both majors and responder inviting with 2N over
--     2H. If you're playing 4-way transfers, that doesn't necessarily show
--     spades.


topic :: Topic
topic = makeTopic "Stayman" "Stmn" situations
  where
    situations = wrap [ wrap [ garbageStayman
                             , nongarbageStayman
                             , bothMajorsGF
                             , bothMajorsUnbalancedPassed
                             , bothMajorsUnbalancedUnpassed]
                      , noMajor
                      , oneMajor
                      , bothMajors
                      , noFitBalancedInv
                      , noFitBalancedGf
                      , noFitBalancedSlamInv
                      , fitInvite
                      , fitGF
                      , fitSlam
                      , wrap [wrongMajorGFH, wrongMajorGFS]
                      , inv54
                      , noFitUnbalancedGF
                      ]
