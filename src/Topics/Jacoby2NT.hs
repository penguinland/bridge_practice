module Topics.Jacoby2NT(topic) where

import Control.Monad(join)

import qualified Bids.Jacoby2NT as B
import CommonBids(setOpener, noInterference)
import EDSL(minSuitLength, hasTopN, forbid)
import Output((.+))
import Situation(situation, (<~))
import qualified Terminology as T
import Topic(wrap, wrapNW, wrapSE, Situations, Topic, makeTopic)


j2nt :: Situations
j2nt = let
    sit (openerBid, j2ntBid) = let
        suit = T.suitBid openerBid
        action = do
            setOpener T.North
            _ <- openerBid
            noInterference suit
        explanation =
            "We've got game-forcing strength with at least 4-card support " .+
            "for partner's " .+ show suit .+ ". Bid Jacoby " .+
            T.Bid 2 T.Notrump .+ " to show this. Now that we've set trump " .+
            "and have entered a game-forcing auction,  we'll figure out " .+
            "whether we belong in game or slam."
      in
        situation "j2n" action j2ntBid explanation
  in
    -- We must be an unpassed hand to be game-forcing.
    wrapNW $ return sit <~ [(B.b1H, B.b1H2N), (B.b1S, B.b1S2N)]


singleton :: Situations
singleton = let
    sit (openerBid, j2ntBid, singletonBid) = let
        suit = T.suitBid openerBid
        action = do
            setOpener T.South
            _ <- openerBid
            noInterference suit
            _ <- j2ntBid
            noInterference suit
        explanation =
            "Partner has bid Jacoby " .+ T.Bid 2 T.Notrump .+ ". We don't " .+
            "have a good 5-card side suit to jump to, but we do have " .+
            "shortness to bid. This will help partner decide whether to " .+
            "sign off in " .+ T.Bid 4 suit .+ ", or start control bidding " .+
            "to investigate slam. If you play Serious or Frivolous " .+
            T.Bid 3 T.Notrump .+ ", partner might also bid that. If they " .+
            "try signing off but we have enough extra strength, we can bid " .+
            "on to look for slam anyway."
      in
        situation "sing" action singletonBid explanation
  in
    -- Partner must be an unpassed hand to be game-forcing.
    wrapSE $ return sit <~ [ (B.b1H, B.b1H2N, B.b1H2N3C)
                           , (B.b1H, B.b1H2N, B.b1H2N3D)
                           , (B.b1H, B.b1H2N, B.b1H2N3S)
                           , (B.b1S, B.b1S2N, B.b1S2N3C)
                           , (B.b1S, B.b1S2N, B.b1S2N3D)
                           , (B.b1S, B.b1S2N, B.b1S2N3H)
                           ]


sideSuit :: Situations
sideSuit = let
    sit (openerBid, j2ntBid, sideBid) = let
        suit = T.suitBid openerBid
        action = do
            setOpener T.South
            _ <- openerBid
            noInterference suit
            _ <- j2ntBid
            noInterference suit
        explanation =
            "Partner has bid Jacoby " .+ T.Bid 2 T.Notrump .+ ". We " .+
            "have a good 5-card side suit, so bid it at the 4 level. " .+
            "Partner can then decide whether to sign off in " .+ T.Bid 4 suit .+
            ", or investigate slam. Even if they decide to sign off, we " .+
            "can investigate slam ourselves if we have enough extra strength."
      in
        situation "side" action sideBid explanation
  in
    -- Partner must be an unpassed hand to be game-forcing.
    wrapSE $ return sit <~ [ (B.b1H, B.b1H2N, B.b1H2N4C)
                           , (B.b1H, B.b1H2N, B.b1H2N4D)
                           , (B.b1S, B.b1S2N, B.b1S2N4C)
                           , (B.b1S, B.b1S2N, B.b1S2N4D)
                           , (B.b1S, B.b1S2N, B.b1S2N4H)
                           ]


badSideSuit :: Situations
badSideSuit = let
    sit (openerBid, j2ntBid, otherSuits) dlr vul = let
        suit = T.suitBid openerBid
        sit' (longSuit, shortBid) = let
            action = do
                setOpener T.South
                _ <- openerBid
                noInterference suit
                _ <- j2ntBid
                noInterference suit
                minSuitLength longSuit 5
                forbid $ hasTopN longSuit 5 2
            explanation =
                "Partner has bid Jacoby " .+ T.Bid 2 T.Notrump .+ ". We " .+
                "have 5 " .+ show longSuit .+ ", but the suit quality is " .+
                "so bad that it is probably not a good source of tricks. " .+
                "Instead, just show our shortness at the 3 level."
          in
            situation "bad5" action shortBid explanation dlr vul
      in
        return sit' <~ otherSuits
  in
    wrap . join $ return sit
        <~ [ (B.b1H, B.b1H2N, [ (T.Diamonds, B.b1H2N3C)
                              -- With 5-5 in the majors, we'd open 1S instead
                              --, (T.Spades,   B.b1H2N3C)
                              , (T.Clubs,    B.b1H2N3D)
                              --, (T.Spades,   B.b1H2N3D)
                              , (T.Clubs,    B.b1H2N3S)
                              , (T.Diamonds, B.b1H2N3S)
                              ])
           , (B.b1S, B.b1S2N, [ (T.Diamonds, B.b1S2N3C)
                              , (T.Hearts,   B.b1S2N3C)
                              , (T.Clubs,    B.b1S2N3D)
                              , (T.Hearts,   B.b1S2N3D)
                              , (T.Clubs,    B.b1S2N3H)
                              , (T.Diamonds, B.b1S2N3H)
                              ])
           ]
        -- Partner must be an unpassed hand to be game-forcing.
        <~ [T.South, T.East]
        <~ T.allVulnerabilities


semibalancedMin :: Situations
semibalancedMin = let
    sit (openerBid, j2ntBid, openerRebid) = let
        suit = T.suitBid openerBid
        action = do
            setOpener T.South
            _ <- openerBid
            noInterference suit
            _ <- j2ntBid
            noInterference suit
        explanation =
            "Partner has bid Jacoby " .+ T.Bid 2 T.Notrump .+ ". We " .+
            "don't have length or shortness in a side suit to show, and " .+
            "are a minimum opener. Bid game directly to show this. Partner " .+
            "will likely pass, but might investigate slam with a monster."
      in
        situation "sbmin" action openerRebid explanation
  in
    -- Partner must be an unpassed hand to be game-forcing.
    wrapSE $ return sit <~ [ (B.b1H, B.b1H2N, B.b1H2N4H)
                           , (B.b1S, B.b1S2N, B.b1S2N4S)
                           ]


semibalancedMed :: Situations
semibalancedMed = let
    sit (openerBid, j2ntBid, openerRebid) = let
        suit = T.suitBid openerBid
        action = do
            setOpener T.South
            _ <- openerBid
            noInterference suit
            _ <- j2ntBid
            noInterference suit
        explanation =
            "Partner has bid Jacoby " .+ T.Bid 2 T.Notrump .+ ". We " .+
            "don't have length or shortness in a side suit to show, but " .+
            "we have a little extra strength we haven't shown yet. Bid " .+
            openerRebid .+ " to show this. Partner might sign off in " .+
            "game, but might start control bidding with slam interest."
      in
        situation "sbmed" action openerRebid explanation
  in
    -- Partner must be an unpassed hand to be game-forcing.
    wrapSE $ return sit <~ [ (B.b1H, B.b1H2N, B.b1H2N3N)
                           , (B.b1S, B.b1S2N, B.b1S2N3N)
                           ]


semibalancedMax :: Situations
semibalancedMax = let
    sit (openerBid, j2ntBid, openerRebid) = let
        suit = T.suitBid openerBid
        action = do
            setOpener T.South
            _ <- openerBid
            noInterference suit
            _ <- j2ntBid
            noInterference suit
        explanation =
            "Partner has bid Jacoby " .+ T.Bid 2 T.Notrump .+ ". We " .+
            "don't have length or shortness in a side suit to show, but " .+
            "we have enough extra strength to be interested in slam " .+
            "opposite partner's game force. Bid " .+ openerRebid .+ ", " .+
            "prompting partner to start control bidding. They might also " .+
            "bid Serious or Frivolous " .+ T.Bid 3 T.Notrump .+ ", if you " .+
            "play one of those."
      in
        situation "sbmax" action openerRebid explanation
  in
    -- Partner must be an unpassed hand to be game-forcing.
    wrapSE $ return sit <~ [ (B.b1H, B.b1H2N, B.b1H2N3H)
                           , (B.b1S, B.b1S2N, B.b1S2N3S)
                           ]


semibalSignoff :: Situations
semibalSignoff = let
    sit (openerBid, j2ntBid, openerRebid, responderSignoff) = let
        suit = T.suitBid openerBid
        action = do
            setOpener T.North
            _ <- openerBid
            noInterference suit
            _ <- j2ntBid
            noInterference suit
            _ <- openerRebid
            noInterference suit
        explanation =
            "We bid Jacoby " .+ T.Bid 2 T.Notrump .+ ". Partner has shown " .+
            "neither unusual shape nor enough extra strength for us to be " .+
            "interested in slam. Just sign off in game."
      in
        situation "sbso" action responderSignoff explanation
  in
    -- Partner must be an unpassed hand to be game-forcing.
    wrapNW $ return sit <~ [ (B.b1H, B.b1H2N, B.b1H2N4H, B.b1H2N4HP)
                           , (B.b1H, B.b1H2N, B.b1H2N3N, B.b1H2N3N4H)
                           , (B.b1S, B.b1S2N, B.b1S2N4S, B.b1S2N4SP)
                           , (B.b1S, B.b1S2N, B.b1S2N3N, B.b1S2N3N4S)
                           ]


semibalMinSlam :: Situations
semibalMinSlam = let
    sit (openerBid, j2ntBid, openerRebid, responderRebid) = let
        suit = T.suitBid openerBid
        action = do
            setOpener T.North
            _ <- openerBid
            noInterference suit
            _ <- j2ntBid
            noInterference suit
            _ <- openerRebid
            noInterference suit
        explanation =
            "We bid Jacoby " .+ T.Bid 2 T.Notrump .+ ". Partner has shown " .+
            "a semibalanced minimum, but we've got enough extra strength " .+
            "to be interested in slam anyway. Bid on! " .+
            if suit == T.Hearts  -- Acknowledge that Kickback exists.
            then "(If you use a bid other than " .+ responderRebid .+ " to " .+
                 "investigate slam, use that instead.)"
            else "" .+ ""
      in
        situation "sbminbw" action responderRebid explanation
  in
    -- Partner must be an unpassed hand to be game-forcing.
    wrapNW $ return sit <~ [ (B.b1H, B.b1H2N, B.b1H2N4H, B.b1H2N4H4N)
                           , (B.b1S, B.b1S2N, B.b1S2N4S, B.b1S2N4S4N)
                           ]


semibalMedSlam :: Situations
semibalMedSlam = let
    sit (openerBid, j2ntBid, openerRebid, responderRebid) = let
        suit = T.suitBid openerBid
        action = do
            setOpener T.North
            _ <- openerBid
            noInterference suit
            _ <- j2ntBid
            noInterference suit
            _ <- openerRebid
            noInterference suit
        explanation =
            "We bid Jacoby " .+ T.Bid 2 T.Notrump .+ ". Partner has shown " .+
            "a semibalanced hand slightly stronger than a minimum, and " .+
            "we've got enough extra strength to be interested in slam " .+
            "opposite that. Make a control bid."
      in
        situation "sbmedcb" action responderRebid explanation
  in
    -- Partner must be an unpassed hand to be game-forcing.
    wrapNW $ return sit
        <~ [ (B.b1H, B.b1H2N, B.b1H2N3N, B.b1H2N3N4C)
           , (B.b1H, B.b1H2N, B.b1H2N3N, B.b1H2N3N4D)
           -- Should there be one for control bidding spades when hearts are
           -- trump? It's very unlikely, and maybe you should just go straight
           -- to asking about keycards instead...
           , (B.b1S, B.b1S2N, B.b1S2N3N, B.b1S2N3N4C)
           , (B.b1S, B.b1S2N, B.b1S2N3N, B.b1S2N3N4D)
           , (B.b1S, B.b1S2N, B.b1S2N3N, B.b1S2N3N4H)
           ]


semibalMaxSlam :: Situations
semibalMaxSlam = let
    sit (openerBid, j2ntBid, openerRebid, responderRebid) = let
        suit = T.suitBid openerBid
        action = do
            setOpener T.North
            _ <- openerBid
            noInterference suit
            _ <- j2ntBid
            noInterference suit
            _ <- openerRebid
            noInterference suit
        explanation =
            "We bid Jacoby " .+ T.Bid 2 T.Notrump .+ ". Partner has shown " .+
            "a very strong, semibalanced hand with definite slam interest. " .+
            "Make a control bid to investigate slam. If you play Serious " .+
            "or Frivolous " .+ T.Bid 3 T.Notrump .+ ", you might bid that, " .+
            "instead."
      in
        situation "sbmaxcb" action responderRebid explanation
  in
    -- Partner must be an unpassed hand to be game-forcing.
    wrapNW $ return sit
        -- Although it's possible to have no controls and want to rebid 4M, it
        -- is exceedingly unlikely. Skip those situations because they're so
        -- rare.
        <~ [ (B.b1H, B.b1H2N, B.b1H2N3H, B.b1H2N3H3S)
           , (B.b1H, B.b1H2N, B.b1H2N3H, B.b1H2N3H4C)
           , (B.b1H, B.b1H2N, B.b1H2N3H, B.b1H2N3H4D)
           --, (B.b1H, B.b1H2N, B.b1H2N3H, B.b1H2N3H4H)
           , (B.b1S, B.b1S2N, B.b1S2N3S, B.b1S2N3S4C)
           , (B.b1S, B.b1S2N, B.b1S2N3S, B.b1S2N3S4D)
           , (B.b1S, B.b1S2N, B.b1S2N3S, B.b1S2N3S4H)
           --, (B.b1S, B.b1S2N, B.b1S2N3S, B.b1S2N3S4S)
           ]


-- TODO:
-- - follow-ups after opener rebids a 5-card side suit
-- - follow-ups after opener rebids a singleton
-- - what if hearts are trump and opener has a 5-card spade suit?
-- - what if hearts are trump, opener rebids 3N, and responder has slam interest
--     but no minor-suit control?
-- - what if opener has 5 spades and 6 hearts? Do you rebid 4S, or show a
--     singleton at the 3 level?


topic :: Topic
topic = makeTopic ("Jacoby ".+ T.Bid 2 T.Notrump)  "J2NT" $
    wrap [ j2nt
         , singleton
         , wrap [sideSuit, sideSuit, badSideSuit]
         , wrap [semibalancedMin, semibalancedMed, semibalancedMax]
         , semibalSignoff
         , wrap [semibalMinSlam, semibalMedSlam, semibalMaxSlam]
         ]
