module Topics.Jacoby2NT(topic) where

import qualified Bids.Jacoby2NT as B
import CommonBids(setOpener, noInterference)
import Output((.+))
import Situation(situation, (<~))
import qualified Terminology as T
import Topic(wrap, wrapVulNW, wrapVulSE, Situations, Topic, makeTopic)


j2nt :: Situations
j2nt = let
    sit (openerBid, j2ntBid, suit) = let
        action = do
            setOpener T.North
            _ <- openerBid
            noInterference suit
        explanation =
            "We've got game-forcing strength with at least 4-card support " .+
            "for partner's " .+ show suit .+ ". Bid Jacoby " .+
            T.Bid 2 T.Notrump .+ " to show this. Now that we've set trump " .+
            "and entered a game-forcing auction,  we'll figure out whether " .+
            "we belong in game or slam."
      in
        situation "j2n" action j2ntBid explanation
  in
    -- We must be an unpassed hand to be game-forcing.
    wrapVulNW $ return sit <~ [ (B.b1H, B.b1H2N, T.Hearts)
                              , (B.b1S, B.b1S2N, T.Spades) ]


singleton :: Situations
singleton = let
    sit (openerBid, j2ntBid, singletonBid, suit) = let
        action = do
            setOpener T.South
            _ <- openerBid
            noInterference suit
            _ <- j2ntBid
            noInterference suit
        explanation =
            "Partner has bid Jacoby " .+ T.Bid 2 T.Notrump .+ ". We don't " .+
            "have a good 5-card side suit to jump into, but we do have " .+
            "shortness to bid at the cheapest level. This will help " .+
            "partner decide whether to sign off in " .+ T.Bid 4 suit .+
            ", or start control bidding to investigate slam. If you play " .+
            "Serious or Frivolous " .+ T.Bid 3 T.Notrump .+ ", they might " .+
            "also bid that. If partner tries signing off, we can look for " .+
            "slam anyway if we have enough extra strength."
      in
        situation "sing" action singletonBid explanation
  in
    -- Partner must be an unpassed hand to be game-forcing.
    wrapVulSE $ return sit <~ [ (B.b1H, B.b1H2N, B.b1H2N3C, T.Hearts)
                              , (B.b1H, B.b1H2N, B.b1H2N3D, T.Hearts)
                              , (B.b1H, B.b1H2N, B.b1H2N3S, T.Hearts)
                              , (B.b1S, B.b1S2N, B.b1S2N3C, T.Spades)
                              , (B.b1S, B.b1S2N, B.b1S2N3D, T.Spades)
                              , (B.b1S, B.b1S2N, B.b1S2N3H, T.Spades)
                              ]


sideSuit :: Situations
sideSuit = let
    sit (openerBid, j2ntBid, sideBid, suit) = let
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
    wrapVulSE $ return sit <~ [ (B.b1H, B.b1H2N, B.b1H2N4C, T.Hearts)
                              , (B.b1H, B.b1H2N, B.b1H2N4D, T.Hearts)
                              , (B.b1S, B.b1S2N, B.b1S2N4C, T.Spades)
                              , (B.b1S, B.b1S2N, B.b1S2N4D, T.Spades)
                              , (B.b1S, B.b1S2N, B.b1S2N4H, T.Spades)
                              ]


semibalancedMin :: Situations
semibalancedMin = let
    sit (openerBid, j2ntBid, openerRebid, suit) = let
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
    wrapVulSE $ return sit <~ [ (B.b1H, B.b1H2N, B.b1H2N4H, T.Hearts)
                              , (B.b1S, B.b1S2N, B.b1S2N4S, T.Spades)
                              ]


semibalancedMed :: Situations
semibalancedMed = let
    sit (openerBid, j2ntBid, openerRebid, suit) = let
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
    wrapVulSE $ return sit <~ [ (B.b1H, B.b1H2N, B.b1H2N3N, T.Hearts)
                              , (B.b1S, B.b1S2N, B.b1S2N3N, T.Spades)
                              ]


semibalancedMax :: Situations
semibalancedMax = let
    sit (openerBid, j2ntBid, openerRebid, suit) = let
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
            "prompting partner to start control bidding (they might also " .+
            "bid Serious or Frivolous " .+ T.Bid 3 T.Notrump .+ ", if you " .+
            "play one of those)."
      in
        situation "sbmax" action openerRebid explanation
  in
    -- Partner must be an unpassed hand to be game-forcing.
    wrapVulSE $ return sit <~ [ (B.b1H, B.b1H2N, B.b1H2N3H, T.Hearts)
                              , (B.b1S, B.b1S2N, B.b1S2N3S, T.Spades)
                              ]


semibalSignoff :: Situations
semibalSignoff = let
    sit (openerBid, j2ntBid, openerRebid, responderSignoff, suit) = let
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
    wrapVulNW $ return sit
        <~ [ (B.b1H, B.b1H2N, B.b1H2N4H, B.b1H2N4HP,  T.Hearts)
           , (B.b1H, B.b1H2N, B.b1H2N3N, B.b1H2N3N4H, T.Hearts)
           , (B.b1S, B.b1S2N, B.b1S2N4S, B.b1S2N4SP,  T.Spades)
           , (B.b1S, B.b1S2N, B.b1S2N3N, B.b1S2N3N4S, T.Spades)
           ]


semibalMinSlam :: Situations
semibalMinSlam = let
    sit (openerBid, j2ntBid, openerRebid, responderRebid, suit) = let
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
            else ""
      in
        situation "sbso" action responderRebid explanation
  in
    -- Partner must be an unpassed hand to be game-forcing.
    wrapVulNW $ return sit
        <~ [ (B.b1H, B.b1H2N, B.b1H2N4H, B.b1H2N4H4N,  T.Hearts)
           , (B.b1S, B.b1S2N, B.b1S2N4S, B.b1S2N4S4N,  T.Spades)
           ]


topic :: Topic
topic = makeTopic ("Jacoby ".+ T.Bid 2 T.Notrump)  "J2NT" $
    wrap [ j2nt
         , singleton
         , sideSuit
         , wrap [semibalancedMin, semibalancedMed, semibalancedMax]
         , semibalSignoff
         ]
