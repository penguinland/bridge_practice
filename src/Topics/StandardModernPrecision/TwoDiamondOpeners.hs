module Topics.StandardModernPrecision.TwoDiamondOpeners(topic) where

import Action(Action, constrain)
import qualified Bids.StandardModernPrecision.TwoDiamonds as B
import CommonBids(cannotPreempt, setOpener, takeoutDouble)
import EDSL(forbid, pointRange, suitLength, minSuitLength, maxSuitLength,
            alternatives, makePass, makeCall, makeAlertableCall)
import Output((.+))
import Situation(situation, (<~))
import qualified Terminology as T
import Topic(Topic, wrap, stdWrap, wrapVulDlr, Situations, makeTopic)


-- TODO: Refactor into a proper list of alertable bids, so that the solutions to
-- the situations can be self-alerted, too.


noDirectOvercall :: Action
noDirectOvercall = do
    cannotPreempt
    alternatives [
        pointRange 0 10  -- Not enough to overcall
      , pointRange 11 16 >>  -- Enough to overcall, but no suit
            constrain "no_suit" ["shape(", ", any 4333 + any 4432)"]]
    makePass


bestFitSpades :: Action
bestFitSpades = do
    minSuitLength T.Spades   4
    maxSuitLength T.Hearts   3
    maxSuitLength T.Diamonds 6
    maxSuitLength T.Clubs    3


bestFitClubs :: Action
bestFitClubs = do
    maxSuitLength T.Spades   3
    maxSuitLength T.Hearts   3
    maxSuitLength T.Diamonds 6
    minSuitLength T.Clubs    4
    forbid $ constrain "s3334" ["shape(", ", 3334)"]  -- maybe bid H instead


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


immediateSignoffSpades3 :: Situations
immediateSignoffSpades3 = let
    action = do
        setOpener T.North
        suitLength T.Spades 3
        B.b2D
        noDirectOvercall
        pointRange 0 9
        bestFitSpades
        suitLength T.Spades 4
    explanation =
        "Without the strength for a game contract, sign off in " .+
        T.Bid 2 T.Spades .+ " with a likely fit. If you're stuck playing a " .+
        "4-3 fit, oh well."
  in
    stdWrap $ situation "S43" action (makeCall $ T.Bid 2 T.Spades) explanation


immediateSignoffSpades4 :: Situations
immediateSignoffSpades4 = let
    action = do
        setOpener T.North
        suitLength T.Spades 4
        B.b2D
        noDirectOvercall
        pointRange 0 9
        bestFitSpades
        suitLength T.Spades 4
    explanation =
        "Without the strength for a game contract, sign off in " .+
        T.Bid 2 T.Spades .+ " with a likely fit."
  in
    stdWrap $ situation "S44" action (makeCall $ T.Bid 2 T.Spades) explanation


immediateSignoffSpades5 :: Situations
immediateSignoffSpades5 = let
    sit = let
        action = do
            setOpener T.North
            B.b2D
            noDirectOvercall
            pointRange 0 6  -- With 7-9 HCP, make a mixed raise!
            bestFitSpades
            minSuitLength T.Spades 5
        explanation =
            "Without the strength for a game contract, sign off in " .+
            T.Bid 2 T.Spades .+ " with a guaranteed fit."
      in
        situation "Sfit" action (makeCall $ T.Bid 2 T.Spades) explanation
  in
    -- Although this can happen when anyone is dealer, it is very rare when East
    -- deals, and generating those hands is difficult for dealer (often
    -- requiring over 1,000,000 hands to be generated). Consequently, we limit
    -- these situations to times when East isn't dealer.
    wrap $ return sit <~ T.allVulnerabilities <~ [T.North, T.South, T.West]


passSignoff2Spades :: Situations
passSignoff2Spades = let
    sit spadeLength = let
        action = do
            setOpener T.South
            B.b2D
            noDirectOvercall
            bestFitSpades
            alternatives [    suitLength T.Spades 4 >> pointRange 0 9
                         , minSuitLength T.Spades 5 >> pointRange 0 6
                         ]
            makeAlertableCall (T.Bid 2 T.Spades) "signoff"
            forbid $ takeoutDouble T.Spades
            noDirectOvercall
            suitLength T.Spades spadeLength
        explanation =
            "Partner has less-than-invitational values and is signing off. " .+
            "Just pass" .+
            (if spadeLength == 4 then "." else
                ", even though you might be in a 7-card fit.")
      in
        situation "2SP" action (makeCall T.Pass) explanation
  in
    wrapVulDlr $ return sit <~ [3, 4]


immediateSignoffClubs :: Situations
immediateSignoffClubs = let
    action = do
        setOpener T.North
        B.b2D
        noDirectOvercall
        bestFitClubs
        pointRange 0 9
    explanation =
        "Without the strength to invite to game, sign off in a club " .+
        "partial."
  in
    stdWrap $ situation "3C" action (makeCall $ T.Bid 3 T.Clubs) explanation


passSignoffClubs :: Situations
passSignoffClubs = let
    action = do
        setOpener T.South
        B.b2D
        noDirectOvercall
        bestFitClubs
        pointRange 0 9
        makeAlertableCall (T.Bid 3 T.Clubs) "signoff"
        forbid $ takeoutDouble T.Clubs
        noDirectOvercall
    explanation =
        "Partner has less-than-invitational values and is signing off. " .+
        "Just pass."
  in
    stdWrap $ situation "3CP" action (makeCall T.Pass) explanation


immediateSignoffHearts :: Situations
immediateSignoffHearts = let
    sit (bid, direction) vul = let
        action = do
            setOpener T.North
            B.b2D
            noDirectOvercall
        explanation =
            "Without the strength to invite to game, sign off in " .+
            bid .+ ". Remember that opener might " .+
            "pull the bid to " .+ T.Bid 2 T.Spades .+ " with " .+
            "exactly 4315 shape."
      in
        situation "2H" action bid explanation vul direction
  in
    -- A new suit being nonforcing is alertable only if you're an unpassed hand,
    -- so we have different bids depending on who the dealer is.
    wrap $ return sit <~ [ (B.b2D2H, T.North)
                         , (B.b2D2H, T.West)
                         , (B.bP2D2H, T.East)
                         , (B.bP2D2H, T.South)
                         ]
                      <~ T.allVulnerabilities


passSignoffHearts :: Situations
passSignoffHearts = let
    sit (bid, direction) vul = let
        action = do
            setOpener T.South
            B.b2D
            noDirectOvercall
            _ <- bid
            noDirectOvercall
            forbid B.b2D2H2S
        explanation =
            "Partner has less-than-invitational values and is signing off. " .+
            "Given that you have 4 hearts, pass."
      in
        situation "2H2S" action (makeCall T.Pass) explanation vul direction
  in
    wrap $ return sit <~ [ (B.b2D2H, T.South)
                         , (B.b2D2H, T.East)
                         , (B.bP2D2H, T.West)
                         , (B.bP2D2H, T.North)
                         ]
                      <~ T.allVulnerabilities


correctSignoffHearts :: Situations
correctSignoffHearts = let
    sit (bid, direction) responderHeartLength vul = let
        action = do
            setOpener T.South
            B.b2D
            noDirectOvercall
            suitLength T.Hearts responderHeartLength
            _ <- bid
            noDirectOvercall
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
    wrap $ return sit <~ [ (B.b2D2H, T.South)
                         , (B.b2D2H, T.East)
                         , (B.bP2D2H, T.West)
                         , (B.bP2D2H, T.North)
                         ]
                      -- We want to commonly show times when responder has just
                      -- 3 hearts (and we're avoiding a 3-3 fit) and times when
                      -- responder has a real heart suit.
                      <~ [3, 5]
                      <~ T.allVulnerabilities


topic :: Topic
topic = makeTopic description "SMP2D" situations
  where
    description = ("SMP " .+ T.Bid 2 T.Diamonds .+ " auctions")
    situations = wrap [ open
                      , wrap [ immediateSignoffSpades3
                             , immediateSignoffSpades4
                             , immediateSignoffSpades5]
                      , passSignoff2Spades
                      , immediateSignoffClubs
                      , passSignoffClubs
                      , immediateSignoffHearts
                      , wrap [ passSignoffHearts
                             , correctSignoffHearts]
                      ]
