module Topics.StandardModernPrecision.Lampe(topic) where

import Bids.StandardModernPrecision.BasicBids(setOpener)
import qualified Bids.StandardModernPrecision.Lampe as B
import CommonBids(noInterference)
import EDSL(alternatives, minSuitLength, forbid, makeCall)
import Output((.+), Punct(..))
import Situation(situation, (<~))
import qualified Terminology as T
import Topic(Topic, wrap, wrapNW, wrapSE, stdWrapSE, Situations, makeTopic,
             wrapDlr)


majorSuitImmediateResponse :: Situations
majorSuitImmediateResponse = let
    sit answer hasMinor = let
        longMinor = alternatives [ minSuitLength T.Clubs    5
                                 , minSuitLength T.Diamonds 5 ]
        action = do
            setOpener T.North
            B.b1D
            noInterference T.Diamonds
            if hasMinor then longMinor else forbid longMinor
        explanation =
            "Partner opened an ambiguous " .+ T.Bid 1 T.Diamonds .+ ". " .+
            "Bid our major suit naturally, and we'll go from there. " .+
            if hasMinor then "We can make a canap" .+ EAcute .+ " bid to " .+
                             "show our long minor afterwards."
                        else "" .+ ""  -- Get the types right
      in
        situation "1M" action answer explanation
  in
    wrapDlr $ return sit <~ [B.b1D1H, B.b1D1S] <~ [True, False]


clubCanape :: Situations
clubCanape = let
    sit (responderBid, openerRebid, responderRebid) = let
        action = do
            setOpener T.North
            B.b1D
            noInterference T.Diamonds
            _ <- responderBid
            noInterference T.Diamonds  -- TODO: forbid 2-suited takeouts, too
            _ <- openerRebid
            makeCall T.Pass
        explanation =
            "Partner opened an ambiguous " .+ T.Bid 1 T.Diamonds .+ ". " .+
            "We started by bidding our major suit naturally, but did " .+
            "not find a fit with partner. XYZ would be on, but instead " .+
            "make a canap" .+ EAcute .+ " bid to show our long clubs. " .+
            "Partner will relay to " .+ T.Bid 3 T.Clubs .+ ", after which " .+
            "we can pass with a weak hand, or bid on with a game-forcing " .+
            "hand. We already have a very good sense of partner's " .+
            "strength, so we can't really have an invitational hand any more."
      in
        situation "ccan" action responderRebid explanation
  in
    wrapNW $ return sit <~ [ (B.b1D1H, B.b1D1H1S, B.b1D1H1S2N)
                           , (B.b1D1H, B.b1D1H1N, B.b1D1H1N2N)
                           , (B.b1D1S, B.b1D1S1N, B.b1D1S1N2N)
                           ]


diamondCanape :: Situations
diamondCanape = let
    sit (responderBid, openerRebid, responderRebid) = let
        action = do
            setOpener T.North
            B.b1D
            noInterference T.Diamonds
            _ <- responderBid
            noInterference T.Diamonds  -- TODO: forbid 2-suited takeouts, too
            _ <- openerRebid
            makeCall T.Pass
        explanation =
            "Partner opened an ambiguous " .+ T.Bid 1 T.Diamonds .+ ". " .+
            "We started by bidding our major suit naturally, but did " .+
            "not find a fit with partner. XYZ would be on, but instead " .+
            "make a canap" .+ EAcute .+ " bid to show our long diamonds: " .+
            T.Bid 3 T.Clubs .+ " shows diamonds and a 4-card major, while " .+
            T.Bid 3 T.Diamonds .+ " shows diamonds and at least a 5-card " .+
            "major. Both bids are game forcing."

      in
        situation "dcan" action responderRebid explanation
  in
    wrapNW $ return sit <~ [ (B.b1D1H, B.b1D1H1S, B.b1D1H1S3C)
                           , (B.b1D1H, B.b1D1H1N, B.b1D1H1N3C)
                           , (B.b1D1S, B.b1D1S1N, B.b1D1S1N3C)
                           , (B.b1D1H, B.b1D1H1S, B.b1D1H1S3D)
                           , (B.b1D1H, B.b1D1H1N, B.b1D1H1N3D)
                           , (B.b1D1S, B.b1D1S1N, B.b1D1S1N3D)
                           ]


twoClubs :: Situations
twoClubs = let
    action = do
        setOpener T.North
        B.b1D
        noInterference T.Diamonds
    explanation =
        "With at least invitational strength and a minor, start with an " .+
        "artificial " .+ T.Bid 2 T.Clubs .+ "."
  in
    wrapNW . return $ situation "2C" action B.b1D2C explanation


twoClubsUnbal :: Situations
twoClubsUnbal = let
    action = do
        setOpener T.South
        B.b1D
        noInterference T.Diamonds
        B.b1D2C
        noInterference T.Diamonds
    explanation =
        "With an unbalanced minimum, rebid an artificial " .+
        T.Bid 2 T.Diamonds .+ "."
  in
    stdWrapSE $ situation "2CUnbal" action B.b1D2C2D explanation


twoClubsBal :: Situations
twoClubsBal = let
    action = do
        setOpener T.South
        B.b1D
        noInterference T.Diamonds
        B.b1D2C
        noInterference T.Diamonds
    explanation =
        "With a balanced minimum, rebid an artificial " .+
        T.Bid 2 T.Hearts .+ "."
  in
    stdWrapSE $ situation "2CBal" action B.b1D2C2H explanation


twoClubsSS :: Situations
twoClubsSS = let
    sit answer = let
        action = do
            setOpener T.South
            B.b1D
            noInterference T.Diamonds
            B.b1D2C
            noInterference T.Diamonds
        explanation =
            "We opened an ambiguous " .+ T.Bid 1 T.Diamonds .+ ", and " .+
            "partner has shown an invitational or better hand with a minor. " .+
            "With a maximum, make a shape-shower bid immediately. Partner " .+
            "is invitational or better, and this bid indicates we would " .+
            "accept an invitation, so is game-forcing."
      in
        situation "2CSS" action answer explanation
  in
    -- For us to bid a forcing 1N, we must be an unpassed hand.
    wrapSE $ return sit
        <~ [B.b1D2C2S, B.b1D2C2N, B.b1D2C3C, B.b1D2C3D, B.b1D2C3H, B.b1D2C3S]


twoClubsUnbalGf :: Situations
twoClubsUnbalGf = let
    action = do
        setOpener T.North
        B.b1D
        noInterference T.Diamonds
        B.b1D2C
        noInterference T.Diamonds
        B.b1D2C2D
        noInterference T.Diamonds
    explanation =
        "Partner has an unbalanced minimum, but we're game forcing. Bid " .+
        "an artificial " .+ B.b1D2C2D2H .+ " to ask partner for a " .+
        "shape-shower. This will let us find the right suit to play " .+
        "in, or perhaps " .+ T.Bid 3 T.Notrump .+ "."
  in
    wrapNW . return $ situation "unbalGF" action B.b1D2C2D2H explanation


twoClubsUnbalSS :: Situations
twoClubsUnbalSS = let
    sit answer = let
        action = do
            setOpener T.South
            B.b1D
            noInterference T.Diamonds
            B.b1D2C
            noInterference T.Diamonds
            B.b1D2C2D
            noInterference T.Diamonds
            B.b1D2C2D2H
            noInterference T.Diamonds
        explanation =
            "We've shown an unbalanced minimum, but partner is " .+
            "game-forcing anyway. Make a shape-shower bid, and they can " .+
            "place the final contract from there."
      in
        situation "uGFSS" action answer explanation
  in
    -- For us to bid a forcing 1N, we must be an unpassed hand.
    wrapSE $ return sit <~ [ B.b1D2C2D2H2S
                           , B.b1D2C2D2H2N
                           , B.b1D2C2D2H3C
                           , B.b1D2C2D2H3D
                           -- We're a minimum, and we only open 1D with a
                           -- 5-card suit if were a maximum. So, the other
                           -- shape-showers don't exist.
                           --, B.b1D2C2D2H3H
                           --, B.b1D2C2D2H3S
                           ]



-- TODO:
--     all of responder's rebids after opener rebids 2D or 2H
--     all of opener's rebids to the Reverse-Flannery-like 2D
--     rethink responder's hands with a 6-card minor and 4- or 5-card major  
--     consider using loser count insead of HCPs to help opener decide whether
--         they're a minimum or maximum


twoDiamonds :: Situations
twoDiamonds = let
    action = do
        setOpener T.North
        B.b1D
        noInterference T.Diamonds
    explanation =
        "With both majors and less than game forcing strength, make a " .+
        "Reverse-Flannery-like " .+ T.Bid 2 T.Diamonds .+ " bid."
  in
    wrapNW . return $ situation "2D" action B.b1D2D explanation


topic :: Topic
topic = makeTopic summary "Lampe" situations
  where
    summary = "Lampe responses to Precision 1" .+ T.Diamonds
    situations = wrap [ twoClubs
                      , twoClubsUnbal
                      , twoClubsBal
                      , twoClubsSS
                      , twoClubsUnbalGf
                      , twoDiamonds
                      , twoClubsUnbalSS
                      , majorSuitImmediateResponse
                      , wrap [clubCanape, diamondCanape]
                      ]
