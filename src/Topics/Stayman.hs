module Topics.Stayman(topic) where

-- TODO: replace makePass with something more intelligent
import Auction(makePass, pointRange, suitLength, maxSuitLength)
import CommonBids(setOpener)
import Output((.+), Punct(..))
import Situation(situation, (<~))
import qualified Terminology as T
import Topic(Topic, stdWrap, wrap, wrapVulDlr, Situations, makeTopic)
import qualified Bids.OneNotrump as B


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
            "accordingly. Pass whatever partner does: if you find an 8-card " .+
            "major fit, that will likely play better than stopping in " .+
            T.Bid 1 T.Notrump .+ ". and if partner has neither major, " .+
            "they're likely to have diamonds for you. It's possible partner " .+
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
            "Stayman, and see if you've got a fit with partner."
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
    sit (bid, shortSuit) = let
        action = do
            setOpener T.South
            B.b1N
            makePass
            B.b1N2C
            makePass
            maxSuitLength shortSuit 3
        explanation =
            "We opened a strong " .+ T.Bid 1 T.Notrump .+ ", and partner " .+
            "has bid Stayman, asking whether we have any major suits. " .+
            "We've got one, so bid it. Partner is captain of " .+
            "the auction; they'll know what to do next."
      in situation "1Maj" action bid explanation
  in
    wrapVulDlr $ return sit <~ [(B.b1N2C2H, T.Spades), (B.b1N2C2S, T.Hearts)]


bothMajors :: Situations
bothMajors = let
    sit = let
        action = do
            setOpener T.South
            B.b1N
            makePass
            B.b1N2C
            makePass
            mapM_ (`suitLength` 4) T.majorSuits
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
    wrap $ return sit <~ T.allVulnerabilities <~ [T.North, T.West]


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
    wrapVulDlr $ return sit <~ [ (B.b1N2C2H, B.b1N2C2H3H)
                               , (B.b1N2C2S, B.b1N2C2S3S) ]


fitGf :: Situations
fitGf = let
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
    wrapVulDlr $ return sit <~ [ (B.b1N2C2H, B.b1N2C2H4H)
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
            "will start control bidding."
      in situation "fitSl" action responderRebid explanation
  in
    -- We're slam invitational; we can't be a passed hand.
    wrap $ return sit <~ [ (B.b1N2C2H, B.b1N2C2H3S)
                         , (B.b1N2C2S, B.b1N2C2S3H) ]
                      <~ T.allVulnerabilities <~ [T.North, T.West]


-- TODO:
--   - opener has both majors, and tries the other one after 3N
--   - responder bids another side suit, GF, after there's no fit
--   - responder bids their major at the 2 level, showing 5-4 shape and
--     invitational strength
--   - opener has both majors, responder bids another side suit after 2H, and
--     opener should go to 4S.
--   - responder bids Texas over 2D
--   - opener completes Texas
--   - DON'T DO Smolen: that's a separate topic
--   - DON'T DO opener having both majors and responder inviting with 2N over
--     2H. If you're playing 4-way transfers, that doesn't necessarily show
--     spades.


topic :: Topic
topic = makeTopic "Stayman" "Stmn" situations
  where
    situations = wrap [ garbageStayman
                      , nongarbageStayman
                      , noMajor
                      , oneMajor
                      , bothMajors
                      , noFitBalancedInv
                      , noFitBalancedGf
                      , noFitBalancedSlamInv
                      , fitInvite
                      , fitGf
                      , fitSlam
                      ]
