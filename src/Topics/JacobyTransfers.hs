module Topics.JacobyTransfers(topic) where

import Action(Action, constrain)
import qualified Bids.OneNotrump as B
import CommonBids(setOpener)
import EDSL(forbid, pointRange, suitLength, balancedHand, flatHand,
            minSuitLength)
import Output((.+))
import Situation(situation, (<~))
import qualified Terminology as T
import Topic(wrap, stdWrap, stdWrapNW, wrapVulDlr, wrapVulNW, Situations,
             Topic, makeTopic)


-- syntactic sugar for writing descriptions of solutions
equalMajors :: Action
equalMajors = constrain "equal_majors" ["hearts(", ") == spades(", ")"]


-- TODO:
--   - Smolen in a separate topic
--   - slam invite with a 6-card suit (should this be in Texas transfers?)


initiateTransferWeak :: Situations
initiateTransferWeak = let
    sit bid = let
        action = do
            setOpener T.North
            B.b1N
            B.noInterference
            pointRange 0 7
            forbid equalMajors  -- With 5-5 in the majors, pick the better one.
        explanation =
            "Partner has opened a strong " .+ B.b1N .+ ". We have " .+
            "such a weak hand that we have no interest in game, but we do " .+
            "have a 5-card major. Playing in it, even if it's a 5-2 fit, is " .+
            "more likely to succeed than playing in notrump. Make a Jacoby " .+
            "transfer into the suit, then pass and leave partner at the 2 " .+
            "level."
      in
        situation "InitWeak" action bid explanation
  in
    wrapVulDlr $ return sit <~ [B.b1N2D, B.b1N2H]


initiateTransferBInv :: Situations
initiateTransferBInv = let
    sit (bid, suit) = let
        action = do
            setOpener T.North
            B.b1N
            B.noInterference
            pointRange 8 9
            forbid equalMajors
            balancedHand
        explanation =
            "Partner has opened a strong " .+ B.b1N .+ ". We have " .+
            "a balanced hand with invitational strength, and a 5-card " .+
            "major. Make a Jacoby transfer into the suit, then bid " .+
            T.Bid 2 T.Notrump .+ ". This gives partner the " .+
            "options of playing in notrump with 2-card " .+ init (show suit) .+
            " support or in " .+ show suit .+ " with a fit, and the option " .+
            "of playing in partscore with a minimum hand and game with a " .+
            "maximum."
      in
        situation "InitBInv" action bid explanation
  in
    wrapVulDlr $ return sit <~ [(B.b1N2D, T.Hearts), (B.b1N2H, T.Spades)]


initiateTransferBGf :: Situations
initiateTransferBGf = let
    sit (bid, suit) = let
        action = do
            setOpener T.North
            B.b1N
            B.noInterference
            pointRange 10 14
            balancedHand
        explanation =
            "Partner has opened a strong " .+ B.b1N .+ ". We have " .+
            "a balanced hand with game-going but not slam-going strength, " .+
            "and a 5-card major. Make a Jacoby transfer into the suit, " .+
            "then bid " .+ T.Bid 3 T.Notrump .+ ". This gives partner the " .+
            "options of passing and playing in notrump with 2-card " .+
            init (show suit) .+ " support or correcting to " .+ T.Bid 4 suit .+
            " with a fit, knowing that we belong in game but not slam."
      in
        situation "InitBGF" action bid explanation
  in
    wrapVulDlr $ return sit <~ [(B.b1N2D, T.Hearts), (B.b1N2H, T.Spades)]


completeTransfer :: Situations
completeTransfer = let
    sit (responderBid, openerRebid) = let
        action = do
            setOpener T.South
            B.b1N
            B.noInterference
            _ <- responderBid
            B.noInterference  -- TODO: Allow overcalls of lower suits
        explanation =
            "We have opened a strong " .+ B.b1N .+ ", and partner " .+
            "has made a Jacoby transfer. Complete the transfer by bidding " .+
            "the next higher suit. Partner promises at least 5 cards in " .+
            "that major, and will describe their hand further (possibly by " .+
            "passing with a weak hand) afterward."
      in
        situation "Complete" action openerRebid explanation
  in
    wrapVulDlr $ return sit <~ [(B.b1N2D, B.b1N2D2H), (B.b1N2H, B.b1N2H2S)]


completeTransferShort :: Situations
completeTransferShort = let
    sit (responderBid, openerRebid) = let
        suit = T.suitBid openerRebid
        action = do
            setOpener T.South
            B.b1N
            B.noInterference
            _ <- responderBid
            B.noInterference  -- TODO: Allow overcalls of lower suits
            suitLength suit 2
        explanation =
            "We have opened a strong " .+ B.b1N .+ ", and partner " .+
            "has made a Jacoby transfer, indicating they have at least 5 " .+
            show suit .+ ". Even though we only have 2-card support, " .+
            "complete the transfer by bidding the next higher suit. We " .+
            "have at least a 7-card fit. If partner is very weak, " .+
            T.Bid 2 suit .+ " rates to play better than notrump, " .+
            "and we want to be declarer so that our strong hand stays " .+
            "hidden. If partner has at least invitational strength, " .+
            "they will make another bid to give us options of where to play."
      in
        situation "Short" action openerRebid explanation
  in
    wrapVulDlr $ return sit <~ [(B.b1N2D, B.b1N2D2H), (B.b1N2H, B.b1N2H2S)]


majors55inv :: Situations
majors55inv = let
    action = do
        setOpener T.North
        B.b1N
        B.noInterference
        suitLength T.Hearts 5
        suitLength T.Spades 5
        pointRange 7 9
    explanation =
        "Partner has opened a strong " .+ B.b1N .+ ". With 5-5 in the " .+
        "majors and invitational strength, first make a Jacoby transfer " .+
        "into hearts, and then bid " .+ T.Bid 2 T.Spades .+ " afterwards. " .+
        "Partner will then have the options of passing " .+
        T.Bid 2 T.Spades .+ " with a minimum hand and a spade " .+
        "fit, bidding " .+ T.Bid 3 T.Hearts .+ " with a minimum " .+
        "hand and no spade fit (in which case a heart fit is guaranteed), " .+
        "or bidding one of the majors at the 4 level with a maximum. This " .+
        "wrong-sides the contract if we end up playing in spades."
  in
    stdWrap $ situation "55Inv" action B.b1N2D explanation


majors55gf :: Situations
majors55gf = let
    sit = let
        action = do
            setOpener T.North
            B.b1N
            B.noInterference
            suitLength T.Hearts 5
            suitLength T.Spades 5
            pointRange 10 14
        explanation =
            "Partner has opened a strong " .+ B.b1N .+ ". With 5-" .+
            "5 in the majors and " .+
            "game-forcing strength, first make a Jacoby transfer into " .+
            "spades, and then bid " .+ T.Bid 3 T.Hearts .+ " afterwards. " .+
            "Partner will then have the options of which game to bid. " .+
            "This wrong-sides the contract if we end up playing in hearts."
      in
        situation "55GF" action B.b1N2H explanation
  in
    -- Note that with 5-5 and game-going strength, we would have opened the
    -- bidding if we had a chance (needing only 10 HCP on the rule of 20). So,
    -- we must not have had a chance to bid before partner.
    stdWrapNW sit


majors55inv2 :: Situations
majors55inv2 = let
    sit = let
        action = do
            setOpener T.North
            B.b1N
            B.noInterference
            B.b1N2D
            B.noInterference
            B.b1N2D2H
            B.noInterference
        explanation =
            "Partner has opened a strong " .+ B.b1N .+ ". With 5-" .+
            "5 in the majors and " .+
            "game-forcing strength, we first made a Jacoby transfer into " .+
            "spades, and now bid " .+ T.Bid 2 T.Spades .+ " afterwards. " .+
            "We definitely have a major-suit fit somewhere: partner has " .+
            "the options of passing with a spade fit and a minimum, " .+
            "correcting back to " .+ T.Bid 3 T.Hearts .+ " with a heart " .+
            "fit and a minimum, or bidding either major-suit game with " .+
            "that fit and a maximum."
      in
        situation "55inv2" action B.b1N2D2H2S explanation
  in
    stdWrap sit


majors55gf2 :: Situations
majors55gf2 = let
    sit = let
        action = do
            setOpener T.North
            B.b1N
            B.noInterference
            B.b1N2H
            B.noInterference
            B.b1N2H2S
            B.noInterference
        explanation =
            "Partner has opened a strong " .+ B.b1N .+ ". With 5-5 in the " .+
            "majors and game-forcing strength, we started with a Jacoby " .+
            "transfer into spades, and then bid " .+ T.Bid 3 T.Hearts .+
            " afterwards. By bidding a new suit at the 3 level, we're " .+
            "definitely game-forcing, and partner knows we're 5-5 because " .+
            "if we were only 5-4 we would have bid Stayman/Smolen. They " .+
            "can choose the trump suit by bidding game now. We'll usually " .+
            "pass that, but if we've got slam interest, we'll now know " .+
            "which suit is trump and can investigate further."
      in
        situation "55GF2" action B.b1N2H2S3H explanation
  in
    -- Note that with 5-5 and game-going strength, we would have opened the
    -- bidding if we had a chance (needing only 10 HCP on the rule of 20). So,
    -- we must not have had a chance to bid before partner.
    stdWrapNW sit


rebidMinor :: Situations
rebidMinor = let
    sit (responderBid, openerRebid, responderRebid) = let
        action = do
            setOpener T.North
            B.b1N
            B.noInterference
            _ <- responderBid
            B.noInterference
            _ <- openerRebid
            B.noInterference
        explanation =
            "Partner has opened a strong " .+ B.b1N .+ ". We've got a " .+
            "game-forcing, two-suited hand including a 5-card major. We " .+
            "started with a Jacoby transfer into our major, and now it's " .+
            "time to rebid our minor. Partner can now find the right game: " .+
            "if they've got a fit in our major suit, they'll rebid that " .+
            "(likely at the 3 level, so we can get in a round of cue " .+
            "bidding if we've got slam interest). If we don't have a fit " .+
            "in either suit, they'll likely bid " .+ T.Bid 3 T.Notrump .+
            ". If we've got a minor-suit fit, partner might raise our " .+
            "second suit, but might still bid " .+ T.Bid 3 T.Notrump .+
            ", if that's likely to be an easier game to make."
      in
        situation "ubgf" action responderRebid explanation
  in
    wrapVulDlr $ return sit <~ [ (B.b1N2D, B.b1N2D2H, B.b1N2D2H3C)
                               , (B.b1N2D, B.b1N2D2H, B.b1N2D2H3D)
                               , (B.b1N2H, B.b1N2H2S, B.b1N2H2S3C)
                               , (B.b1N2H, B.b1N2H2S, B.b1N2H2S3D)
                               ]

superaccept :: Situations
superaccept = let
    sit (responderBid, openerRebid) = let
        action = do
            setOpener T.South
            B.b1N
            B.noInterference
            _ <- responderBid
            B.noInterference
        explanation =
            "We've opened a strong " .+ B.b1N .+ ", and partner has made " .+
            "a Jacoby transfer. We've got at least 4-card support in that " .+
            "suit and a maximum, so superaccept by completing the transfer " .+
            "one level higher than usual. This sets the trump suit. If " .+
            "partner was a minimum, they'll pass, and our 9-card fit " .+
            "gives the contract decent chances despite not having much " .+
            "strength. If instead partner was at least invitational, " .+
            "they'll raise to game, knowing this is the right suit. If " .+
            "partner has slam interest, they'll start control bidding. " .+
            "They know we can't confuse a control bid for a second suit " .+
            "because we've already found our trump fit."
      in
        situation "supac" action openerRebid explanation
  in
    wrapVulDlr $ return sit <~ [(B.b1N2D, B.b1N2D3H), (B.b1N2H, B.b1N2H3S)]


noFlatSuperaccept :: Situations
noFlatSuperaccept = let
    sit (responderBid, openerRebid) = let
        suit = T.suitBid openerRebid
        action = do
            setOpener T.South
            B.b1N
            B.noInterference
            _ <- responderBid
            B.noInterference
            pointRange 17 17
            minSuitLength suit 4
            flatHand
        explanation =
            "We've opened a strong " .+ B.b1N .+ ", and partner has made " .+
            "a Jacoby transfer. Although we've got a maximum with at least " .+
            "4-card support in " .+ show suit .+ ", we have a very flat " .+
            "4333 shape, which will make it very hard to ruff anything in " .+
            "our hand. Don't superaccept: just complete the transfer " .+
            "normally."
      in
        situation "flatSA" action openerRebid explanation
  in
    wrapVulDlr $ return sit <~ [(B.b1N2D, B.b1N2D2H), (B.b1N2H, B.b1N2H2S)]


singleSuitedInvite :: Situations
singleSuitedInvite = let
    sit (responderBid, openerRebid, responderRebid) = let
        suit = T.suitBid openerRebid
        action = do
            setOpener T.North
            B.b1N
            B.noInterference
            _ <- responderBid
            B.noInterference
            _ <- openerRebid
            B.noInterference
        explanation =
            "Partner opened a strong " .+ B.b1N .+ ". We've got " .+
            "a single-suited hand with at least 6 " .+ show suit .+ ", so " .+
            "we know we have a major-suit fit. We transferred to our " .+
            "suit, and now can raise to the 3 level to show our " .+
            "invitational strength. Partner will pass with a minimum, " .+
            "and bid " .+ T.Bid 4 suit .+ " with a maximum."
      in
        situation "ssinv" action responderRebid explanation
  in
    -- We must be an unpassed hand: if we had a chance to bid earlier, we would
    -- have bid a weak two.
    wrapVulNW $ return sit <~ [ (B.b1N2D, B.b1N2D2H, B.b1N2D2H3H)
                              , (B.b1N2H, B.b1N2H2S, B.b1N2H2S3S) ]


topic :: Topic
topic = makeTopic "Jacoby transfers"  "JacTrans" $
    wrap [ initiateTransferWeak
         , initiateTransferBInv
         , initiateTransferBGf
         , completeTransfer
         , completeTransferShort
         -- rare situations
         , wrap [majors55inv, majors55gf, majors55inv2, majors55gf2]
         , rebidMinor
         , wrap [superaccept, noFlatSuperaccept]
         , singleSuitedInvite
         ]
