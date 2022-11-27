module Topics.StandardModernPrecision.MafiaResponses(topic) where

import Output(output)
import Topic(Topic(..), wrap, Situations)
import Auction(withholdBid, Action, suitLength, maxSuitLength)
import Situation(Situation, situation, base, (<~))
import qualified Terminology as T
import Topics.StandardModernPrecision.BasicBids(oppsPass, smpWrapN)
import qualified Topics.StandardModernPrecision.Bids1C as B


minSupport :: Situations
minSupport = let
    -- The type signature is to convince the compiler that we're not throwing
    -- away values from openerBid
    sit :: (Action, Action) -> T.Vulnerability -> T.Direction -> Situation
    sit (openerBid, responderBid) = let
        action = do
            B.startOfMafia
            openerBid
            oppsPass
            withholdBid responderBid
        explanation _ =
            "With 4-card support and a minimum hand, raise partner's major.\
           \ You are neither strong enough nor shapely enough to invite to\
           \ game."
      in
        situation "2M" action responderBid explanation
  in
    smpWrapN $ base sit <~ [ (B.b1C1D1H, B.b1C1D1H2H)
                           , (B.b1C1D1S, B.b1C1D1S2S) ]


maxSupportSemibalanced :: Situations
maxSupportSemibalanced = let
    -- The type signature is to convince the compiler that we're not throwing
    -- away values from openerBid
    sit :: (Action, Action) -> T.Vulnerability -> T.Direction -> Situation
    sit (openerBid, responderBid) = let
        action = do
            B.startOfMafia
            openerBid
            oppsPass
            withholdBid responderBid
        explanation fmt =
            "With 4-card support and a maximum hand but no singleton, invite\
           \ with a double raise. You've already shown that game might not be\
           \ possible with your " ++ output fmt (T.Bid 1 T.Diamonds) ++ " bid,\
           \ so partner won't get too excited."
      in
        situation "3MB" action responderBid explanation
  in
    smpWrapN $ base sit <~ [ (B.b1C1D1H, B.b1C1D1H3H)
                           , (B.b1C1D1S, B.b1C1D1S3S) ]


maxSupportUnbalanced :: Situations
maxSupportUnbalanced = let
    -- The type signature is to convince the compiler that we're not throwing
    -- away values from openerBid
    sit :: (Action, Action) -> T.Vulnerability -> T.Direction -> Situation
    sit (openerBid, responderBid) = let
        action = do
            B.startOfMafia
            openerBid
            oppsPass
            withholdBid responderBid
        explanation fmt =
            "With 4-card support, a maximum hand, and a singleton or void,\
           \ make a mini-splinter bid with " ++
             output fmt (T.Bid 2 T.Notrump) ++ ". Partner can\
           \ either sign off in our major (in partscore or game), or bid " ++
            output fmt (T.Bid 3 T.Clubs) ++ " to ask\
           \ us to bid our singleton. Remember that you can bid the\
           \ singleton/void ``naturally'' unless it's clubs, in which case bid\
           \ our trump suit to show it!"
      in
        situation "3MV" action responderBid explanation
  in
    smpWrapN $ base sit <~ [ (B.b1C1D1H, B.b1C1D1H2N)
                           , (B.b1C1D1S, B.b1C1D1S2N) ]


brakesHearts :: Situations
brakesHearts = let
    sit = let
        action = do
            B.startOfMafia
            B.b1C1D1H
            oppsPass
            withholdBid B.b1C1D1H1N
        explanation fmt =
            "With neither major and a non-maximum hand, bid " ++
             output fmt (T.Bid 1 T.Notrump) ++ " as the ``double negative''\
           \ brake bid. Unless partner makes a jump bid, seriously consider\
           \ passing whatever they do next (if they do anything at all;\
           \ perhaps they'll prefer playing in " ++
             output fmt (T.Bid 1 T.Notrump) ++ " itself, since your bid is\
           \ not forcing)."
      in
        situation "1NH" action B.b1C1D1H1N explanation
  in
    smpWrapN $ base sit


brakesSpades :: Situations
brakesSpades = let
    sit = let
        action = do
            B.startOfMafia
            B.b1C1D1S
            oppsPass
            maxSuitLength T.Hearts 4
            withholdBid B.b1C1D1S1N
        explanation fmt =
            "With a minimum hand and no support for partner's spades, bid " ++
             output fmt (T.Bid 1 T.Notrump) ++ " as the ``double negative''\
           \ brake bid. Unless partner makes a jump bid, seriously consider\
           \ passing whatever they do next (if they do anything at all;\
           \ perhaps they'll prefer playing in " ++
             output fmt (T.Bid 1 T.Notrump) ++ " itself, since your bid is\
           \ not forcing)."
      in
        situation "1NS" action B.b1C1D1S1N explanation
  in
    smpWrapN $ base sit


brakesSpadesHearts :: Situations
brakesSpadesHearts = let
    sit = let
        action = do
            B.startOfMafia
            B.b1C1D1S
            oppsPass
            suitLength T.Hearts 5
            withholdBid B.b1C1D1S1N
        explanation fmt =
            "With a minimum hand and no support for partner's spades, bid " ++
             output fmt (T.Bid 1 T.Notrump) ++ " as the ``double negative''\
           \ brake bid. Do this even if you've got a heart suit! Partner's " ++
             output fmt (T.Bid 1 T.Spades) ++ " bid has denied 4 hearts, and\
           \ although we might have a 5-3 fit, you're too weak to risk getting\
           \ stuck at the 3 level if we don't have such a fit. Unless partner\
           \ makes a jump bid, seriously consider passing whatever they do\
           \ next (if they do anything at all; perhaps they'll prefer playing\
           \ in " ++ output fmt (T.Bid 1 T.Notrump) ++ " itself, since your\
           \ bid is not forcing)."
      in
        situation "1NSH" action B.b1C1D1S1N explanation
  in
    smpWrapN $ base sit


otherMajorHearts :: Situations
otherMajorHearts = let
    sit = let
        action = do
            B.startOfMafia
            B.b1C1D1H
            oppsPass
            withholdBid B.b1C1D1H1S
        explanation fmt =
            "With no support for partner's hearts but at least 4 spades,\
           \ bid " ++ output fmt (T.Bid 1 T.Spades) ++ " to show that major.\
           \ Because we're still at the 1 level, you can do this with any\
           \ strength hand, even a 0 count!"
      in
        situation "1S" action B.b1C1D1H1S explanation
  in
    smpWrapN $ base sit


otherMajorSpades :: Situations
otherMajorSpades = let
    sit = let
        action = do
            B.startOfMafia
            B.b1C1D1S
            oppsPass
            withholdBid B.b1C1D1S2H
        explanation _ =
            "With a maximum hand, no support for partner's spades, but 5+\
           \ hearts, show that suit in our quest to find a major fit. Partner\
           \ has already denied 4 hearts (unless they're at least 5-4 in the\
           \ majors, in which case we'll find out soon), so we need a 5-card\
           \ suit to bid this. We also need a maximum, because if it turns out\
           \ we don't have a fit, we're going to get pushed up to the 3 level\
           \ at least."
      in
        situation "2H" action B.b1C1D1S2H explanation
  in
    smpWrapN $ base sit


threeCardSupport :: Situations
threeCardSupport = let
    sit :: (Action, Action) -> T.Vulnerability -> T.Direction -> Situation
    sit (openerBid, responderBid) = let
        action = do
            B.startOfMafia
            openerBid
            oppsPass
            withholdBid responderBid
        explanation fmt =
            "With 3-card support and a non-minimum hand (5-7 HCP), bid " ++
            output fmt (T.Bid 2 T.Diamonds) ++ ". Partner can sign off at the\
           \ 2 level with a 7-card fit, invite with an 8-card fit, or bid\
           \ other suits naturally (minor suits must be 6 cards long). Note\
           \ that partner's " ++ output fmt (T.Bid 2 T.Notrump) ++ " is an\
           \ artificial game force."
      in
        situation "2D" action responderBid explanation
  in
    smpWrapN $ base sit <~ [ (B.b1C1D1H, B.b1C1D1H2D)
                           , (B.b1C1D1S, B.b1C1D1S2D) ]


threeCardSupportHearts :: Situations
threeCardSupportHearts = let
    sit  = let
        action = do
            B.startOfMafia
            B.b1C1D1S
            oppsPass
            suitLength T.Hearts 5
            withholdBid B.b1C1D1S2D
        explanation fmt =
            "With 3-card spade support and a non-minimum hand (5-7 HCP),\
           \ bid " ++ output fmt (T.Bid 2 T.Diamonds) ++ ". Do this even with\
           \ a 5-card heart suit!"
      in
        situation "2D" action B.b1C1D1S2D explanation
  in
    smpWrapN $ base sit


maxNoMajors :: Situations
maxNoMajors = let
    sit :: (Action, Action) -> T.Vulnerability -> T.Direction -> Situation
    sit (openerBid, responderBid) = let
        action = do
            B.startOfMafia
            openerBid
            oppsPass
            withholdBid responderBid
        explanation fmt =
            "With a maximum hand but no obvious major fit, respond " ++
            output fmt (T.Bid 2 T.Clubs) ++ ". Opener might bid an\
           \ artificial " ++ output fmt (T.Bid 2 T.Diamonds) ++ " to show a\
           \ misfit where we're scrambling for partscore, or bid a natural\
           \ suit (6-card minor, 4 cards in the other major), or " ++
            output fmt (T.Bid 2 T.Notrump) ++ " as an artificial game\
           \ force/waiting bid."
      in
        situation "2C" action responderBid explanation
  in
    smpWrapN $ base sit <~ [ (B.b1C1D1H, B.b1C1D1H2C)
                           , (B.b1C1D1S, B.b1C1D1S2C) ]


topic :: Topic
topic = Topic "MaFiA responses" "MafResp" situations
  where
    situations = wrap [ minSupport
                      , maxSupportSemibalanced
                      , maxSupportUnbalanced
                      , wrap [brakesHearts, wrap [brakesSpades,
                                                  brakesSpadesHearts]]
                      , wrap [otherMajorHearts, otherMajorSpades]
                      -- 3-card support in spades when you also have hearts is
                      -- much rarer than any other 3-card support situation.
                      , wrap [ threeCardSupport, threeCardSupport
                             , threeCardSupport, threeCardSupport
                             , threeCardSupportHearts ]
                      , maxNoMajors
                      -- TODO: jump responses, splinters, etc.
                      ]
