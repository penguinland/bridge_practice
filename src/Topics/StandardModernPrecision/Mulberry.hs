module Topics.StandardModernPrecision.Mulberry(topic) where

import Action(Action)
import Bids.StandardModernPrecision.BasicBids(setOpener)
import qualified Bids.StandardModernPrecision.TwoDiamonds as TD
import qualified Bids.StandardModernPrecision.Mulberry as Mul
import CommonBids(takeoutDouble)
import EDSL(forbid, suitLength, makePass)
import Output((.+))
import Situation(situation, (<~))
import qualified Terminology as T
import Topic(Topic, wrap, wrapWeighted, stdWrap, wrapDlr, Situations, makeTopic,
             wrapNW, wrapSE, stdWrapNW, stdWrapSE)


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
    wrapNW $ return sit <~ [B.b2D2N3CP, B.b2D2N3C3H, B.b2D2N3C3S]


-- TODO:
--   - Make a 4D bid
--   - Relay 4H over 4D
--   - Pass or correct over 4D-4H
--   - Make a keycard ask
--   - Add auctions starting with 1C
--   - Over auctions starting 1C, bid 4C
--   - Over auctions starting 1C and a 4C bid, relay 4D
--   - Over auctions starting 1C and a 4C-4D relay, bid trump
--   - Over auctions starting 1C and a 4C-4D-trump, pass
--   - Over auctions starting 1C and a 4C-4D-trump, investigate slam


topic :: Topic
topic = makeTopic description "mulberry over SMP 3-suiters" situations
  where
    description = ("SMP " .+ T.Bid 2 T.Diamonds .+ " auctions")
    situations = wrap [ invSignoff
                      , invSignoff
                      ]
