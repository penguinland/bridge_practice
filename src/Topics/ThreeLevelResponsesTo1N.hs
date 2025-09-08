module Topics.ThreeLevelResponsesTo1N(topic) where

import qualified Bids.OneNotrump as B
import CommonBids(setOpener)
import Output((.+))
import Situation(situation, (<~))
import qualified Terminology as T
import Topic(Topic, stdWrap, wrap, wrapDlr, Situations, makeTopic)


splinter :: Situations
splinter = let
    sit bid = let
        action = do
            setOpener T.North
            B.b1N
            B.noInterference
        explanation =
            "Partner opened a strong " .+ B.b1N .+ ". With game-forcing " .+
            "strength, 5-4 in the minors and 3-1 in the majors, make a " .+
            "splinter bid. Partner will place the contract from here. In " .+
            "rare circumstances, we can investigate slam after partner has " .+
            "set the trump suit (if any)."
      in situation "splnt" action bid explanation
  in
    wrapDlr $ return sit <~ [B.b1N3H, B.b1N3S]


bothMinors :: Situations
bothMinors = let
    sit = let
        action = do
            setOpener T.North
            B.b1N
            B.noInterference
        explanation =
            "Partner opened a strong " .+ B.b1N .+ ". With game-forcing " .+
            "strength and at least 5-5 in the minors, jump in diamonds. " .+
            "Opener can place the strain from here, either by signing off " .+
            "in " .+ B.b1N3N .+ ", or setting trump at the 4 level. We'll " .+
            "usually just raise a 4-level bid to game in the minor, but " .+
            "could explore slam with sufficient strength."
      in situation "m55" action B.b1N3D explanation
  in
    stdWrap sit


signoff :: Situations
signoff = let
    sit = let
        action = do
            setOpener T.North
            B.b1N
            B.noInterference
        explanation =
            "Partner opened a strong " .+ B.b1N .+ ". With game-forcing " .+
            "strength with no slam interest, a balanced hand, and no " .+
            "interest in the majors, just sign off in game."
      in situation "1N3N" action B.b1N3N explanation
  in
    stdWrap sit


topic :: Topic
topic = makeTopic ("Common 3-level responses to" .+ B.b1N) "1N3X" situations
  where
    situations = wrap [ splinter
                      , bothMinors
                      , signoff
                      ]
