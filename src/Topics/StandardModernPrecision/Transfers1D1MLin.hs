module Topics.StandardModernPrecision.Transfers1D1MLin(topic) where

import Bids.StandardModernPrecision.BasicBids(setOpener)
import qualified Bids.StandardModernPrecision.Transfers1D1MLin as B
import qualified EDSL as E
import Output((.+))
import Situation(situation, (<~))
import qualified Terminology as T
import Topic(Topic, makeTopic, Situations, wrap, wrapDlr, stdWrap)


nonNegativeDouble :: Situations
nonNegativeDouble = let
    sit spadeLength = let
        action = do
            setOpener T.North
            B.b1D
            B.b1Do1H
            E.suitLength T.Spades spadeLength
        explanation =
            "After a " .+ B.b1Do1H .+ " overcall, the double shows 4 or " .+
            "more spades, not merely 4."
      in
        situation "1HX" action B.b1Do1SX explanation
  in
    wrapDlr $ return sit <~ [4, 5, 6]


negativeDouble :: Situations
negativeDouble = let
    action = do
        setOpener T.North
        B.b1D
        B.b1Do1S
    explanation =
        "After a " .+ B.b1Do1S .+ " overcall, the double is still negative. " .+
        "With 5+ hearts, bid TODO to transfer into the suit."
  in
    stdWrap $ situation "1SX" action B.b1Do1SX explanation


topic :: Topic
topic = makeTopic description "smp1d1mT" situations
  where
    description = "SMP transfer responses to " .+ T.Bid 1 T.Diamonds .+
                  " and a 1M overcall (Lin variation)"
    situations = wrap [ wrap [negativeDouble, nonNegativeDouble]
                      ]
