module Topics.StandardModernPrecision.Lampe(topic) where

import CommonBids(noInterference)
import Output((.+), Punct(..))
import Situation(situation)
import qualified Terminology as T
import Topic(Topic, wrap, Situations, makeTopic)
import Bids.StandardModernPrecision.BasicBids(firstSeatOpener, smpWrapN)
import qualified Bids.StandardModernPrecision.Lampe as B


twoClubs :: Situations
twoClubs = let
    action = do
        firstSeatOpener
        B.b1D
        noInterference T.Diamonds
    explanation =
        "With at least invitational strength and a minor, start with an " .+
        "artificial " .+ T.Bid 2 T.Clubs .+ "."
  in
    smpWrapN . return $ situation "2C" action B.b1D2C explanation


twoDiamonds :: Situations
twoDiamonds = let
    action = do
        firstSeatOpener
        B.b1D
        noInterference T.Diamonds
    explanation =
        "With both majors and less than game forcing strength, make a " .+
        "Reverse-Flannery-like " .+ T.Bid 2 T.Diamonds .+ " bid."
  in
    smpWrapN . return $ situation "2D" action B.b1D2D explanation


topic :: Topic
topic = makeTopic summary "Lampe" situations
  where
    summary = "Lampe responses to Precision 1" .+ T.Diamonds .+ NDash .+ "2m"
    situations = wrap [ twoClubs
                      , twoDiamonds
                      ]
