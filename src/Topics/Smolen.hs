module Topics.Smolen(topic) where

import qualified Bids.OneNotrump as B
import CommonBids(setOpener)
import EDSL(suitLength, alternatives)
import Output(Punct(..), (.+))
import Situation(situation)--, (<~))
import qualified Terminology as T
import Topic(Topic, stdWrap, wrap, Situations, makeTopic)


stayman :: Situations
stayman = let
    sit = let
        action = do
            setOpener T.North
            B.b1N
            B.noInterference
            alternatives [ suitLength T.Hearts 4 >> suitLength T.Spades 5
                         , suitLength T.Spades 4 >> suitLength T.Hearts 5
                         ]
        explanation =
            "Partner opened a strong " .+ B.b1N .+ ". With 5" .+ NDash .+
            "4 in the majors and game-forcing strength, start by bidding " .+
            "Stayman. If opener has a major, we're in good shape. If they " .+
            "don't, we can bid Smolen afterwards to look for a 5" .+ NDash .+
            "3 major, and still stop in " .+ T.Bid 3 T.Notrump .+ " if we " .+
            "don't find one."
      in situation "stay" action B.b1N2C explanation
  in
    stdWrap sit


-- TODO:
-- Smolen
-- Opener has a fit
-- Opener doesn't have a fit


topic :: Topic
topic = makeTopic "Smolen" "leb1N" situations
  where
    situations = wrap [ stayman
                      ]
