module Topics.TakeoutDouble(topic) where

import qualified Bids.OneNotrump as B
import CommonBids(setOpener)
import EDSL(alternatives)
import Output(Punct(..), (.+))
import Situation(situation, (<~))
import qualified Terminology as T
import Topic(Topic, stdWrap, stdWrapSE, wrap, wrapDlr, Situations, makeTopic)


smolen :: Situations
smolen = let
    sit bid = let
        action = do
            setOpener T.North
            B.b1N
            B.noInterference
            B.b1N2C
            B.noInterference
            B.b1N2C2D
            B.noInterference
        explanation =
            "Partner opened a strong " .+ B.b1N .+ ", we bid Stayman, and " .+
            "partner denied a 4-card major. They might still have a 3-card " .+
            "major, though. Jump in our shorter major to show our 5" .+ NDash .+
            "4 shape: partner then has a choice of games, depending on " .+
            "whether we have a major-suit fit."
      in situation "smol" action bid explanation
  in
    wrapDlr $ return sit <~ [B.b1N2C2D3H, B.b1N2C2D3S]


topic :: Topic
topic = makeTopic "takeout doubles of 1-level suit openings" "tox" situations
  where
    situations = wrap [ smolen
                      ]
