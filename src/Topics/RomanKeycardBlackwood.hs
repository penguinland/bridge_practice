module Topics.RomanKeycardBlackwood(topic) where

import qualified Bids.RomanKeycardBlackwood as RKC
import CommonBids(setOpener)
-- TODO: replace makePass with something more intelligent
import EDSL(makePass, pointRange, suitLength, maxSuitLength, forEach,
            forbid, semibalancedHand, alternatives)
import Output((.+), Punct(..))
import Situation(situation, (<~))
import qualified Terminology as T
import Topic(Topic, stdWrap, wrap, wrapDlr, wrapNW, wrapSE, Situations,
             makeTopic)


setUpAuctions_ :: [Action]
setUpAuctions_ = [
    ]


initiate :: Situations
initiate = let
    sit setup = let
        action = do
            setOpener T.South
            _ <- setup
        explanation =
            "We've found a trump fit and have slam interest. Time to check " .+
            "for keycards by bidding " .+ RKC.bRKC4N .+ "! If we're missing " .+
            "two of them, we'll sign off at the 5 level."
      in situation "init" action RKC.bRKC4N explanation
  in
    wrapNW $ return sit <- setUpAuctions_


topic1430 :: Topic
topic1430 = makeTopic "Roman Keycard Blackwood 1430" "RKC1430" situations
  where
    situations = wrap [ initiate
                      , initiate
                      ]

topic3014 :: Topic
topic3014 = makeTopic "Roman Keycard Blackwood 3014" "RKC3014" situations
  where
    situations = wrap [ initiate
                      , initiate
                      ]

