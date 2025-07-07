module Topics.Overcalls(topic) where

import qualified Bids.Overcalls as B
import CommonBids(setOpener)
import Output((.+))
import Situation(situation, (<~))
import qualified Terminology as T
import Topic(Topic, wrap, wrapDlr, Situations, makeTopic)


oneLevelOvercall :: Situations
oneLevelOvercall = let
    sit (opening, overcall) = let
        action = do
            setOpener T.East
            opening
        explanation =
            "RHO has opened the bidding. We've got a 5-card suit and " .+
            "enough strength to overcall it."
      in situation "o1" action overcall explanation
  in
    wrapDlr $ return sit <~ [ (B.b1C, B.b1Co1D)
                            , (B.b1C, B.b1Co1H)
                            , (B.b1C, B.b1Co1S)
                            , (B.b1D, B.b1Co1H)
                            , (B.b1D, B.b1Co1S)
                            , (B.b1H, B.b1Co1S)
                            ]


topic :: Topic
topic = makeTopic "immediate overcalls" "overC" situations
  where
    situations = wrap [ oneLevelOvercall
                      ]
