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
                            , (B.b1D, B.b1Do1H)
                            , (B.b1D, B.b1Do1S)
                            , (B.b1H, B.b1Ho1S)
                            ]


twoLevelOvercall :: Situations
twoLevelOvercall = let
    sit (opening, overcall) = let
        action = do
            setOpener T.East
            opening
        explanation =
            "RHO has opened the bidding. We can't overcall at the 1 level, " .+
            "but we've got enough extra strength that we can overcall at " .+
            "the 2 level instead."
      in situation "o2" action overcall explanation
  in
    wrapDlr $ return sit <~ [ (B.b1D, B.b1Do2C)
                            , (B.b1H, B.b1Ho2C)
                            , (B.b1H, B.b1Ho2D)
                            , (B.b1S, B.b1So2C)
                            , (B.b1S, B.b1So2D)
                            , (B.b1S, B.b1So2H)
                            ]


topic :: Topic
topic = makeTopic "immediate overcalls" "overC" situations
  where
    situations = wrap [ oneLevelOvercall
                      , twoLevelOvercall
                      ]
