module Topics.Overcalls(topic) where

import qualified Bids.Overcalls as B
import CommonBids(setOpener)
import Output((.+), Punct(..))
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


notrumpOvercall :: Situations
notrumpOvercall = let
    sit (opening, overcall) = let
        action = do
            setOpener T.East
            opening
        explanation =
            "RHO has opened the bidding. We've got a balanced hand, 16" .+
            NDash .+ "18 HCPs, and one and a half stoppers in opener's " .+
            "suit. Overcall " .+ overcall .+ ". Systems (e.g., Jacoby " .+
            "transfers) are on."
      in situation "o2" action overcall explanation
  in
    wrapDlr $ return sit <~ [ (B.b1C, B.b1Co1N)
                            , (B.b1D, B.b1Do1N)
                            , (B.b1H, B.b1Ho1N)
                            , (B.b1S, B.b1So1N)
                            ]


preempt :: Situations
preempt = let
    sit (opening, overcall) = let
        action = do
            setOpener T.East
            opening
        explanation =
            "RHO has opened the bidding. We can make a preempt, hopefully " .+
            "high enough to make it hard for the opponents to find the " .+
            "correct contract. Partner might even continue the preempt " .+
            "even higher on their turn."
      in situation "preempt" action overcall explanation
  in
    wrapDlr $ return sit <~ [ (B.b1C, B.b1Co3D)
                            , (B.b1C, B.b1Co3H)
                            , (B.b1C, B.b1Co3S)
                            , (B.b1C, B.b1Co4D)
                            , (B.b1C, B.b1Co4H)
                            , (B.b1C, B.b1Co4S)
                            , (B.b1D, B.b1Do3C)
                            , (B.b1D, B.b1Do3H)
                            , (B.b1D, B.b1Do3S)
                            , (B.b1D, B.b1Do4C)
                            , (B.b1D, B.b1Do4H)
                            , (B.b1D, B.b1Do4S)
                            , (B.b1H, B.b1Ho3C)
                            , (B.b1H, B.b1Ho3D)
                            , (B.b1H, B.b1Ho3S)
                            , (B.b1H, B.b1Ho4C)
                            , (B.b1H, B.b1Ho4D)
                            , (B.b1H, B.b1Ho4S)
                            , (B.b1S, B.b1So3C)
                            , (B.b1S, B.b1So3D)
                            , (B.b1S, B.b1So3H)
                            , (B.b1S, B.b1So4C)
                            , (B.b1S, B.b1So4D)
                            , (B.b1S, B.b1So4H)
                            ]


topic :: Topic
topic = makeTopic "immediate overcalls" "overC" situations
  where
    situations = wrap [ oneLevelOvercall
                      , twoLevelOvercall
                      , notrumpOvercall
                      , preempt
                      ]
