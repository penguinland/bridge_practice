module Topics.Overcalls(topic) where

import Control.Monad(when)

import qualified Bids.Overcalls as B
import CommonBids(setOpener)
import EDSL(soundHolding, pointRange, makeCall, forEach, forbid, hasTopN,
            suitLength, maxSuitLength, minSuitLength)
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
            "enough strength to overcall. Bid it as cheaply as possible."
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
      in situation "ost2" action overcall explanation
  in
    wrap $ return sit <~ [ (B.b1C, B.b1Co1N)
                         , (B.b1D, B.b1Do1N)
                         , (B.b1H, B.b1Ho1N)
                         , (B.b1S, B.b1So1N)
                         ]
                      -- You would have opened if you could have.
                      <~ [T.West, T.North, T.East]
                      <~ T.allVulnerabilities


weakTwo :: Situations
weakTwo = let
    sit (opening, overcall, suit) dealer vul = let
        action = do
            setOpener T.East
            _ <- opening
            when (vul == T.EW || vul == T.Both) (soundHolding suit)
            -- If partner is an unpassed hand, overcall at the 1 level if you
            -- can, to avoid preempting partner.
            when (dealer == T.East) (pointRange 0 7)
        explanation =
            "RHO has opened the bidding. We have a hand appropriate for " .+
            "a weak two bid, and jumping to the 2 level shows this."
      in situation "owk2" action overcall explanation dealer vul
  in
    wrap $ return sit <~ [ (B.b1C, B.b1Co2D, T.Diamonds)
                         , (B.b1C, B.b1Co2H, T.Hearts)
                         , (B.b1C, B.b1Co2S, T.Spades)
                         , (B.b1D, B.b1Do2H, T.Hearts)
                         , (B.b1D, B.b1Do2S, T.Spades)
                         , (B.b1H, B.b1Ho2S, T.Spades)
                         ]
                      -- You would have already bid a weak 2 if you could have.
                      <~ [T.West, T.North, T.East]
                      <~ T.allVulnerabilities


preempt :: Situations
preempt = let
    sit (opening, overcall, suit) dealer vul = let
        action = do
            setOpener T.East
            _ <- opening
            when (vul == T.EW || vul == T.Both) (soundHolding suit)
            -- If partner is an unpassed hand, overcall at the 1 level if you
            -- can, to avoid preempting partner.
            when (dealer == T.East) (pointRange 0 7)
        explanation =
            "RHO has opened the bidding. We can make a preempt, hopefully " .+
            "high enough to make it hard for the opponents to find the " .+
            "correct contract. Partner might even continue the preempt " .+
            "even higher on their turn."
      in situation "preempt" action overcall explanation dealer vul
  in
    wrap $ return sit <~ [ (B.b1C, B.b1Co3D, T.Diamonds)
                         , (B.b1C, B.b1Co3H, T.Hearts)
                         , (B.b1C, B.b1Co3S, T.Spades)
                         , (B.b1C, B.b1Co4D, T.Diamonds)
                         , (B.b1C, B.b1Co4H, T.Hearts)
                         , (B.b1C, B.b1Co4S, T.Spades)
                         , (B.b1D, B.b1Do3C, T.Clubs)
                         , (B.b1D, B.b1Do3H, T.Hearts)
                         , (B.b1D, B.b1Do3S, T.Spades)
                         , (B.b1D, B.b1Do4C, T.Clubs)
                         , (B.b1D, B.b1Do4H, T.Hearts)
                         , (B.b1D, B.b1Do4S, T.Spades)
                         , (B.b1H, B.b1Ho3C, T.Clubs)
                         , (B.b1H, B.b1Ho3D, T.Diamonds)
                         , (B.b1H, B.b1Ho3S, T.Spades)
                         , (B.b1H, B.b1Ho4C, T.Clubs)
                         , (B.b1H, B.b1Ho4D, T.Diamonds)
                         , (B.b1H, B.b1Ho4S, T.Spades)
                         , (B.b1S, B.b1So3C, T.Clubs)
                         , (B.b1S, B.b1So3D, T.Diamonds)
                         , (B.b1S, B.b1So3H, T.Hearts)
                         , (B.b1S, B.b1So4C, T.Clubs)
                         , (B.b1S, B.b1So4D, T.Diamonds)
                         , (B.b1S, B.b1So4H, T.Hearts)
                         ]
                      -- You would already have preempted if you could have.
                      <~ [T.West, T.North, T.East]
                      <~ T.allVulnerabilities


passNot1 :: Situations
passNot1 = let
    sit (opening, suit) = let
        action = do
            setOpener T.East
            _ <- opening
            suitLength suit 5
            pointRange 5 7
            forbid $ hasTopN suit 5 2
            forbid $ hasTopN suit 3 1
            -- If you were 5-5, you might be tempted to bid Michaels/UNT
            forEach (T.otherSuits suit) (`maxSuitLength` 4)
        explanation =
            "RHO has opened the bidding. Although we have a 5-card suit, " .+
            "we are too weak and have too bad a suit to make an overcall. " .+
            "Just pass."
      in situation "pass1" action (makeCall T.Pass) explanation
  in
    wrapDlr $ return sit <~ [ (B.b1C, T.Diamonds)
                            , (B.b1C, T.Hearts)
                            , (B.b1C, T.Spades)
                            , (B.b1D, T.Hearts)
                            , (B.b1D, T.Spades)
                            , (B.b1H, T.Spades)
                            ]


passNot2 :: Situations
passNot2 = let
    sit (opening, oppsSuit, suit) = let
        action = do
            setOpener T.East
            _ <- opening
            suitLength suit 5
            pointRange 8 9
            forbid $ hasTopN suit 5 2
            -- If you were 5-5, you might be tempted to bid Michaels/UNT
            forEach (T.otherSuits suit) (`maxSuitLength` 4)
            -- Don't be tempted to make a takeout double, either
            minSuitLength oppsSuit 3
        explanation =
            "RHO has opened the bidding. Although we have a 5-card suit, " .+
            "we are too weak to make an overcall at the 2 level. Just pass."
      in situation "pass2" action (makeCall T.Pass) explanation
  in
    wrapDlr $ return sit <~ [ (B.b1D, T.Diamonds, T.Clubs)
                            , (B.b1H, T.Hearts,   T.Clubs)
                            , (B.b1H, T.Hearts,   T.Diamonds)
                            , (B.b1S, T.Spades,   T.Clubs)
                            , (B.b1S, T.Spades,   T.Diamonds)
                            , (B.b1S, T.Spades,   T.Hearts)
                            ]


topic :: Topic
topic = makeTopic "immediate overcalls" "overC" situations
  where
    situations = wrap [ oneLevelOvercall
                      , twoLevelOvercall
                      , notrumpOvercall
                      , wrap [weakTwo, preempt]
                      , wrap [passNot1, passNot2]
                      ]
