module Topics.Meckwell(topic) where

import CommonBids(setOpener)
import Output((.+))
import Situation(situation, (<~))
import qualified Terminology as T
import Topic(Topic, wrap, Situations, makeTopic)
import qualified Topics.BidsMeckwell as B


majorSuit :: Situations
majorSuit = let
    sit (suit, bid) = let
        action = do
            setOpener T.East
            B.b1N
        explanation =
            "With a single-suited hand with " .+ show suit .+ ", make a " .+
            "natural overcall."
        in situation "SInvAcc" action bid explanation
  in
    -- Ensure we're not dealer: it's too rare to find a hand where we'd want to
    -- overcall after 1N but not open the bidding ourselves.
    wrap $ return sit <~ [(T.Hearts, B.b1No2H), (T.Spades, B.b1No2S)]
                      <~ allVulnerabilities <~ [T.West, T.North, T.East]



topic :: Topic
topic = makeTopic "Meckwell over strong notrump" "MW1N" situations
  where
    situations = wrap [ majorSuit
                      , majorSuit
                      , majorSuit
                      ]
