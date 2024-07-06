module Topics.Meckwell(topic) where

import CommonBids(setOpener)
import Output((.+))
import Situation(situation, (<~))
import qualified Terminology as T
import Topic(Topic, wrap, wrapVulDlr, Situations, makeTopic)
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
    wrapVulDlr $ return sit <~ [(T.Hearts, B.b1No2H), (T.Spades, B.b1No2S)]



topic :: Topic
topic = makeTopic "Meckwell over strong notrump" "MW1N" situations
  where
    situations = wrap [ majorSuit
                      , majorSuit
                      , majorSuit
                      ]
