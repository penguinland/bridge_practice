module Topics.Jordan2NT(topic) where

import qualified Bids.Jordan2NT as J 
import qualified Bids.StandardOpenings as S 
import qualified Bids.TakeoutDoubles as TD
import CommonBids(setOpener)
import Output((.+))
import Situation(situation, (<~))
import qualified Terminology as T
import Topic(wrap, wrapDlr, Situations, Topic, makeTopic)


jd2nt :: Situations
jd2nt = let
    sit (suit, setup, answer) = let
        action = do
            setOpener T.North
            setup
        explanation =
            "We've got at least invitational strength with at least 4-card support " .+
            "for partner's " .+ show suit .+ ". Bid Jordan " .+
            T.Bid 2 T.Notrump .+ " to show this. Now that we've set trump " .+
            "then partner can show their strength further."
      in
        situation "jd2n" action answer explanation
  in
    wrapDlr $ return sit <~ [
        (
        T.Hearts,
        do 
            setOpener T.North
            S.b1H
            TD.b1HoX
        , J.b1HoX2N
        )
    , (T.Spades,
        do
            setOpener T.North
            S.b1S
            TD.b1SoX
        , J.b1SoX2N)
    ]


topic :: Topic
topic = makeTopic ("Jordan ".+ T.Bid 2 T.Notrump)  "Jd2NT" $
    wrap [ jd2nt
         ]
