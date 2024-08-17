module Topics.Jacoby2NT(topic) where

import qualified Bids.Jacoby2NT as B
import CommonBids(setOpener, noInterference)
import Output((.+))
import Situation(situation, (<~))
import qualified Terminology as T
import Topic(wrap, wrapVulNW, Situations, Topic, makeTopic)


j2nt :: Situations
j2nt = let
    sit (openerBid, j2ntBid, suit) = let
        action = do
            setOpener T.North
            _ <- openerBid
            noInterference suit
        explanation =
            "We've got game-forcing strength with at least 4-card support " .+
            "for partner's " .+ show suit .+ ". Bid Jacoby " .+
            T.Bid 2 T.Notrump .+ " to show this. Now that we've set trump " .+
            "and entered a game-forcing auction,  we'll figure out whether " .+
            "we belong in game or slam."
      in
        situation "j2n" action j2ntBid explanation
  in
    -- We must be an unpassed hand to be game-forcing.
    wrapVulNW $ return sit <~ [(B.b1H, B.b1H2N, T.Hearts),
                               (B.b1S, B.b1S2N, T.Spades)]


topic :: Topic
topic = makeTopic ("Jacoby ".+ T.Bid 2 T.Notrump)  "J2NT" $
    wrap [ j2nt
         , j2nt
         ]
