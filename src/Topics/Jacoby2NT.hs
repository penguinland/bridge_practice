module Topics.Jacoby2NT(topic) where

import qualified Bids.Jacoby2NT as B
import CommonBids(setOpener, noInterference)
import Output((.+))
import Situation(situation, (<~))
import qualified Terminology as T
import Topic(wrap, wrapVulNW, wrapVulSE, Situations, Topic, makeTopic)


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
    wrapVulNW $ return sit <~ [ (B.b1H, B.b1H2N, T.Hearts)
                              , (B.b1S, B.b1S2N, T.Spades) ]


singleton :: Situations
singleton = let
    sit (openerBid, j2ntBid, singletonBid, suit) = let
        action = do
            setOpener T.South
            _ <- openerBid
            noInterference suit
            _ <- j2ntBid
            noInterference suit
        explanation =
            "Partner has bid Jacoby " .+ T.Bid 2 T.Notrump .+ ". We don't " .+
            "have a good 5-card side suit to jump into, but we do have " .+
            "shortness to bid at the cheapest level. This will help " .+
            "partner decide whether to sign off in " .+ T.Bid 4 suit .+
            ", or start control bidding to investigate slam. If you play " .+
            "Serious or Frivolous " .+ T.Bid 3 T.Notrump .+ ", they might " .+
            "also bid that."
      in
        situation "sing" action singletonBid explanation
  in
    -- Partner must be an unpassed hand to be game-forcing.
    wrapVulSE $ return sit <~ [ (B.b1H, B.b1H2N, B.b1H2N3C, T.Hearts)
                              , (B.b1H, B.b1H2N, B.b1H2N3D, T.Hearts)
                              , (B.b1H, B.b1H2N, B.b1H2N3S, T.Hearts)
                              , (B.b1S, B.b1S2N, B.b1S2N3C, T.Spades)
                              , (B.b1S, B.b1S2N, B.b1S2N3D, T.Spades)
                              , (B.b1S, B.b1S2N, B.b1S2N3H, T.Spades)
                              ]


topic :: Topic
topic = makeTopic ("Jacoby ".+ T.Bid 2 T.Notrump)  "J2NT" $
    wrap [ j2nt
         , singleton
         ]
