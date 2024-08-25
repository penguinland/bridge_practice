module Topics.StandardModernPrecision.TripleFourOne(topic) where

import Bids.StandardModernPrecision.BasicBids(setOpener, oppsPass)
import Bids.StandardModernPrecision.TwoDiamonds(name44Rkc)
import qualified Bids.StandardModernPrecision.OneClub as B
import Output((.+))
import Situation(situation, (<~))
import qualified Terminology as T
import Topic(Topic, wrap, Situations, makeTopic)


showAny4441 :: Situations
showAny4441 = let
    sit (action, answer, opener, dealers) vul = let
        explanation =
            "Bidding " .+ T.Bid 2 T.Spades .+ " shows your 4441 shape. " .+
            "partner will relay to " .+ T.Bid 2 T.Notrump .+ ", prompting " .+
            "you to bid your singleton, after which " .+ name44Rkc .+ "is on."
        inner dealer = let
            action' = do
                setOpener opener
                action
          in
            situation "any" action' answer explanation vul dealer
      in
        wrap $ return inner <~ dealers
  in
    wrap $ return sit <~ [ (do B.b1C
                               oppsPass
                               B.b1C1H
                               oppsPass
                           , B.b1C1H2S, T.South, [T.South, T.East])
                         , (do B.b1C
                               oppsPass
                           , B.b1C2S, T.North, [T.North, T.West])
                         , (do B.b1C
                               oppsPass
                           , B.bP1C2S, T.North, [T.South, T.East])
                         ]
                      <~ T.allVulnerabilities


topic :: Topic
topic = makeTopic description "SMP4441" situations
  where
    description = "SMP 4441 hands"
    situations = wrap [ showAny4441
                      ]
