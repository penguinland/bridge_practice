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
            "Bidding " .+ T.Bid 2 T.Spades .+ " shows our 4441 shape. " .+
            "Partner will relay to " .+ T.Bid 2 T.Notrump .+ ", prompting " .+
            "us to bid our singleton, after which " .+ name44Rkc .+ " is on."
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


relay :: Situations
relay = let
    sit (action, answer, opener, dealers) vul = let
        explanation =
            "Partner's " .+ T.Bid 2 T.Spades .+ " showed some 4441 shape. " .+
            "Relay to " .+ T.Bid 2 T.Notrump .+ ", asking partner to bid " .+
            "their singleton, after which " .+ name44Rkc .+ " is on."
        inner dealer = let
            action' = do
                setOpener opener
                action
          in
            situation "relay" action' answer explanation vul dealer
      in
        wrap $ return inner <~ dealers
  in
    wrap $ return sit <~ [ (do B.b1C
                               oppsPass
                               B.b1C1H
                               oppsPass
                               B.b1C1H2S
                               oppsPass
                           , B.b1C1H2S2N, T.North, [T.North, T.West])
                         , (do B.b1C
                               oppsPass
                               B.b1C2S
                               oppsPass
                           , B.b1C2S2N, T.South, [T.South, T.East])
                         , (do B.b1C
                               oppsPass
                               B.bP1C2S
                               oppsPass
                           , B.b1C2S2N, T.South, [T.North, T.West])
                         ]
                      <~ T.allVulnerabilities


bidSingleton :: Situations
bidSingleton = let
    sit (action, answers, opener, dealers) vul = let
        explanation =
            "Our " .+ T.Bid 2 T.Spades .+ " showed some 4441 shape, and " .+
            "partner has relayed " .+ T.Bid 2 T.Notrump .+ " to ask what " .+
            "our singleton is. Bid it at the 3 level. " .+ name44Rkc .+
            " is now on."
        inner answer dealer = let
            action' = do
                setOpener opener
                action
          in
            situation "bsing" action' answer explanation vul dealer
      in
        wrap $ return inner <~ answers <~ dealers
  in
    wrap $ return sit <~ [ (do B.b1C
                               oppsPass
                               B.b1C1H
                               oppsPass
                               B.b1C1H2S
                               oppsPass
                               B.b1C1H2S2N
                               oppsPass
                           , [ B.b1C1H2S2N3C, B.b1C1H2S2N3D
                             , B.b1C1H2S2N3H, B.b1C1H2S2N3S ]
                           , T.South, [T.South, T.East])
                         , (do B.b1C
                               oppsPass
                               B.b1C2S
                               oppsPass
                               B.b1C2S2N
                               oppsPass
                           , [ B.b1C2S2N3C, B.b1C2S2N3D
                             , B.b1C2S2N3H, B.b1C2S2N3S ]
                           , T.North, [T.North, T.West])
                         , (do B.b1C
                               oppsPass
                               B.bP1C2S
                               oppsPass
                               B.b1C2S2N
                               oppsPass
                           , [ B.b1C2S2N3C, B.b1C2S2N3D
                             , B.b1C2S2N3H, B.b1C2S2N3S ]
                           , T.North, [T.South, T.East])
                         ]
                      <~ T.allVulnerabilities


topic :: Topic
topic = makeTopic description "SMP4441" situations
  where
    description = ("SMP 4441 hands in " .+ T.Bid 1 T.Clubs .+ " auctions")
    situations = wrap [ showAny4441
                      , relay
                      , bidSingleton
                      ]
