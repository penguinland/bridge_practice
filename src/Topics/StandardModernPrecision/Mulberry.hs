module Topics.StandardModernPrecision.Mulberry(topic) where

import Action(Action)
import Bids.StandardModernPrecision.BasicBids(setOpener)
import qualified Bids.StandardModernPrecision.TwoDiamonds as TD
import qualified Bids.StandardModernPrecision.Mulberry as Mul
import CommonBids(cannotPreempt, andNextBidderIs)
import EDSL(alternatives, makePass)
import Output((.+))
import Situation(situation, (<~))
import qualified Terminology as T
import Topic(Topic, wrap, wrapWeighted, stdWrap, wrapDlr, Situations, makeTopic,
             wrapNW, wrapSE, stdWrapNW, stdWrapSE)


initiateSignoff :: Situations
initiateSignoff = let
    sit (auctionMiddle, answer) = let
        action = do TD.b2D
                    cannotPreempt >> makePass
                    TD.b2D2N
                    cannotPreempt >> makePass
                    auctionMiddle `andNextBidderIs` T.South
        explanation =
            "Partner has limited their hand: even if we're a maximum, we " .+
            "have no interest in slam. Bid " .+ answer .+ " to indicate " .+
            "this. Partner will relay " .+ T.Bid 4 T.Hearts .+ ", which " .+
            "you can pass or correct to the final trump suit."
      in
        situation "initSO" action answer explanation
  in
    wrap $ return sit
        <~ [ ( do TD.b2D2N3C
                  makePass
                  TD.b2D2N3C3D
                  makePass
                  TD.b2D2N3C3D3H
                  makePass
               -- If trump will be spades, maybe you'd just bid 3S. So, make
               -- sure trump is something else.
             , do alternatives [ Mul.b2D2N3C3D3H4D4HP
                               , Mul.b2D2N3C3D3H4D4H5C
                               ]
                  Mul.b2D2N3C3D3H4D
             )
           , ( do TD.b2D2N3C
                  makePass
                  TD.b2D2N3C3D
                  makePass
                  TD.b2D2N3C3D3S
                  makePass
             , do alternatives [ Mul.b2D2N3C3D3S4D4HP
                               , Mul.b2D2N3C3D3S4D4H4S
                               , Mul.b2D2N3C3D3S4D4H5C
                               ]
                  Mul.b2D2N3C3D3S4D
             )
           , ( do TD.b2D2N3C
                  makePass
                  TD.b2D2N3C3D
                  makePass
                  TD.b2D2N3C3D3N
                  makePass
             , do alternatives [ Mul.b2D2N3C3D3N4D4HP
                               , Mul.b2D2N3C3D3N4D4H4S
                               , Mul.b2D2N3C3D3N4D4H5C
                               ]
                  Mul.b2D2N3C3D3N4D
             )
           , ( do TD.b2D2N3D
                  makePass
               -- You could set trump to a major at the 3 level, so just focus
               -- on signing off in clubs.
             , do alternatives [Mul.b2D2N3D4D4H5C]
                  Mul.b2D2N3D4D
             )
           , ( do TD.b2D2N3H
                  makePass
             -- You could set trump with 3S, so make sure trump is hearts or
             -- clubs.
             , do alternatives [ Mul.b2D2N3H4D4HP
                               , Mul.b2D2N3H4D4H5C
                               ]
                  Mul.b2D2N3H4D
             )
           , ( do TD.b2D2N3S
                  makePass
             , do alternatives [ Mul.b2D2N3S4D4HP
                               , Mul.b2D2N3S4D4H4S
                               , Mul.b2D2N3S4D4H5C
                               ]
                  Mul.b2D2N3S4D
             )
           ]


-- TODO:
--   - Make a 4D bid
--   - Relay 4H over 4D
--   - Pass or correct over 4D-4H
--   - Make a keycard ask
--   - Add auctions starting with 1C
--   - Over auctions starting 1C, bid 4C
--   - Over auctions starting 1C and a 4C bid, relay 4D
--   - Over auctions starting 1C and a 4C-4D relay, bid trump
--   - Over auctions starting 1C and a 4C-4D-trump, pass
--   - Over auctions starting 1C and a 4C-4D-trump, investigate slam


topic :: Topic
topic = makeTopic description "mulberry over SMP 3-suiters" situations
  where
    description = ("SMP " .+ T.Bid 2 T.Diamonds .+ " auctions")
    situations = wrap [ initiateSignoff
                      ]
