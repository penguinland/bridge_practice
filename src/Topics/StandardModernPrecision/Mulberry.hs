module Topics.StandardModernPrecision.Mulberry(topic) where

import Control.Monad(join)
import Data.Tuple.Extra(first)

import Action(Action)
--import Bids.StandardModernPrecision.BasicBids(setOpener)
import qualified Bids.StandardModernPrecision.TwoDiamonds as TD
import qualified Bids.StandardModernPrecision.Mulberry as Mul
import CommonBids(cannotPreempt, andNextBidderIs)
import EDSL(alternatives, makePass)
import Output((.+))
import Situation(situation, (<~))
import qualified Terminology as T
import Topic(Topic, wrap, wrapDlr, Situations, makeTopic)


-- This should probably be moved somewhere more general, if it turns out to be
-- as useful as I hope.
bidTree :: Action -> [(Action, s)] -> [(Action, s)]
bidTree a ts = map (first (a >>)) ts


initiateSignoff :: Situations
initiateSignoff = let
    sit (setup, answer) = let
        action = setup `andNextBidderIs` T.South
        explanation =
            "Partner has limited their hand: even if we're a maximum, we " .+
            "have no interest in slam. Bid " .+ answer .+ " to indicate " .+
            "this. Partner will relay " .+ T.Bid 4 T.Hearts .+ ", which " .+
            "you can pass or correct to the final trump suit."
      in
        situation "initSO" action answer explanation
  in
    wrapDlr $ return sit
        <~ bidTree (do TD.b2D   >> cannotPreempt >> makePass
                       TD.b2D2N >> cannotPreempt >> makePass
                   )
                   (concat [ bidTree (TD.b2D2N3C >> makePass)
                                     [ ( do TD.b2D2N3C3D   >> makePass
                                            TD.b2D2N3C3D3H >> makePass
                                       , do alternatives [ Mul.b2D2N3C3D3H4D4HP
                                                         , Mul.b2D2N3C3D3H4D4H5C
                                                         ]
                                            Mul.b2D2N3C3D3H4D
                                       )
                                     , ( do TD.b2D2N3C3D   >> makePass
                                            TD.b2D2N3C3D3S >> makePass
                                       , do alternatives [ Mul.b2D2N3C3D3S4D4HP
                                                         , Mul.b2D2N3C3D3S4D4H4S
                                                         , Mul.b2D2N3C3D3S4D4H5C
                                                         ]
                                            Mul.b2D2N3C3D3S4D
                                       )
                                     , ( do TD.b2D2N3C3D   >> makePass
                                            TD.b2D2N3C3D3N >> makePass
                                       , do alternatives [ Mul.b2D2N3C3D3N4D4HP
                                                         , Mul.b2D2N3C3D3N4D4H4S
                                                         , Mul.b2D2N3C3D3N4D4H5C
                                                         ]
                                            Mul.b2D2N3C3D3N4D
                                       )
                                     ]
                           , [ ( TD.b2D2N3D >> makePass
                                 -- You could set trump to a major at the 3
                                 -- level, so just focus on signing off in
                                 -- clubs.
                               , do alternatives [Mul.b2D2N3D4D4H5C]
                                    Mul.b2D2N3D4D
                               )
                             , ( TD.b2D2N3H >> makePass
                               -- You could set trump with 3S, so make sure
                               -- trump is hearts or clubs.
                               , do alternatives [ Mul.b2D2N3H4D4HP
                                                 , Mul.b2D2N3H4D4H5C
                                                 ]
                                    Mul.b2D2N3H4D
                               )
                             , ( TD.b2D2N3S >> makePass
                               , do alternatives [ Mul.b2D2N3S4D4HP
                                                 , Mul.b2D2N3S4D4H4S
                                                 , Mul.b2D2N3S4D4H5C
                                                 ]
                                    Mul.b2D2N3S4D
                               )
                             ]
                           ]
                   )


relaySignoff :: Situations
relaySignoff = let
    sit (setup, answer) = let
        action = setup `andNextBidderIs` T.South
        explanation =
            "Partner's " .+ T.Bid 4 T.Diamonds .+ " asks us to relay to " .+
            "the next cheapest bid, as a way to start signing off in game. " .+
            "Complete the relay, and partner will pass or correct to the " .+
            "final contract."
      in
        situation "relSO" action answer explanation
  in
    wrapDlr $ return sit
        <~ bidTree (do TD.b2D   >> cannotPreempt >> makePass
                       TD.b2D2N >> cannotPreempt >> makePass
                   )
                   (concat [ bidTree (TD.b2D2N3C >> makePass)
                                     [ ( do TD.b2D2N3C3D   >> makePass
                                            TD.b2D2N3C3D3H >> makePass
                                            alternatives [ Mul.b2D2N3C3D3H4D4HP
                                                         , Mul.b2D2N3C3D3H4D4H5C
                                                         ]
                                            Mul.b2D2N3C3D3H4D >> makePass
                                       , Mul.b2D2N3C3D3H4D4H
                                       )
                                     , ( do TD.b2D2N3C3D   >> makePass
                                            TD.b2D2N3C3D3S >> makePass
                                            alternatives [ Mul.b2D2N3C3D3S4D4HP
                                                         , Mul.b2D2N3C3D3S4D4H4S
                                                         , Mul.b2D2N3C3D3S4D4H5C
                                                         ]
                                            Mul.b2D2N3C3D3S4D >> makePass
                                       , Mul.b2D2N3C3D3S4D4H
                                       )
                                     , ( do TD.b2D2N3C3D   >> makePass
                                            TD.b2D2N3C3D3N >> makePass
                                            alternatives [ Mul.b2D2N3C3D3N4D4HP
                                                         , Mul.b2D2N3C3D3N4D4H4S
                                                         , Mul.b2D2N3C3D3N4D4H5C
                                                         ]
                                            Mul.b2D2N3C3D3N4D >> makePass
                                       , Mul.b2D2N3C3D3N4D4H
                                       )
                                     ]
                           , [ ( do TD.b2D2N3D >> makePass
                                    -- You could set trump to a major at the 3
                                    -- level, so just focus on signing off in
                                    -- clubs.
                                    alternatives [Mul.b2D2N3D4D4H5C]
                                    Mul.b2D2N3D4D >> makePass
                               , Mul.b2D2N3D4D4H
                               )
                             , ( do TD.b2D2N3H >> makePass
                                     -- You could set trump with 3S, so make
                                     -- sure trump is hearts or clubs.
                                    alternatives [ Mul.b2D2N3H4D4HP
                                                 , Mul.b2D2N3H4D4H5C
                                                 ]
                                    Mul.b2D2N3H4D >> makePass
                               , Mul.b2D2N3H4D4H
                               )
                             , ( do TD.b2D2N3S >> makePass
                                    alternatives [ Mul.b2D2N3S4D4HP
                                                 , Mul.b2D2N3S4D4H4S
                                                 , Mul.b2D2N3S4D4H5C
                                                 ]
                                    Mul.b2D2N3S4D >> makePass
                               , Mul.b2D2N3S4D4H
                               )
                             ]
                           ]
                   )


completeSignoff :: Situations
completeSignoff = let
    sit (setup, answers) = let
        action = setup `andNextBidderIs` T.South
        explanation =
            "Our " .+ T.Bid 4 T.Diamonds .+ " initiated a signoff in game, " .+
            "and partner's relay has set us up to pass or correct to the " .+
            "final contract. Time to sign off."
        sit' answer = situation "SOSO" action answer explanation
      in
        return sit' <~ answers
  in
    wrapDlr . join $ return sit
        <~ bidTree (do TD.b2D   >> cannotPreempt >> makePass
                       TD.b2D2N >> cannotPreempt >> makePass
                   )
                   (concat [ bidTree (TD.b2D2N3C >> makePass)
                                     [ ( do TD.b2D2N3C3D        >> makePass
                                            TD.b2D2N3C3D3H      >> makePass
                                            Mul.b2D2N3C3D3H4D   >> makePass
                                            Mul.b2D2N3C3D3H4D4H >> makePass
                                       , [ Mul.b2D2N3C3D3H4D4HP
                                         , Mul.b2D2N3C3D3H4D4H5C
                                         ]
                                       )
                                     , ( do TD.b2D2N3C3D        >> makePass
                                            TD.b2D2N3C3D3S      >> makePass
                                            Mul.b2D2N3C3D3S4D   >> makePass
                                            Mul.b2D2N3C3D3S4D4H >> makePass
                                       , [ Mul.b2D2N3C3D3S4D4HP
                                         , Mul.b2D2N3C3D3S4D4H4S
                                         , Mul.b2D2N3C3D3S4D4H5C
                                         ]
                                       )
                                     , ( do TD.b2D2N3C3D        >> makePass
                                            TD.b2D2N3C3D3N      >> makePass
                                            Mul.b2D2N3C3D3N4D   >> makePass
                                            Mul.b2D2N3C3D3N4D4H >> makePass
                                       , [ Mul.b2D2N3C3D3N4D4HP
                                         , Mul.b2D2N3C3D3N4D4H4S
                                         , Mul.b2D2N3C3D3N4D4H5C
                                         ]
                                       )
                                     ]
                           , [ ( do TD.b2D2N3D      >> makePass
                                    Mul.b2D2N3D4D   >> makePass
                                    Mul.b2D2N3D4D4H >> makePass
                               , [Mul.b2D2N3D4D4H5C]
                               )
                             , ( do TD.b2D2N3H      >> makePass
                                    Mul.b2D2N3H4D   >> makePass
                                    Mul.b2D2N3H4D4H >> makePass
                               , [ Mul.b2D2N3H4D4HP
                                 , Mul.b2D2N3H4D4H5C
                                 ]
                               )
                             , ( do TD.b2D2N3S      >> makePass
                                    Mul.b2D2N3S4D   >> makePass
                                    Mul.b2D2N3S4D4H >> makePass
                               , [ Mul.b2D2N3S4D4HP
                                 , Mul.b2D2N3S4D4H4S
                                 , Mul.b2D2N3S4D4H5C
                                 ]
                               )
                             ]
                           ]
                   )


keycardAsk :: Situations
keycardAsk = let
    sit (setup, answers) = let
        action = setup `andNextBidderIs` T.South
        explanation =
            "We have slam interest but no room to set trump at the 3 level " .+
            "to start a round of control bidding. Instead, ask for " .+
            "keycards. This sets trump, so partner knows how to answer " .+
            "properly."
        sit' answer = situation "kask" action answer explanation
      in
        return sit' <~ answers
  in
    wrapDlr . join $ return sit
        <~ bidTree (do TD.b2D   >> cannotPreempt >> makePass
                       TD.b2D2N >> cannotPreempt >> makePass
                   )
                   (concat [ bidTree (TD.b2D2N3C >> makePass)
                                     [ ( do TD.b2D2N3C3D   >> makePass
                                            TD.b2D2N3C3D3H >> makePass
                                       , [ Mul.b2D2N3C3D3H4H
                                         , Mul.b2D2N3C3D3H4S
                                         -- You could bid 3S instead
                                         --, Mul.b2D2N3C3D3H4N
                                         ]
                                       )
                                     , ( do TD.b2D2N3C3D   >> makePass
                                            TD.b2D2N3C3D3S >> makePass
                                       , [ Mul.b2D2N3C3D3S4H
                                         , Mul.b2D2N3C3D3S4S
                                         , Mul.b2D2N3C3D3S4N
                                         ]
                                       )
                                     , ( do TD.b2D2N3C3D   >> makePass
                                            TD.b2D2N3C3D3N >> makePass
                                       , [ Mul.b2D2N3C3D3N4H
                                         , Mul.b2D2N3C3D3N4S
                                         , Mul.b2D2N3C3D3N4N
                                         ]
                                       )
                                     ]
                           , [ ( do TD.b2D2N3D >> makePass
                               -- To play in a major, bid it at the 3 level.
                               , [Mul.b2D2N3D4H]
                               )
                             , ( do TD.b2D2N3H >> makePass
                               , [ Mul.b2D2N3H4H
                                 , Mul.b2D2N3H4S
                                 -- You could bid 3S instead
                                 --, Mul.b2D2N3H4N
                                 ]
                               )
                             , ( do TD.b2D2N3S >> makePass
                               , [ Mul.b2D2N3S4H
                                 , Mul.b2D2N3S4S
                                 , Mul.b2D2N3S4N
                                 ]
                               )
                             ]
                           ]
                   )


-- TODO:
--   - Make a keycard ask
--   - Add auctions starting with 1C
--   - Over auctions starting 1C, bid 4C
--   - Over auctions starting 1C and a 4C bid, relay 4D
--   - Over auctions starting 1C and a 4C-4D relay, bid trump
--   - Over auctions starting 1C and a 4C-4D-trump, pass
--   - Over auctions starting 1C and a 4C-4D-trump, investigate slam


topic :: Topic
topic = makeTopic "mulberry over SMP 3-suiters" "mulb" situations
  where
    situations = wrap [ keycardAsk ]
    _situations = wrap [ initiateSignoff
                      , relaySignoff
                      , completeSignoff
                      , keycardAsk
                      ]
