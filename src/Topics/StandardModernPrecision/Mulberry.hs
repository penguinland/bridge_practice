module Topics.StandardModernPrecision.Mulberry(topic) where

import Control.Monad(join)
import Control.Monad.Trans.State.Strict(State)
import Data.Tuple.Extra(first)
import System.Random(StdGen)

import Action(Action)
--import Bids.StandardModernPrecision.BasicBids(setOpener)
import qualified Bids.StandardModernPrecision.OneClub as OC
import qualified Bids.StandardModernPrecision.TwoDiamonds as TD
import qualified Bids.StandardModernPrecision.Mulberry as Mul
import Collection(collect, weightedList)
import CommonBids(cannotPreempt, andNextBidderIs, noInterference)
import EDSL(alternatives, makePass, suitLength)
import Output((.+))
import Situation(Situation, situation, (<~), (<<~))
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
    sit setup = let
        action = setup `andNextBidderIs` T.South
        explanation =
            "Partner's " .+ T.Bid 4 T.Diamonds .+ " asks us to relay to " .+
            "the next cheapest bid, as a way to start signing off in game. " .+
            "Complete the relay, and partner will pass or correct to the " .+
            "final contract."
      in
        situation "relSO" action Mul.b4D4H explanation
  in
    wrapDlr $ return sit
        <~ [ do TD.b2D   >> cannotPreempt >> makePass
                TD.b2D2N >> cannotPreempt >> makePass
                TD.b2D2N3C >> makePass
                TD.b2D2N3C3D   >> makePass
                TD.b2D2N3C3D3H >> makePass
                -- If spades were trump, we might bid 3S instead. Avoid that.
                alternatives [ Mul.b2D2N3C3D3H4D4HP
                             , Mul.b2D2N3C3D3H4D4H5C
                             ]
                Mul.b2D2N3C3D3H4D >> makePass
           , do TD.b2D   >> cannotPreempt >> makePass
                TD.b2D2N >> cannotPreempt >> makePass
                TD.b2D2N3C >> makePass
                TD.b2D2N3C3D   >> makePass
                TD.b2D2N3C3D3S >> makePass
                alternatives [ Mul.b2D2N3C3D3S4D4HP
                             , Mul.b2D2N3C3D3S4D4H4S
                             , Mul.b2D2N3C3D3S4D4H5C
                             ]
                Mul.b2D2N3C3D3S4D >> makePass
           , do TD.b2D   >> cannotPreempt >> makePass
                TD.b2D2N >> cannotPreempt >> makePass
                TD.b2D2N3C >> makePass
                TD.b2D2N3C3D   >> makePass
                TD.b2D2N3C3D3N >> makePass
                alternatives [ Mul.b2D2N3C3D3N4D4HP
                             , Mul.b2D2N3C3D3N4D4H4S
                             , Mul.b2D2N3C3D3N4D4H5C
                             ]
                Mul.b2D2N3C3D3N4D >> makePass
           , do TD.b2D   >> cannotPreempt >> makePass
                TD.b2D2N >> cannotPreempt >> makePass
                TD.b2D2N3D >> makePass
                -- You could set trump to a major at the 3 level, so just focus
                -- on signing off in clubs.
                alternatives [Mul.b2D2N3D4D4H5C]
                Mul.b2D2N3D4D >> makePass
           , do TD.b2D   >> cannotPreempt >> makePass
                TD.b2D2N >> cannotPreempt >> makePass
                TD.b2D2N3H >> makePass
                -- You could set trump with 3S, so make sure trump is hearts or
                -- clubs.
                alternatives [ Mul.b2D2N3H4D4HP
                             , Mul.b2D2N3H4D4H5C
                             ]
                Mul.b2D2N3H4D >> makePass
           , do TD.b2D   >> cannotPreempt >> makePass
                TD.b2D2N >> cannotPreempt >> makePass
                TD.b2D2N3S >> makePass
                alternatives [ Mul.b2D2N3S4D4HP
                             , Mul.b2D2N3S4D4H4S
                             , Mul.b2D2N3S4D4H5C
                             ]
                Mul.b2D2N3S4D >> makePass
           ]


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
                                     [ ( do TD.b2D2N3C3D      >> makePass
                                            TD.b2D2N3C3D3H    >> makePass
                                            Mul.b2D2N3C3D3H4D >> makePass
                                            Mul.b4D4H         >> makePass
                                       , [ Mul.b2D2N3C3D3H4D4HP
                                         , Mul.b2D2N3C3D3H4D4H5C
                                         ]
                                       )
                                     , ( do TD.b2D2N3C3D      >> makePass
                                            TD.b2D2N3C3D3S    >> makePass
                                            Mul.b2D2N3C3D3S4D >> makePass
                                            Mul.b4D4H         >> makePass
                                       , [ Mul.b2D2N3C3D3S4D4HP
                                         , Mul.b2D2N3C3D3S4D4H4S
                                         , Mul.b2D2N3C3D3S4D4H5C
                                         ]
                                       )
                                     , ( do TD.b2D2N3C3D      >> makePass
                                            TD.b2D2N3C3D3N    >> makePass
                                            Mul.b2D2N3C3D3N4D >> makePass
                                            Mul.b4D4H         >> makePass
                                       , [ Mul.b2D2N3C3D3N4D4HP
                                         , Mul.b2D2N3C3D3N4D4H4S
                                         , Mul.b2D2N3C3D3N4D4H5C
                                         ]
                                       )
                                     ]
                           , [ ( do TD.b2D2N3D    >> makePass
                                    Mul.b2D2N3D4D >> makePass
                                    Mul.b4D4H     >> makePass
                               , [Mul.b2D2N3D4D4H5C]
                               )
                             , ( do TD.b2D2N3H    >> makePass
                                    Mul.b2D2N3H4D >> makePass
                                    Mul.b4D4H     >> makePass
                               , [ Mul.b2D2N3H4D4HP
                                 , Mul.b2D2N3H4D4H5C
                                 ]
                               )
                             , ( do TD.b2D2N3S    >> makePass
                                    Mul.b2D2N3S4D >> makePass
                                    Mul.b4D4H     >> makePass
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
        sit' answer = situation "kcask" action answer explanation
      in
        return sit' <~ answers
  in
    wrapDlr . join $ return sit
        -- For auctions starting 1C-2S, we need the opponents not to interfere.
        -- Saying they can't bid over a 1C opener is pretty close.
        <~ [ -- With definite slam interest over a singleton club, you should
             -- probably set trump at the 3 level and control bid. Skip this
             -- first auction.
             --( do OC.b1C       >> noInterference T.Clubs
             --     OC.b1C2S     >> noInterference T.Clubs
             --     OC.b1C2S2N   >> makePass
             --     OC.b1C2S2N3C >> makePass
             --, [ Mul.b1C2S2N3C4H
             --  , Mul.b1C2S2N3C4S
             --  , Mul.b1C2S2N3C4N
             --  ]
             --)
             ( do OC.b1C       >> noInterference T.Clubs
                  OC.b1C2S     >> noInterference T.Clubs
                  OC.b1C2S2N   >> makePass
                  OC.b1C2S2N3D >> makePass
             -- With definite slam interest in a major, you might set trump at
             -- the 3 level instead.
             , [ Mul.b1C2S2N3D4H
               --, Mul.b1C2S2N3D4S
               --, Mul.b1C2S2N3D4N
               ]
             )
           , ( do OC.b1C       >> noInterference T.Clubs
                  OC.b1C2S     >> noInterference T.Clubs
                  OC.b1C2S2N   >> makePass
                  OC.b1C2S2N3H >> makePass
             , [ Mul.b1C2S2N3H4H
               , Mul.b1C2S2N3H4S
               -- With slam interest in spades, probably bid 3S instead.
               --, Mul.b1C2S2N3H4N
               ]
             )
           , ( do OC.b1C       >> noInterference T.Clubs
                  OC.b1C2S     >> noInterference T.Clubs
                  OC.b1C2S2N   >> makePass
                  OC.b1C2S2N3S >> makePass
             , [ Mul.b1C2S2N3S4H
               , Mul.b1C2S2N3S4S
               , Mul.b1C2S2N3S4N
               ]
             )
           , ( do TD.b2D   >> cannotPreempt >> makePass
                  TD.b2D2N >> cannotPreempt >> makePass
                  TD.b2D2N3C                >> makePass
                  TD.b2D2N3C3D              >> makePass
                  TD.b2D2N3C3D3H            >> makePass
             , [ Mul.b2D2N3C3D3H4H
               , Mul.b2D2N3C3D3H4S
               -- You could bid 3S instead
               --, Mul.b2D2N3C3D3H4N
               ]
             )
           , ( do TD.b2D   >> cannotPreempt >> makePass
                  TD.b2D2N >> cannotPreempt >> makePass
                  TD.b2D2N3C                >> makePass
                  TD.b2D2N3C3D              >> makePass
                  TD.b2D2N3C3D3S            >> makePass
             , [ Mul.b2D2N3C3D3S4H
               , Mul.b2D2N3C3D3S4S
               , Mul.b2D2N3C3D3S4N
               ]
             )
           , ( do TD.b2D   >> cannotPreempt >> makePass
                  TD.b2D2N >> cannotPreempt >> makePass
                  TD.b2D2N3C                >> makePass
                  TD.b2D2N3C3D              >> makePass
                  -- Performance optimization: predeal opener's entire shape.
                  suitLength T.Clubs 4
                  suitLength T.Diamonds 1
                  TD.b2D2N3C3D3N            >> makePass
             , [ Mul.b2D2N3C3D3N4H
               , Mul.b2D2N3C3D3N4S
               , Mul.b2D2N3C3D3N4N
               ]
             )
           , ( do TD.b2D   >> cannotPreempt >> makePass
                  TD.b2D2N >> cannotPreempt >> makePass
                  -- Performance optimization: predeal opener's entire shape.
                  suitLength T.Clubs 5
                  suitLength T.Diamonds 0
                  TD.b2D2N3D >> makePass
             , [ Mul.b2D2N3D4H
               -- To play in a major, bid it at the 3 level.
               --, Mul.b2D2N3D4S
               --, Mul.b2D2N3D4N
               ]
             )
           , ( do TD.b2D   >> cannotPreempt >> makePass
                  TD.b2D2N >> cannotPreempt >> makePass
                  TD.b2D2N3H >> makePass
             , [ Mul.b2D2N3H4H
               , Mul.b2D2N3H4S
               -- You could bid 3S instead
               --, Mul.b2D2N3H4N
               ]
             )
           , ( do TD.b2D   >> cannotPreempt >> makePass
                  TD.b2D2N >> cannotPreempt >> makePass
                  TD.b2D2N3S >> makePass
             , [ Mul.b2D2N3S4H
               , Mul.b2D2N3S4S
               , Mul.b2D2N3S4N
               ]
             )
           ]


keycardResponse :: Situations
keycardResponse = let
    -- Help the compiler infer ambiguous types for `place` and `suit`
    sit :: ([(Action, Bool)], [Action], String, T.Suit) ->
           State StdGen (T.Direction -> T.Vulnerability -> Situation)
    sit (setups, answers, place, suit) = let
        sit' (setup, hasVoid) answer = let
            action = setup `andNextBidderIs` T.South
            explanation =
                "Partner has made a keycard ask in our " .+ place .+
                " suit, " .+ show suit .+ ". Give the right response." .+
                if hasVoid
                then " Even if we have a diamond void, just show a normal " .+
                     "keycard response. Partner already knows about our " .+
                     "major-suit shape and diamond shortness, so doesn't " .+
                     "care as much about extra shape info."
                else mempty
          in
            situation "kcresp" action answer explanation
      in
        return sit' <~ setups <~ answers
  in
    wrapDlr . join $ return sit <<~ (collect . weightedList)
        -- NOTE: Skip auctions where you could set trump at the 3 level (e.g.,
        -- starting 1C-2S and the trump suit is higher than the singleton).
        -- NOTE: We have 6 auctions starting with 1C and 18(ish) auctions
        -- starting with 2D, so to balance them out we'll weight the 1C auctions
        -- 3 times higher than expected.
        [ (9, ( [ ( do OC.b1C          >> noInterference T.Clubs
                       OC.b1C2S        >> noInterference T.Clubs
                       OC.b1C2S2N      >> makePass
                       OC.b1C2S2N3D    >> makePass
                       Mul.b1C2S2N3D4H >> makePass
                  , False)
                , ( do OC.b1C          >> noInterference T.Clubs
                       OC.b1C2S        >> noInterference T.Clubs
                       OC.b1C2S2N      >> makePass
                       OC.b1C2S2N3H    >> makePass
                       Mul.b1C2S2N3H4H >> makePass
                  , False)
                , ( do OC.b1C          >> noInterference T.Clubs
                       OC.b1C2S        >> noInterference T.Clubs
                       OC.b1C2S2N      >> makePass
                       OC.b1C2S2N3S    >> makePass
                       Mul.b1C2S2N3S4H >> makePass
                  , False)
                ]
              , [ Mul.bKCC4H4S, Mul.bKCC4H4N, Mul.bKCC4H5C, Mul.bKCC4H5D]
              , "lowest", T.Clubs
              )
          )
        , (6, ( [ ( do OC.b1C          >> noInterference T.Clubs
                       OC.b1C2S        >> noInterference T.Clubs
                       OC.b1C2S2N      >> makePass
                       OC.b1C2S2N3H    >> makePass
                       Mul.b1C2S2N3H4S >> makePass
                  , False)
                , ( do OC.b1C          >> noInterference T.Clubs
                       OC.b1C2S        >> noInterference T.Clubs
                       OC.b1C2S2N      >> makePass
                       OC.b1C2S2N3S    >> makePass
                       Mul.b1C2S2N3S4S >> makePass
                  , False)
                ]
              , [ Mul.bKCD4S4N, Mul.bKCD4S5C, Mul.bKCD4S5D, Mul.bKCD4S5H]
              , "middle", T.Diamonds
              )
          )
        , (3, ( [ ( do OC.b1C          >> noInterference T.Clubs
                       OC.b1C2S        >> noInterference T.Clubs
                       OC.b1C2S2N      >> makePass
                       OC.b1C2S2N3S    >> makePass
                       Mul.b1C2S2N3S4N >> makePass
                  , False)
                ]
              , [ Mul.bKCH4N5C, Mul.bKCH4N5D, Mul.bKCH4N5H, Mul.bKCH4N5S]
              , "highest", T.Hearts
              )
          )
        -- Performance optimization: always specify the exact shape of the 2D
        -- opener, or else the dealer engine struggles to find a deal.
        , (5, ( [ ( do TD.b2D            >> cannotPreempt >> makePass
                       TD.b2D2N          >> cannotPreempt >> makePass
                       TD.b2D2N3C        >> makePass
                       TD.b2D2N3C3D      >> makePass
                       TD.b2D2N3C3D3H    >> makePass
                       Mul.b2D2N3C3D3H4H >> makePass
                  , False)
                , ( do TD.b2D            >> cannotPreempt >> makePass
                       TD.b2D2N          >> cannotPreempt >> makePass
                       TD.b2D2N3C        >> makePass
                       TD.b2D2N3C3D      >> makePass
                       TD.b2D2N3C3D3S    >> makePass
                       Mul.b2D2N3C3D3S4H >> makePass
                  , False)
                -- Performance optimization: This one periodically times out.
                -- Just give up on it. If it comes up at the table, you've had
                -- enough practice with the others that you'll know what to do
                -- here, too.
                --, ( do TD.b2D >> cannotPreempt >> makePass
                --       TD.b2D2N >> cannotPreempt >> makePass
                --       TD.b2D2N3C >> makePass
                --       TD.b2D2N3C3D >> makePass
                --       -- Performance improvement: 4414 shape is about 4 times
                --       -- more common than 4405, which sometimes times out. So,
                --       -- only do 4414 shape.
                --       suitLength T.Clubs 4 >> suitLength T.Diamonds 1
                --       TD.b2D2N3C3D3N >> makePass
                --       Mul.b2D2N3C3D3N4H >> makePass
                --  , False)
                , ( do TD.b2D        >> cannotPreempt >> makePass
                       TD.b2D2N      >> cannotPreempt >> makePass
                       suitLength T.Clubs 4
                       suitLength T.Diamonds 1
                       TD.b2D2N3D    >> makePass
                       Mul.b2D2N3D4H >> makePass
                  , False)
                , ( do TD.b2D        >> cannotPreempt >> makePass
                       TD.b2D2N      >> cannotPreempt >> makePass
                       TD.b2D2N3H    >> makePass
                       Mul.b2D2N3H4H >> makePass
                  , False)
                , ( do TD.b2D        >> cannotPreempt >> makePass
                       TD.b2D2N      >> cannotPreempt >> makePass
                       TD.b2D2N3S    >> makePass
                       Mul.b2D2N3S4H >> makePass
                  , False)
                ]
              , [ Mul.bKCC4H4S, Mul.bKCC4H4N, Mul.bKCC4H5C, Mul.bKCC4H5D]
              , "lowest", T.Clubs
              )
          )
        , (6, ( [ ( do TD.b2D            >> cannotPreempt >> makePass
                       TD.b2D2N          >> cannotPreempt >> makePass
                       TD.b2D2N3C        >> makePass
                       TD.b2D2N3C3D      >> makePass
                       TD.b2D2N3C3D3H    >> makePass
                       Mul.b2D2N3C3D3H4S >> makePass
                  , False)
                , ( do TD.b2D            >> cannotPreempt >> makePass
                       TD.b2D2N          >> cannotPreempt >> makePass
                       TD.b2D2N3C        >> makePass
                       TD.b2D2N3C3D      >> makePass
                       TD.b2D2N3C3D3S    >> makePass
                       Mul.b2D2N3C3D3S4S >> makePass
                  , False)
                , ( do TD.b2D            >> cannotPreempt >> makePass
                       TD.b2D2N          >> cannotPreempt >> makePass
                       TD.b2D2N3C        >> makePass
                       TD.b2D2N3C3D      >> makePass
                       suitLength T.Clubs 5
                       suitLength T.Diamonds 0
                       TD.b2D2N3C3D3N    >> makePass
                       Mul.b2D2N3C3D3N4S >> makePass
                  , True)
                , ( do TD.b2D        >> cannotPreempt >> makePass
                       TD.b2D2N      >> cannotPreempt >> makePass
                       suitLength T.Clubs 4
                       suitLength T.Diamonds 1
                       TD.b2D2N3D    >> makePass
                       Mul.b2D2N3D4S >> makePass
                  , False)
                , ( do TD.b2D        >> cannotPreempt >> makePass
                       TD.b2D2N      >> cannotPreempt >> makePass
                       TD.b2D2N3H    >> makePass
                       Mul.b2D2N3H4S >> makePass
                  , False)
                , ( do TD.b2D        >> cannotPreempt >> makePass
                       TD.b2D2N      >> cannotPreempt >> makePass
                       TD.b2D2N3S    >> makePass
                       Mul.b2D2N3S4S >> makePass
                  , False)
                ]
              , [ Mul.bKCH4S4N, Mul.bKCH4S5C, Mul.bKCH4S5D, Mul.bKCH4S5H]
              , "middle", T.Hearts
              )
          )
        , (6, ( [ ( do TD.b2D            >> cannotPreempt >> makePass
                       TD.b2D2N          >> cannotPreempt >> makePass
                       TD.b2D2N3C        >> makePass
                       TD.b2D2N3C3D      >> makePass
                       TD.b2D2N3C3D3H    >> makePass
                       Mul.b2D2N3C3D3H4N >> makePass
                  , False)
                , ( do TD.b2D            >> cannotPreempt >> makePass
                       TD.b2D2N          >> cannotPreempt >> makePass
                       TD.b2D2N3C        >> makePass
                       TD.b2D2N3C3D      >> makePass
                       TD.b2D2N3C3D3S    >> makePass
                       Mul.b2D2N3C3D3S4N >> makePass
                  , False)
                , ( do TD.b2D            >> cannotPreempt >> makePass
                       TD.b2D2N          >> cannotPreempt >> makePass
                       TD.b2D2N3C        >> makePass
                       TD.b2D2N3C3D      >> makePass
                       suitLength T.Clubs 4
                       suitLength T.Diamonds 1
                       TD.b2D2N3C3D3N    >> makePass
                       Mul.b2D2N3C3D3N4N >> makePass
                  , False)
                , ( do TD.b2D        >> cannotPreempt >> makePass
                       TD.b2D2N      >> cannotPreempt >> makePass
                       suitLength T.Clubs 5
                       suitLength T.Diamonds 0
                       TD.b2D2N3D    >> makePass
                       Mul.b2D2N3D4N >> makePass
                  , True)
                , ( do TD.b2D        >> cannotPreempt >> makePass
                       TD.b2D2N      >> cannotPreempt >> makePass
                       TD.b2D2N3H    >> makePass
                       Mul.b2D2N3H4N >> makePass
                  , False)
                , ( do TD.b2D        >> cannotPreempt >> makePass
                       TD.b2D2N      >> cannotPreempt >> makePass
                       TD.b2D2N3S    >> makePass
                       Mul.b2D2N3S4N >> makePass
                  , False)
                ]
              , [ Mul.bKCS4N5C, Mul.bKCS4N5D, Mul.bKCS4N5H, Mul.bKCS4N5S]
              , "highest", T.Spades
            )
          )
        ]


inviteInit, inviteRelay, inviteSuit :: Situations
(inviteInit, inviteRelay, inviteSuit) = (inviteInit', inviteRelay', inviteSuit')
  where
    inviteInit' = let
        sit (setup, preRelay, trumpBids) = let
            action = do setup `andNextBidderIs` T.South
                        alternatives trumpBids
            explanation =
                "Partner has shown a 4441 shape. We don't have any slam " .+
                "interest on our own, but partner is unlimited and " .+
                "might not want to sign off in game. Make a very mild " .+
                "slam try by bidding " .+ preRelay .+ ". Partner will " .+
                "relay to " .+ Mul.b4C4D .+ ", after which you can set " .+
                "trump. Partner can then decide whether to pass or " .+
                "investigate slam."
          in
            situation "invinit" action preRelay explanation
      in
        wrapDlr $ return sit <~ auctions

    inviteRelay' = let
        sit (setup, preRelay, trumpBids) = let
            action = do setup `andNextBidderIs` T.North
                        alternatives trumpBids
                        _ <- preRelay
                        makePass
            explanation =
                "We've shown our 4441 shape. Partner doesn't have " .+
                "obvious slam interest, but is giving us a chance to " .+
                "make a slam try if we're interested. Relay to " .+
                Mul.b4C4D .+ ", after which partner will set trump, " .+
                "and we can decide whether to pass or look for slam."
          in
            situation "invrel" action Mul.b4C4D explanation
      in
        wrapDlr $ return sit <~ auctions

    inviteSuit' = let
        sit (setup, preRelay, trumpBids) = let
            sit' answer = let
                action = do setup `andNextBidderIs` T.South
                            _ <- preRelay
                            makePass
                            Mul.b4C4D
                            makePass
                explanation =
                    "Partner showed their 4441 shape. We bid " .+
                    T.Bid 4 T.Clubs .+ " to show that we don't have slam " .+
                    "interest on our own, but are open to partner having " .+
                    "some. Partner relayed " .+ Mul.b4C4D .+ ", and now " .+
                    "it's time for us to bid the trump suit. Partner can " .+
                    "then pass or try looking for slam, depending on how " .+
                    "strong they really are."
              in
                situation "invsuit" action answer explanation
          in
            return sit' <~ trumpBids
      in
        wrapDlr . join $ return sit <~ auctions

    auctions = [ -- Focus on setting trump to something you couldn't bid at the
                 -- 3 level.
                 --( do OC.b1C       >> noInterference T.Clubs
                 --     OC.b1C2S     >> noInterference T.Clubs
                 --     OC.b1C2S2N   >> makePass
                 --     OC.b1C2S2N3C >> makePass
                 --, Mul.b1C2S2N3A4C
                 --, [ Mul.b1C2S2N3C4C4D4H
                 --  , Mul.b1C2S2N3C4C4D4S
                 --  , Mul.b1C2S2N3C4C4D5D
                 --  ]
                 --)
                 ( do OC.b1C       >> noInterference T.Clubs
                      OC.b1C2S     >> noInterference T.Clubs
                      OC.b1C2S2N   >> makePass
                      OC.b1C2S2N3D >> makePass
                 , Mul.b1C2S2N3A4C
                 , [ --Mul.b1C2S2N3D4C4D4H
                   --, Mul.b1C2S2N3D4C4D4S
                     Mul.b1C2S2N3D4C4D5C
                   ]
                 )
               , ( do OC.b1C       >> noInterference T.Clubs
                      OC.b1C2S     >> noInterference T.Clubs
                      OC.b1C2S2N   >> makePass
                      OC.b1C2S2N3H >> makePass
                 , Mul.b1C2S2N3A4C
                 , [ --Mul.b1C2S2N3S4C4D4S
                     Mul.b1C2S2N3S4C4D5C
                   , Mul.b1C2S2N3S4C4D5D
                   ]
                 )
               , ( do OC.b1C       >> noInterference T.Clubs
                      OC.b1C2S     >> noInterference T.Clubs
                      OC.b1C2S2N   >> makePass
                      OC.b1C2S2N3S >> makePass
                 , Mul.b1C2S2N3A4C
                 , [ Mul.b1C2S2N3S4C4D4H
                   , Mul.b1C2S2N3S4C4D5C
                   , Mul.b1C2S2N3S4C4D5D
                   ]
                 )
                 --( do OC.b1C         >> noInterference T.Clubs
                 --     OC.b1C1H       >> noInterference T.Clubs
                 --     OC.b1C1H2S     >> makePass
                 --     OC.b1C1H2S2N   >> makePass
                 --     OC.b1C1H2S2N3C >> makePass
                 --, Mul.b1C1H2S2N3A4C
                 --, [ Mul.b1C2S2N3C4C4D4H
                 --  , Mul.b1C2S2N3C4C4D4S
                 --  , Mul.b1C2S2N3C4C4D5D
                 --  ]
                 --)
               , ( do OC.b1C         >> noInterference T.Clubs
                      OC.b1C1H       >> noInterference T.Clubs
                      OC.b1C1H2S     >> makePass
                      OC.b1C1H2S2N   >> makePass
                      OC.b1C1H2S2N3D >> makePass
                 , Mul.b1C1H2S2N3A4C
                 , [ --Mul.b1C2S2N3D4C4D4H
                   --, Mul.b1C2S2N3D4C4D4S
                     Mul.b1C2S2N3D4C4D5C
                   ]
                 )
               , ( do OC.b1C         >> noInterference T.Clubs
                      OC.b1C1H       >> noInterference T.Clubs
                      OC.b1C1H2S     >> makePass
                      OC.b1C1H2S2N   >> makePass
                      OC.b1C1H2S2N3H >> makePass
                 , Mul.b1C1H2S2N3A4C
                 , [ --Mul.b1C2S2N3H4C4D4S
                     Mul.b1C2S2N3H4C4D5C
                   , Mul.b1C2S2N3H4C4D5D
                   ]
                 )
               , ( do OC.b1C         >> noInterference T.Clubs
                      OC.b1C1H       >> noInterference T.Clubs
                      OC.b1C1H2S     >> noInterference T.Clubs
                      OC.b1C1H2S2N   >> makePass
                      OC.b1C1H2S2N3S >> makePass
                 , Mul.b1C1H2S2N3A4C
                 , [ Mul.b1C2S2N3S4C4D4H
                   , Mul.b1C2S2N3S4C4D5C
                   , Mul.b1C2S2N3S4C4D5D
                   ]
                 )
               ]


-- TODO:
--   - Make a situation to bid naturally at the 3 level when we definitely don't
--     want to stop in game.
--   - Include auctions that start P-1C-2S and thus could bid 4D-4H to sign off
--   - For trump suits besides hearts, make 4D the weakest bid and make 4C a
--     slighly stronger bid. When hearts will be trump, don't do this. Make
--     situations that clarify these distinctions


topic :: Topic
topic = makeTopic "mulberry over SMP 3-suiters" "mulb" situations
  where
    situations = wrap [ initiateSignoff
                      , relaySignoff
                      , completeSignoff
                      , keycardAsk
                      , keycardResponse
                      , inviteInit
                      , inviteRelay
                      , inviteSuit
                      ]
