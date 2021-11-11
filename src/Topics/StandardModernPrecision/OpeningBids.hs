module Topics.StandardModernPrecision.OpeningBids(topic) where

import Output(output)
import Topic(Topic(..), wrap, Situations)
import Auction(withholdBid)
import Situation(situation, base, (<~))
import qualified Terminology as T
import qualified Topics.StandardModernPrecision.Bids as B


oneClub :: Situations
oneClub = let
    action = do
        B.firstSeatOpener
        withholdBid B.b1C
    explanation fmt =
        "With 16 or more points (17 or more when balanced), open a strong " ++
        output fmt (T.Bid 1 T.Clubs) ++ ". This is the hallmark of SMP."
  in
    B.smpWrapS . base $ situation "1C" action (T.Bid 1 T.Clubs) explanation


oneDiamond :: Situations
oneDiamond = let
    action = do
        B.firstSeatOpener
        withholdBid B.b1D
    explanation fmt =
        "With opening strength but the wrong strength/shape for any other\
      \ opening bid, start with " ++ output fmt (T.Bid 1 T.Diamonds) ++ ".\
      \ Partner will announce that it ``could be as short as 2.''"
  in
    B.smpWrapS . base $ situation "1D" action (T.Bid 1 T.Diamonds) explanation


oneMajor :: Situations
oneMajor = let
    sit suit = let
        action = do
            B.firstSeatOpener
            withholdBid $ B.b1M suit
            -- TODO: What if you're 5-5?
        explanation fmt =
            "With opening strength but not enough for a strong " ++
            output fmt (T.Bid 1 T.Clubs) ++ " or " ++
            output fmt (T.Bid 1 T.Notrump) ++ ", open a 5-card major suit."
      in
        situation "1M" action (T.Bid 1 suit) explanation
  in
    -- TODO: figure out some syntactic sugar for this, too
    B.smpWrapS $ base sit <~ T.majorSuits


oneNotrump :: Situations
oneNotrump = let
    action = do
        B.firstSeatOpener
        withholdBid B.b1N
    explanation fmt =
        "With a balanced hand and 14-16 HCP, open " ++
        output fmt (T.Bid 1 T.Notrump) ++ "."
  in
    B.smpWrapS . base $ situation "1N" action (T.Bid 1 T.Notrump) explanation


twoClubs :: Situations
twoClubs = let
    action = do
        B.firstSeatOpener
        withholdBid B.b2C
    explanation fmt =
        "With a 6-card club suit, no 5-card major, an opening hand but not a\
       \ hand strong enough to open " ++ output fmt (T.Bid 1 T.Clubs) ++ ",\
       \ open " ++ output fmt (T.Bid 2 T.Clubs) ++ "."
  in
    B.smpWrapS . base $ situation "2C" action (T.Bid 2 T.Clubs) explanation


twoDiamonds :: Situations
twoDiamonds = let
    action = do
        B.firstSeatOpener
        withholdBid B.b2D
    explanation fmt =
        "The " ++ output fmt (T.Bid 2 T.Diamonds) ++ " hand is that 3-suited\
       \ hand without diamonds, which can be thought of as a 14-card hand with\
       \ 4415 shape but missing any single card."
  in
    B.smpWrapS . base $ situation "2D" action (T.Bid 2 T.Diamonds) explanation


twoNotrump :: Situations
twoNotrump = let
    action = do
        B.firstSeatOpener
        withholdBid B.b2N
    explanation fmt =
        "With a balanced hand and 19 to a bad 21 HCP, open " ++
        output fmt (T.Bid 2 T.Notrump) ++ "."
  in
    B.smpWrapS . base $ situation "2N" action (T.Bid 2 T.Notrump) explanation


topic :: Topic
topic = Topic "SMP opening bids" "SmpOpen" situations
  where
    situations = wrap [ oneClub
                      , oneDiamond
                      , oneMajor
                      , oneNotrump
                      , twoClubs
                      , twoDiamonds
                      , twoNotrump
                      ]
