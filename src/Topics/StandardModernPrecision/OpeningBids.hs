module Topics.StandardModernPrecision.OpeningBids(topic) where

import Output((.+))
import Situation(situation, (<~))
import qualified Terminology as T
import Topic(Topic, wrap, Situations, makeTopic)
import qualified Topics.StandardModernPrecision.BasicBids as B


oneClub :: Situations
oneClub = let
    action = do
        B.firstSeatOpener
    explanation =
        "With 16 or more points (17 or more when balanced), open a strong " .+
        T.Bid 1 T.Clubs .+ ". This is the hallmark of SMP."
  in
    B.smpWrapS . return $ situation "1C" action B.b1C explanation


oneDiamond :: Situations
oneDiamond = let
    action = do
        B.firstSeatOpener
    explanation =
        "With opening strength but the wrong strength/shape for any other\
      \ opening bid, start with " .+ T.Bid 1 T.Diamonds .+ "."
  in
    B.smpWrapS . return $ situation "1D" action B.b1D explanation


oneMajor :: Situations
oneMajor = let
    sit suit = let
        action = do
            B.firstSeatOpener
            -- TODO: What if you're 5-5?
        explanation =
            "With opening strength but not enough for a strong " .+
            T.Bid 1 T.Clubs .+ " or " .+ T.Bid 1 T.Notrump .+ ", open a 5-card\
          \ major suit."
      in
        situation "1M" action (B.b1M suit) explanation
  in
    -- TODO: figure out some syntactic sugar for this, too
    B.smpWrapS $ return sit <~ T.majorSuits


oneNotrump :: Situations
oneNotrump = let
    action = do
        B.firstSeatOpener
    explanation =
        "With a balanced hand and 14-16 HCP, open " .+ T.Bid 1 T.Notrump .+ "."
  in
    B.smpWrapS . return $ situation "1N" action B.b1N explanation


twoClubs :: Situations
twoClubs = let
    action = do
        B.firstSeatOpener
    explanation =
        "With a 6-card club suit, no 5-card major, an opening hand but not a\
       \ hand strong enough to open " .+ T.Bid 1 T.Clubs .+ ",\
       \ open " .+ T.Bid 2 T.Clubs .+ "."
  in
    B.smpWrapS . return $ situation "2C" action B.b2C explanation


twoDiamonds :: Situations
twoDiamonds = let
    action = do
        B.firstSeatOpener
    explanation =
        "The " .+ T.Bid 2 T.Diamonds .+ " hand is that 3-suited\
       \ hand without diamonds, which can be thought of as a 14-card hand with\
       \ 4415 shape but missing any single card."
  in
    B.smpWrapS . return $ situation "2D" action B.b2D explanation


twoNotrump :: Situations
twoNotrump = let
    action = do
        B.firstSeatOpener
    explanation =
        "With a balanced hand and 19 or 20 HCP, open " .+
        T.Bid 2 T.Notrump .+ "."
  in
    B.smpWrapS . return $ situation "2N" action B.b2N explanation


topic :: Topic
topic = makeTopic "SMP opening bids" "SmpOpen" situations
  where
    situations = wrap [ oneClub
                      , oneDiamond
                      , oneMajor
                      , oneNotrump
                      , twoClubs
                      , twoDiamonds
                      , twoNotrump
                      ]
