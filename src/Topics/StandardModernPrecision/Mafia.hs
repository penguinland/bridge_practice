module Topics.StandardModernPrecision.Mafia(topic) where

import Auction(forbid, minSuitLength, suitLength, balancedHand, equalLength,
               longerThan)
import Output(Punct(..), (.+))
import Situation(situation, (<~))
import qualified Terminology as T
import Topic(Topic, wrap, Situations, makeTopic)
import Bids.StandardModernPrecision.BasicBids(smpWrapS)
import qualified Bids.StandardModernPrecision.OneClub as B


notrump :: Situations
notrump = let
    sit bid = let
        action = do
            B.startOfMafia
        explanation =
            "With a balanced hand, rebid notrump to show your point range.\
           \ Even if you have a 5-card major, it's better to know that\
           \ systems are on and partner knows your strength within 1 HCP."
      in
        situation "xN" action bid explanation
  in
    smpWrapS $ return sit <~ [B.b1C1D1N, B.b1C1D2N]


oneMajor :: Situations
oneMajor = let
    sit bid = let
        action = do
            B.startOfMafia
            forbid balancedHand
        explanation =
            "With an unbalanced hand that isn't game forcing, bid a 4-card\
           \ major if you have one."
      in
        situation "1M" action bid explanation
  in
    smpWrapS $ return sit <~ [B.b1C1D1H, B.b1C1D1S]


oneMajorMinor :: Situations
oneMajorMinor = let
    sit (majorSuit, bid) minorSuit = let
        action = do
            B.startOfMafia
            forbid balancedHand
            minSuitLength minorSuit 5
            suitLength majorSuit 4
        explanation =
            "With an unbalanced hand that isn't game forcing, bid a 4-card\
           \ major if you have one. This holds even if you've got a longer\
           \ minor (MAjors FIrst Always)."
      in
        situation "1Mm" action bid explanation
  in
    smpWrapS $ return sit <~ [(T.Hearts, B.b1C1D1H), (T.Spades, B.b1C1D1S)]
                          <~ T.minorSuits


twoMinorSingle :: Situations
twoMinorSingle = let
    sit (minorSuit, bid) = let
        action = do
            B.startOfMafia
            forbid balancedHand
            minSuitLength minorSuit 6
        explanation =
            "With an unbalanced hand that isn't game forcing and doesn't have\
           \ a 4-card major, bid your long minor."
      in
        situation "2m" action bid explanation
  in
    smpWrapS $ return sit <~ [(T.Clubs, B.b1C1D2C), (T.Diamonds, B.b1C1D2D)]


twoMinorMinors :: Situations
twoMinorMinors = let
    sit (minorSuit, bid) = let
        action = do
            B.startOfMafia
            forbid balancedHand
            suitLength minorSuit 5
            -- The answer Action also ensures that the minor we bid is longer
            -- than the minor we don't bid.
        explanation =
            "With an unbalanced hand that isn't game forcing and doesn't have\
           \ a 4-card major, rebid your long minor. Sometimes this minor is\
           \ only 5 cards, if you're 5-4 in the minors."
      in
        situation "2mm" action bid explanation
  in
    smpWrapS $ return sit <~ [(T.Clubs, B.b1C1D2C), (T.Diamonds, B.b1C1D2D)]


equalMinors :: Situations
equalMinors = let
    sit = let
        action = do
            B.startOfMafia
            forbid balancedHand
            T.Clubs `equalLength` T.Diamonds
        explanation =
            "With an unbalanced hand that isn't game forcing and doesn't have\
           \ a 4-card major, rebid your long minor. When the minors are the\
           \ same length, bid diamonds first so that you can bid clubs later\
           \ without reversing."
      in
        situation "2me" action B.b1C1D2D explanation
  in
    smpWrapS $ return sit


bothMajorsLongSpades :: Situations
bothMajorsLongSpades = let
    sit = let
        action = do
            B.startOfMafia
            forbid balancedHand
            minSuitLength T.Hearts 4
            T.Spades `longerThan` T.Hearts
        explanation =
            "With both majors but longer spades, start by bidding " .+
            T.Bid 1 T.Spades .+ ". You can then bid the hearts later without\
          \ reversing."
      in
        situation "2MS" action B.b1C1D1S explanation
  in
    smpWrapS $ return sit


jumpBid :: Situations
jumpBid = let
    sit bid = let
        action = do
            B.startOfMafia
        explanation =
            "With an unbalanced hand that is strong enough to\
           \ force to game, jump in your suit. Responder can treat this like\
           \ the 2/1 sequence " .+ T.Bid 2 T.Clubs .+ NDash .+
            T.Bid 2 T.Diamonds .+ NDash .+ bid .+ "."
      in
        situation "J1" action bid explanation
  in
    smpWrapS $ return sit <~ [B.b1C1D2H, B.b1C1D2S, B.b1C1D3C, B.b1C1D3D]


topic :: Topic
topic = makeTopic "MaFiA bids by opener" "MaFiA" situations
  where
    situations = wrap [ notrump
                      , wrap [oneMajor, oneMajorMinor]
                      , wrap [twoMinorSingle, twoMinorMinors]
                      -- Unusual cases
                      , wrap [equalMinors, bothMajorsLongSpades, jumpBid]
                      ]
