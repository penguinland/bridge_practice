module Topics.StandardModernPrecision.Mafia(topic) where

import Output(output)
import Topic(Topic(..), wrap, Situations)
import Auction(withholdBid, forbid, {-makePass, maxSuitLength, -} minSuitLength, suitLength,
               {-Action,-} balancedHand, pointRange, SuitLengthComparator(..), compareSuitLength)
import Situation(situation, base, (<~))
--import CommonBids(cannotPreempt)
import qualified Terminology as T
import qualified Topics.StandardModernPrecision.Bids as B


notrump :: Situations
notrump = let
    sit (minHcp, maxHcp, level) = let
        action = do
            B.startOfMafia
            balancedHand
            pointRange minHcp maxHcp
            -- NOTE: we're not using B.b1C1D1N or B.b1C1D2N here. Maybe we
            -- should be? Not sure.
        explanation fmt =
            "With " ++ show minHcp ++ "--" ++ show maxHcp ++ " HCP and a\
           \ balanced hand, rebid " ++ output fmt (T.Bid level T.Notrump) ++
            ". Even if you have a 5-card major, the comfort of knowing that\
           \ systems are on is preferable."
      in
        situation "xN" action (T.Bid level T.Notrump) explanation
  in
    B.smpWrapS $ base sit <~ [(17, 18, 1), (22, 24, 2)]


oneMajor :: Situations
oneMajor = let
    sit (majorSuit, bid) = let
        action = do
            B.startOfMafia
            forbid balancedHand
            withholdBid bid
        explanation _ =
            "With an unbalanced hand that isn't game forcing, bid a 4-card\
           \ major if you have one."
      in
        situation "1M" action (T.Bid 1 majorSuit) explanation
  in
    B.smpWrapS $ base sit <~ [(T.Hearts, B.b1C1D1H), (T.Spades, B.b1C1D1S)]


oneMajorMinor :: Situations
oneMajorMinor = let
    sit (majorSuit, bid) minorSuit = let
        action = do
            B.startOfMafia
            forbid balancedHand
            minSuitLength minorSuit 5
            suitLength majorSuit 4
            withholdBid bid
        explanation _ =
            "With an unbalanced hand that isn't game forcing, bid a 4-card\
           \ major if you have one. This holds even if you've got a longer\
           \ minor (MAjors FIrst Always)."
      in
        situation "1Mm" action (T.Bid 1 majorSuit) explanation
  in
    B.smpWrapS $ base sit <~ [(T.Hearts, B.b1C1D1H), (T.Spades, B.b1C1D1S)]
                          <~ T.minorSuits


twoMinorSingle :: Situations
twoMinorSingle = let
    sit (minorSuit, bid) = let
        action = do
            B.startOfMafia
            forbid balancedHand
            minSuitLength minorSuit 6
            withholdBid bid
        explanation _ =
            "With an unbalanced hand that isn't game forcing and doesn't have\
           \ a 4-card major, rebid your long minor."
      in
        situation "2m" action (T.Bid 2 minorSuit) explanation
  in
    B.smpWrapS $ base sit <~ [(T.Clubs, B.b1C1D2C), (T.Diamonds, B.b1C1D2D)]


twoMinorMinors :: Situations
twoMinorMinors = let
    sit (minorSuit, bid) = let
        action = do
            B.startOfMafia
            forbid balancedHand
            suitLength minorSuit 5
            withholdBid bid
        explanation _ =
            "With an unbalanced hand that isn't game forcing and doesn't have\
           \ a 4-card major, rebid your long minor. Sometimes this minor is\
           \ only 5 cards, if you're 5-4 in the minors."
      in
        situation "2mm" action (T.Bid 2 minorSuit) explanation
  in
    B.smpWrapS $ base sit <~ [(T.Clubs, B.b1C1D2C), (T.Diamonds, B.b1C1D2D)]


equalMinors :: Situations
equalMinors = let
    sit = let
        action = do
            B.startOfMafia
            forbid balancedHand
            compareSuitLength T.Clubs Equal T.Diamonds
            withholdBid B.b1C1D2D
        explanation _ =
            "With an unbalanced hand that isn't game forcing and doesn't have\
           \ a 4-card major, rebid your long minor. When the minors are the\
           \ same length, bid diamonds first so that you can bid clubs later\
           \ without reversing."
      in
        situation "2me" action (T.Bid 2 T.Diamonds) explanation
  in
    B.smpWrapS $ base sit


bothMajorsLongSpades :: Situations
bothMajorsLongSpades = let
    sit = let
        action = do
            B.startOfMafia
            forbid balancedHand
            minSuitLength T.Hearts 4
            compareSuitLength T.Spades Longer T.Hearts
        explanation fmt =
            "With both majors but longer spades, start by bidding " ++
            output fmt (T.Bid 1 T.Spades) ++ ". You can then bid the hearts\
           \ later without reversing."
      in
        situation "2BS" action (T.Bid 1 T.Spades) explanation
  in
    B.smpWrapS $ base sit


topic :: Topic
topic = Topic "MaFiA" "MaFiA" situations
  where
    situations = wrap [ notrump
                      , wrap [oneMajor, oneMajorMinor]
                      , wrap [twoMinorSingle, twoMinorMinors]
                      -- Unusual cases
                      , wrap [equalMinors, bothMajorsLongSpades]
{-
                      , jumpBid
-}
                      ]
