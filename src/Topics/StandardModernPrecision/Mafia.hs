module Topics.StandardModernPrecision.Mafia(topic) where

import Output(output)
import Topic(Topic(..), wrap, Situations)
import Auction(withholdBid, forbid, {-makePass, maxSuitLength, -} minSuitLength, suitLength,
               {-Action,-} balancedHand, pointRange, SuitLengthComparator(..), compareSuitLength, extractLastCall)
import Situation(situation, base, (<~))
--import CommonBids(cannotPreempt)
import qualified Terminology as T
import Topics.StandardModernPrecision.BasicBids(smpWrapS)
import qualified Topics.StandardModernPrecision.Bids1C as B


notrump :: Situations
notrump = let
    sit (minHcp, maxHcp, bid) = let
        action = do
            B.startOfMafia
            balancedHand
            pointRange minHcp maxHcp
            -- NOTE: we're not using B.b1C1D1N or B.b1C1D2N here. Maybe we
            -- should be? Not sure.
        explanation fmt =
            "With " ++ show minHcp ++ "--" ++ show maxHcp ++ " HCP and a\
           \ balanced hand, rebid " ++ output fmt (extractLastCall bid) ++
            ". Even if you have a 5-card major, the comfort of knowing that\
           \ systems are on is preferable."
      in
        situation "xN" action bid explanation
  in
    smpWrapS $ base sit <~ [(17, 18, B.b1C1D1N), (21, 23, B.b1C1D2N)]


oneMajor :: Situations
oneMajor = let
    sit bid = let
        action = do
            B.startOfMafia
            forbid balancedHand
            withholdBid bid
        explanation _ =
            "With an unbalanced hand that isn't game forcing, bid a 4-card\
           \ major if you have one."
      in
        situation "1M" action bid explanation
  in
    smpWrapS $ base sit <~ [B.b1C1D1H, B.b1C1D1S]


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
        situation "1Mm" action bid explanation
  in
    smpWrapS $ base sit <~ [(T.Hearts, B.b1C1D1H), (T.Spades, B.b1C1D1S)]
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
           \ a 4-card major, bid your long minor."
      in
        situation "2m" action bid explanation
  in
    smpWrapS $ base sit <~ [(T.Clubs, B.b1C1D2C), (T.Diamonds, B.b1C1D2D)]


twoMinorMinors :: Situations
twoMinorMinors = let
    sit (minorSuit, bid) = let
        action = do
            B.startOfMafia
            forbid balancedHand
            suitLength minorSuit 5
            withholdBid bid  -- Ensures clubs are longer than diamonds
        explanation _ =
            "With an unbalanced hand that isn't game forcing and doesn't have\
           \ a 4-card major, rebid your long minor. Sometimes this minor is\
           \ only 5 cards, if you're 5-4 in the minors."
      in
        situation "2mm" action bid explanation
  in
    smpWrapS $ base sit <~ [(T.Clubs, B.b1C1D2C), (T.Diamonds, B.b1C1D2D)]


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
        situation "2me" action B.b1C1D2D explanation
  in
    smpWrapS $ base sit


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
        situation "2MS" action B.b1C1D1S explanation
  in
    smpWrapS $ base sit


jumpBid :: Situations
jumpBid = let
    sit bid = let
        action = do
            B.startOfMafia
            withholdBid bid
        explanation fmt =
            "With an unbalanced hand that is strong enough to\
           \ force to game, jump in your suit. Responder can treat this like\
           \ the 2/1 sequence " ++
            output fmt (T.Bid 2 T.Clubs) ++ "--" ++
            output fmt (T.Bid 2 T.Diamonds) ++ "--" ++
            output fmt (extractLastCall bid) ++ "."
      in
        situation "J1" action bid explanation
  in
    smpWrapS $ base sit <~ [B.b1C1D2H, B.b1C1D2S, B.b1C1D3C, B.b1C1D3D]


topic :: Topic
topic = Topic "MaFiA bids by opener" "MaFiA" situations
  where
    situations = wrap [ notrump
                      , wrap [oneMajor, oneMajorMinor]
                      , wrap [twoMinorSingle, twoMinorMinors]
                      -- Unusual cases
                      , wrap [equalMinors, bothMajorsLongSpades, jumpBid]
                      ]
