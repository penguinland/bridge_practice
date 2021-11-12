module Topics.StandardModernPrecision.MafiaResponses(topic) where

--import Output(output)
import Topic(Topic(..), wrap, Situations)
import Auction(withholdBid, Action)--, forbid, minSuitLength, suitLength,
--               {-balancedHand, pointRange, SuitLengthComparator(..), compareSuitLength-})
import Situation(Situation, situation, base, (<~))
import qualified Terminology as T
import qualified Topics.StandardModernPrecision.Bids as B


minimumSupport :: Situations
minimumSupport = let
    -- The type signature is to convince the compiler that we're not throwing
    -- away values from openerBid
    sit :: (Action, Action, T.Suit) -> T.Vulnerability -> T.Direction -> Situation
    sit (openerBid, responderBid, suit) = let
        action = do
            B.startOfMafia
            openerBid
            B.oppsPass
            withholdBid responderBid
        explanation _ =
            "With 4-card support and a minimum hand, raise partner's major.\
           \ You are neither strong enough nor shapely enough to invite to\
           \ game."
      in
        situation "2M" action (T.Bid 2 suit) explanation
  in
    B.smpWrapN $ base sit <~ [ (B.b1C1D1H, B.b1C1D1H2H, T.Hearts)
                             , (B.b1C1D1S, B.b1C1D1S2S, T.Spades) ]

{-
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
        situation "2MS" action (T.Bid 1 T.Spades) explanation
  in
    B.smpWrapS $ base sit


jumpBid :: Situations
jumpBid = let
    sit (rawBid, bid) = let
        action = do
            B.startOfMafia
            withholdBid bid
        explanation fmt =
            "With an unbalanced, single-suited hand that is strong enough to\
           \ force to game, jump in your suit. Responder can treat this like\
           \ the 2/1 sequence " ++
            output fmt (T.Bid 2 T.Clubs) ++ "--" ++
            output fmt (T.Bid 2 T.Diamonds) ++ "--" ++
            output fmt rawBid ++ "."
      in
        situation "J1" action rawBid explanation
  in
    B.smpWrapS $ base sit <~ [ (T.Bid 2 T.Hearts,   B.b1C1D2H)
                             , (T.Bid 2 T.Spades,   B.b1C1D2S)
                             , (T.Bid 3 T.Clubs,    B.b1C1D3C)
                             , (T.Bid 3 T.Diamonds, B.b1C1D3D)]
-}


topic :: Topic
topic = Topic "MaFiA responses" "MafResp" situations
  where
    situations = wrap [ minimumSupport
{-
                      , wrap [brakesHearts, brakesSpades]
                      , wrap [otherMajorHearts, otherMajorSpades]
                      , threeCardSupport
                      , maxNoMajors
-}
                      -- TODO: jump responses, splinters, etc.
                      ]
