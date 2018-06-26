module PrecisionOpeners(topic) where

import Data.List.Utils(join)

import Output(output, Punct(NDash))
import Topic(Topic(..), base, (<~), wrap, Situations)
import Auction(forbid, makeCall, makePass, pointRange, suitLength,
               minSuitLength, maxSuitLength, Action, balancedHand, withholdBid,
               constrain)
import Situation(situation, Situation)
import qualified Terminology as T
import qualified CommonBids as B


oneClubOpener :: Action
oneClubOpener = pointRange 16 40


oneMajorOpener :: T.Suit -> Action
oneMajorOpener suit = do
    forbid oneClubOpener
    minSuitLength suit 5


twoClubOpener6 :: Action
twoClubOpener6 = do
    forbid oneClubOpener
    forbid (oneMajorOpener T.Hearts)
    forbid (oneMajorOpener T.Spades)
    minSuitLength T.Clubs 6


twoDiamondOpener :: Action
twoDiamondOpener = do
    forbid oneClubOpener
    constrain "two_diamond_opener" ["shape(", ", 4414 + 4405)"]


twoClubOpener54 :: T.Suit -> Action  -- The suit is the 4-card major
twoClubOpener54 suit = do
    sequence_ . map forbid $ [ oneClubOpener
                             , oneMajorOpener T.Hearts
                             , oneMajorOpener T.Spades
                             , twoDiamondOpener
                             ]
    minSuitLength T.Clubs 5
    suitLength suit 4


oneNotrumpOpener :: Action
oneNotrumpOpener = do
    pointRange 13 15
    balancedHand
    forbid (minSuitLength T.Spades 5)
    forbid (minSuitLength T.Hearts 5)


oneDiamondOpener :: Action
oneDiamondOpener = do
    sequence_ . map forbid $ [ oneClubOpener
                             , oneMajorOpener T.Hearts
                             , oneMajorOpener T.Spades
                             , twoClubOpener6
                             , twoClubOpener54 T.Hearts
                             , twoClubOpener54 T.Spades
                             , oneNotrumpOpener
                             ]
    minSuitLength T.Diamonds 2



oneClub :: Situations
oneClub = let
    sit dealer vul = let
        action = do
            B.setDealerAndOpener dealer T.South
            oneClubOpener
        explanation fmt =
            "With 16 or more points, open a strong " ++
            output fmt (T.Bid 1 T.Clubs) ++ ". This is the hallmark of the\
          \ Precision Club system."
      in
        situation dealer vul action (T.Bid 1 T.Clubs) explanation
  in
    wrap $ base sit <~ T.allDirections <~ T.allVulnerabilities


oneDiamond :: Situations
oneDiamond = let
    sit dealer vul = let
        action = do
            B.setDealerAndOpener dealer T.South
            oneDiamondOpener
        explanation fmt =
            "With opening strength but not enough for a strong " ++
            output fmt (T.Bid 1 T.Clubs) ++ " and no 5-card major or 6-card\
          \ club suit, open a diamond. Partner will announce that it ``could\
          \ be short.''"
      in
        situation dealer vul action (T.Bid 1 T.Diamonds) explanation
  in
    wrap $ base sit <~ T.allDirections <~ T.allVulnerabilities


oneMajor :: Situations
oneMajor = let
    sit dealer vul suit = let
        action = do
            B.setDealerAndOpener dealer T.South
            oneMajorOpener suit
            -- TODO: What if you're 5-5?
        explanation fmt =
            "With opening strength but not enough for a strong " ++
            output fmt (T.Bid 1 T.Clubs) ++ " open a 5-card major suit."
      in
        situation dealer vul action (T.Bid 1 suit) explanation
  in
    wrap $ base sit <~ T.allDirections <~ T.allVulnerabilities <~ T.majorSuits


twoClubs6 :: Situations
twoClubs6 = let
    sit dealer vul = let
        action = do
            B.setDealerAndOpener dealer T.South
            pointRange 10 15
            minSuitLength T.Clubs 6
            maxSuitLength T.Spades 4
            maxSuitLength T.Hearts 4
        explanation fmt =
            "With opening strength but not enough for a strong " ++
            output fmt (T.Bid 1 T.Clubs) ++ " open a 6-card club suit at the\
          \ two level."
      in
        situation dealer vul action (T.Bid 2 T.Clubs) explanation
  in
    wrap $ base sit <~ T.allDirections <~ T.allVulnerabilities


topic :: Topic
topic = Topic "Precision opening bids" situations
  where
    situations = wrap [ oneClub
                      , oneDiamond
                      , oneMajor
                      ]
