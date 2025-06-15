module Bids.StandardModernPrecision.OneClubOneHeartAlt(
  -- Copied from earlier auctions
    b1C
  , b1C1H  -- An alias for b1C1Hnos!
  , b1C1H1S
  , b1C1H1N
  , b1C1H2C
  , b1C1H2D
  , b1C1H2H
  , b1C1H2S
  -- New bids start here
  , b1C1H1S1N  -- Responses when opener rebids 1S
  , b1C1H1S2C
  , b1C1H1S2D
  , b1C1H1S2H
  , b1C1H1S2S
  , b1C1H1S2N

  , b1C1H2C2D  -- Responses when opener rebids 2C
  , b1C1H2C2H
  , b1C1H2C2S
  , b1C1H2C2N
  , b1C1H2C3C
  , b1C1H2C3D

  , b1C1H2D2H  -- Responses when opener rebids 2D
  , b1C1H2D2S
  , b1C1H2D2N
  , b1C1H2D3C
  , b1C1H2D3D
  , b1C1H2D3H

  , b1C1H2H2S  -- Responses when opener rebids 2H
  , b1C1H2H2N
  , b1C1H2H3C
  , b1C1H2H3D
  , b1C1H2H3H
  , b1C1H2H3S
) where

import Action(Action)
import Bids.StandardModernPrecision.OneClub(
    b1C, b1C1Hnos, b1C1H1S, b1C1H1N, b1C1H2C, b1C1H2D, b1C1H2H, b1C1H2S,
    tripleFourOneShape)
import EDSL(forbid, suitLength, minSuitLength, maxSuitLength, balancedHand,
            makeCall, makeAlertableCall, alternatives, longerThan,
            atLeastAsLong, forEach, forbidAll, nameAction)
import qualified Terminology as T


-- Alias the bid properly to re-export from here.
b1C1H :: Action
b1C1H = b1C1Hnos


b1C1H1S1N :: Action
b1C1H1S1N = nameAction "smp_b1C1H1S1N" $ do
    balancedHand
    forbid b1C1H1S2S
    forEach T.allSuits (`maxSuitLength` 4)
    makeCall (T.Bid 1 T.Notrump)


b1C1H1S2C :: Action
b1C1H1S2C = nameAction "smp_b1C1H1S2C" $ do
    forbid b1C1H1S2S
    minSuitLength T.Clubs 5
    T.Clubs `longerThan` T.Diamonds
    T.Clubs `longerThan` T.Hearts
    makeCall (T.Bid 2 T.Clubs)


b1C1H1S2D :: Action
b1C1H1S2D = nameAction "smp_b1C1H1S2D" $ do
    forbid b1C1H1S2S
    minSuitLength T.Diamonds 5
    T.Diamonds `longerThan` T.Hearts
    T.Diamonds `atLeastAsLong` T.Clubs
    makeCall (T.Bid 2 T.Diamonds)


b1C1H1S2H :: Action
b1C1H1S2H = nameAction "smp_b1C1H1S2H" $ do
    forbid b1C1H1S2S
    minSuitLength T.Hearts 5
    T.Hearts `atLeastAsLong` T.Clubs
    T.Hearts `atLeastAsLong` T.Diamonds
    makeCall (T.Bid 2 T.Hearts)


b1C1H1S2S :: Action
b1C1H1S2S = nameAction "smp_b1C1H1S2S" $ do
    minSuitLength T.Spades 3
    makeCall (T.Bid 2 T.Spades)


b1C1H1S2N :: Action
b1C1H1S2N = nameAction "smp_b1C1H1S2N" $ do
    tripleFourOneShape
    suitLength T.Spades 1
    makeAlertableCall (T.Bid 2 T.Notrump) "1444 shape, singleton spade"


b1C1H2C2D :: Action
b1C1H2C2D = nameAction "smp_b1C1H2C2D" $ do
    alternatives . map (`suitLength` 4) $ T.majorSuits
    balancedHand
    makeAlertableCall (T.Bid 2 T.Diamonds) "waiting bid"


b1C1H2C2H :: Action
b1C1H2C2H = nameAction "smp_b1C1H2C2H" $ do
    minSuitLength T.Hearts 5
    T.Hearts `atLeastAsLong` T.Diamonds
    T.Hearts `longerThan` T.Clubs
    -- TODO: Do we need extra restrictions here on not having a club fit, or
    -- having longer hearts than diamonds or anything?
    makeCall (T.Bid 2 T.Hearts)


b1C1H2C2S :: Action
b1C1H2C2S = nameAction "smp_b1C1H2C2S" $ do
    forbid b1C1H2C3C
    forbid b1C1H2C2H
    minSuitLength T.Diamonds 5
    makeAlertableCall (T.Bid 2 T.Diamonds) "5+ diamonds"


b1C1H2C2N :: Action
b1C1H2C2N = nameAction "smp_b1C1H2C2N" $ do
    maxSuitLength T.Clubs 3  -- TODO: should the max club length be only 2?
    forEach T.allSuits (`maxSuitLength` 4)
    balancedHand
    forbid b1C1H2C2D
    makeCall (T.Bid 2 T.Notrump)


b1C1H2C3C :: Action
b1C1H2C3C = nameAction "smp_b1C1H2C3C" $ do
    forbidAll [balancedHand, b1C1H2C2D, b1C1H2C2H, b1C1H2C2N]
    minSuitLength T.Clubs 3
    makeCall (T.Bid 3 T.Clubs)


b1C1H2C3D :: Action
b1C1H2C3D = nameAction "smp_b1C1H2C3D" $ do
    tripleFourOneShape
    suitLength T.Clubs 1
    makeAlertableCall (T.Bid 3 T.Diamonds) "4441 shape, singleton club"


b1C1H2D2H :: Action
b1C1H2D2H = nameAction "smp_b1C1H2D2H" $ do
    minSuitLength T.Hearts 5
    maxSuitLength T.Diamonds 4
    T.Hearts `atLeastAsLong` T.Clubs
    makeCall (T.Bid 2 T.Hearts)


b1C1H2D2S :: Action
b1C1H2D2S = nameAction "smp_b1C1H2D2S" $ do
    alternatives . map (`suitLength` 4) $ T.majorSuits
    forbid b1C1H2D2H
    balancedHand
    makeAlertableCall (T.Bid 2 T.Spades) "waiting bid"


b1C1H2D2N :: Action
b1C1H2D2N = nameAction "smp_b1C1H2D2N" $ do
    maxSuitLength T.Diamonds 3  -- TODO: is the max Diamonds length actually 2?
    forEach T.allSuits (`maxSuitLength` 4)
    balancedHand
    forbid b1C1H2D2S
    makeCall (T.Bid 2 T.Notrump)


b1C1H2D3C :: Action
b1C1H2D3C = nameAction "smp_b1C1H2D3C" $ do
    minSuitLength T.Clubs 5
    forbid b1C1H2D2S
    forbid b1C1H2D3D
    makeCall (T.Bid 3 T.Clubs)


b1C1H2D3D :: Action
b1C1H2D3D = nameAction "smp_b1C1H2D3D" $ do
    forbidAll [balancedHand, b1C1H2D2H, b1C1H2D2S, b1C1H2D2N]
    minSuitLength T.Diamonds 3
    makeCall (T.Bid 3 T.Diamonds)


b1C1H2D3H :: Action
b1C1H2D3H = nameAction "smp_b1C1H2D3H" $ do
    tripleFourOneShape
    suitLength T.Diamonds 1
    makeAlertableCall (T.Bid 3 T.Hearts) "4414 shape, singleton diamond"


b1C1H2H2S :: Action
b1C1H2H2S = nameAction "smp_b1C1H2H2S" $ do
    -- TODO: we really want to show hands where either we have 4 spades *or* we
    -- think partner should declare in notrump. Is there an easy way to code
    -- that latter condition?
    suitLength T.Spades 4
    forbid b1C1H2H3H
    balancedHand
    makeAlertableCall (T.Bid 2 T.Spades) "waiting bid"


b1C1H2H2N :: Action
b1C1H2H2N = nameAction "smp_b1C1H2H2N" $ do
    forEach T.allSuits (`maxSuitLength` 4)
    balancedHand
    forbidAll [b1C1H2H2S, b1C1H2H3H]
    makeCall (T.Bid 2 T.Notrump)


b1C1H2H3C :: Action
b1C1H2H3C = nameAction "smp_b1C1H2H3C" $ do
    forbidAll [b1C1H2H2S, b1C1H2H2N, b1C1H2H3H]
    minSuitLength T.Clubs 5
    T.Clubs `longerThan` T.Diamonds
    makeCall (T.Bid 3 T.Clubs)


b1C1H2H3D :: Action
b1C1H2H3D = nameAction "smp_b1C1H2H3D" $ do
    forbidAll [b1C1H2H2S, b1C1H2H2N, b1C1H2H3H]
    minSuitLength T.Diamonds 5
    T.Diamonds `atLeastAsLong` T.Clubs
    makeCall (T.Bid 3 T.Diamonds)


b1C1H2H3H :: Action
b1C1H2H3H = nameAction "smp_b1C1H2H3H" $ do
    minSuitLength T.Hearts 3
    makeCall (T.Bid 3 T.Hearts)


b1C1H2H3S :: Action
b1C1H2H3S = nameAction "smp_b1C1H2H3S" $ do
    tripleFourOneShape
    suitLength T.Hearts 1
    makeAlertableCall (T.Bid 3 T.Spades) "4144 shape, singleton heart"
