module Bids.OneOverOne(
    b1C1D
  , b1C1H
  , b1C1S
  , b1C1N
  , b1D1H
  , b1D1S
  , b1D1N
  , b1H1S
  -- We skip responding 1N over a major: not sure if we're SAYC or 2/1
) where

import Action(Action)
import EDSL(suitLength, minSuitLength, maxSuitLength, makeCall, impliesThat,
            pointRange, balancedHand, forbid, atLeastAsLong, forEach,
            equalLength, impliesThat, nameAction)
import qualified Terminology as T


-- TODO: Should we default to Walsh Style or not?
b1C1D :: Action
b1C1D = nameAction "sayc_b1C1D" $ do
    forEach T.majorSuits (T.Diamonds `atLeastAsLong`)
    minSuitLength T.Diamonds 4
    pointRange 6 40
    forbid b1C1N
    makeCall $ T.Bid 1 T.Diamonds


-- TODO: what should you bid with a 4-card major and 6-card minor?

respondWithMajor_ :: T.Suit -> Action
respondWithMajor_ major = do
    minSuitLength major 4
    major `atLeastAsLong` (T.otherMajor major)
    pointRange 6 40
    makeCall $ T.Bid 1 major

b1C1H :: Action
b1C1H = nameAction "sayc_b1C1H" $ do
    -- With 4-4 in the majors, start with 1H.
    T.Hearts `equalLength` T.Spades `impliesThat` suitLength T.Hearts 4
    respondWithMajor_ T.Hearts

b1C1S :: Action
b1C1S = nameAction "sayc_b1C1S" $ do
    -- With at least 5-5 in the majors, start with 1S.
    T.Spades `equalLength` T.Hearts `impliesThat` minSuitLength T.Spades 5
    respondWithMajor_ T.Spades

b1D1H :: Action
b1D1H = nameAction "sayc_b1D1H" $ do
    -- With 4-4 in the majors, start with 1H.
    T.Hearts `equalLength` T.Spades `impliesThat` suitLength T.Hearts 4
    respondWithMajor_ T.Hearts

b1D1S :: Action
b1D1S = nameAction "sayc_b1D1S" $ do
    -- With at least 5-5 in the majors, start with 1S.
    T.Spades `equalLength` T.Hearts `impliesThat` minSuitLength T.Spades 5
    respondWithMajor_ T.Spades


respond1N_ :: Action
respond1N_ = do
    pointRange 6 10
    balancedHand
    forEach T.majorSuits (`maxSuitLength` 3)
    makeCall $ T.Bid 1 T.Notrump

b1C1N :: Action
b1C1N = nameAction "sayc_b1C1N" $ respond1N_

b1D1N :: Action
b1D1N = nameAction "sayc_b1D1N" $ respond1N_


b1H1S :: Action
b1H1S = nameAction "sayc_b1H1S" $ do
    minSuitLength T.Spades 4
    -- If you could have raised hearts but chose not to, you should have some
    -- extra strength
    minSuitLength T.Hearts 3 `impliesThat` pointRange 10 40
    makeCall $ T.Bid 1 T.Spades
