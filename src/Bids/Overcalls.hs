module Bids.Overcalls(
    b1C  -- re-exported from StandardOpenings
  , b1C1D
  , b1C1H
  , b1C1S
--  , b1C1N
--  , b1C2D
--  , b1C2H
--  , b1C2S
--  , b1C3D
--  , b1C3H
--  , b1C3S
--  , b1C4D
--  , b1C4H
--  , b1C4S
  , b1D  -- re-exported from StandardOpenings
  , b1D1H
  , b1D1S
--  , b1D1N
  , b1D2C
--  , b1D2H
--  , b1D2S
--  , b1D3C
--  , b1D3H
--  , b1D3S
--  , b1D4C
--  , b1D4H
--  , b1D4S
  , b1H  -- re-exported from StandardOpenings
  , b1H1S
--  , b1H1N
  , b1H2C
  , b1H2D
--  , b1H2S
--  , b1H3C
--  , b1H3D
--  , b1H3S
--  , b1H4C
--  , b1H4D
--  , b1H4S
  , b1S  -- re-exported from StandardOpenings
--  , b1S1N
  , b1S2C
  , b1S2D
  , b1S2H
--  , b1S3C
--  , b1S3D
--  , b1S3H
--  , b1S4C
--  , b1S4D
--  , b1S4H
) where


import Action(Action)
import Bids.StandardOpenings(b1C, b1D, b1H, b1S)
import EDSL(makeCall, forEach, minSuitLength, longerThan, atLeastAsLong,
            nameAction, pointRange, soundHolding)
import qualified Terminology as T


haveOvercall_ :: T.Suit -> Action
haveOvercall_ suit = do
    minSuitLength suit 5
    soundHolding suit
    forEach T.allSuits (suit `atLeastAsLong`)


oneLevelOvercall_ :: T.Suit -> Action
oneLevelOvercall_ suit = do
    haveOvercall_ suit
    pointRange 8 16
    makeCall $ T.Bid 1 suit


twoLevelOvercall_ :: T.Suit -> Action
twoLevelOvercall_ suit = do
    haveOvercall_ suit
    pointRange 11 16
    makeCall $ T.Bid 2 suit


b1C1D :: Action
b1C1D = nameAction "b1C1D" $ do
    T.Diamonds `longerThan` T.Hearts
    T.Diamonds `longerThan` T.Spades
    oneLevelOvercall_ T.Diamonds


b1C1H :: Action
b1C1H = nameAction "b1C1H" $ do
    T.Hearts `longerThan` T.Spades
    oneLevelOvercall_ T.Hearts


b1C1S :: Action
b1C1S = nameAction "b1C1S" $ do
    oneLevelOvercall_ T.Spades


b1D1H :: Action
b1D1H = nameAction "b1D1H" $ do
    T.Hearts `longerThan` T.Spades
    oneLevelOvercall_ T.Hearts


b1D1S :: Action
b1D1S = nameAction "b1D1S" $ do
    oneLevelOvercall_ T.Spades


b1D2C :: Action
b1D2C = nameAction "b1D2C" $ do
    T.Clubs `longerThan` T.Hearts
    T.Clubs `longerThan` T.Spades
    twoLevelOvercall_ T.Clubs


b1H1S :: Action
b1H1S = nameAction "b1H1S" $ do
    oneLevelOvercall_ T.Spades


b1H2C :: Action
b1H2C = nameAction "b1H2C" $ do
    T.Clubs `longerThan` T.Spades
    twoLevelOvercall_ T.Clubs


b1H2D :: Action
b1H2D = nameAction "b1H2D" $ do
    twoLevelOvercall_ T.Diamonds


b1S2C :: Action
b1S2C = nameAction "b1S2C" $ do
    twoLevelOvercall_ T.Clubs


b1S2D :: Action
b1S2D = nameAction "b1S2D" $ do
    twoLevelOvercall_ T.Diamonds


b1S2H :: Action
b1S2H = nameAction "b1S2H" $ do
    twoLevelOvercall_ T.Spades
