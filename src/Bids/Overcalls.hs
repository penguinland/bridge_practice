module Bids.Overcalls(
    b1C  -- re-exported from StandardOpenings
  , b1Co1D
  , b1Co1H
  , b1Co1S
  , b1Co1N
--  , b1Co2D
--  , b1Co2H
--  , b1Co2S
--  , b1Co3D
--  , b1Co3H
--  , b1Co3S
--  , b1Co4D
--  , b1Co4H
--  , b1Co4S
  , b1D  -- re-exported from StandardOpenings
  , b1Do1H
  , b1Do1S
  , b1Do1N
  , b1Do2C
--  , b1Do2H
--  , b1Do2S
--  , b1Do3C
--  , b1Do3H
--  , b1Do3S
--  , b1Do4C
--  , b1Do4H
--  , b1Do4S
  , b1H  -- re-exported from StandardOpenings
  , b1Ho1S
  , b1Ho1N
  , b1Ho2C
  , b1Ho2D
--  , b1Ho2S
--  , b1Ho3C
--  , b1Ho3D
--  , b1Ho3S
--  , b1Ho4C
--  , b1Ho4D
--  , b1Ho4S
  , b1S  -- re-exported from StandardOpenings
  , b1So1N
  , b1So2C
  , b1So2D
  , b1So2H
--  , b1So3C
--  , b1So3D
--  , b1So3H
--  , b1So4C
--  , b1So4D
--  , b1So4H
) where


import Action(Action)
import Bids.StandardOpenings(b1C, b1D, b1H, b1S)
import EDSL(makeCall, minSuitLength, maxSuitLength, longerThan, atLeastAsLong,
            forEach, nameAction, pointRange, soundHolding, balancedHand)
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


overcall1N_ :: T.Suit -> Action
overcall1N_ suit = do
    balancedHand
    pointRange 16 18
    soundHolding suit
    forEach (T.otherSuits suit) (`maxSuitLength` 4)
    makeCall $ T.Bid 1 T.Notrump


b1Co1D :: Action
b1Co1D = nameAction "b1Co1D" $ do
    T.Diamonds `longerThan` T.Hearts
    T.Diamonds `longerThan` T.Spades
    oneLevelOvercall_ T.Diamonds


b1Co1H :: Action
b1Co1H = nameAction "b1Co1H" $ do
    T.Hearts `longerThan` T.Spades
    oneLevelOvercall_ T.Hearts


b1Co1S :: Action
b1Co1S = nameAction "b1Co1S" $ do
    oneLevelOvercall_ T.Spades


b1Do1H :: Action
b1Do1H = nameAction "b1Do1H" $ do
    T.Hearts `longerThan` T.Spades
    oneLevelOvercall_ T.Hearts


b1Do1S :: Action
b1Do1S = nameAction "b1Do1S" $ do
    oneLevelOvercall_ T.Spades


b1Do2C :: Action
b1Do2C = nameAction "b1Do2C" $ do
    T.Clubs `longerThan` T.Hearts
    T.Clubs `longerThan` T.Spades
    twoLevelOvercall_ T.Clubs


b1Ho1S :: Action
b1Ho1S = nameAction "b1Ho1S" $ do
    oneLevelOvercall_ T.Spades


b1Ho2C :: Action
b1Ho2C = nameAction "b1Ho2C" $ do
    T.Clubs `longerThan` T.Spades
    twoLevelOvercall_ T.Clubs


b1Ho2D :: Action
b1Ho2D = nameAction "b1Ho2D" $ do
    twoLevelOvercall_ T.Diamonds


b1So2C :: Action
b1So2C = nameAction "b1So2C" $ do
    twoLevelOvercall_ T.Clubs


b1So2D :: Action
b1So2D = nameAction "b1So2D" $ do
    twoLevelOvercall_ T.Diamonds


b1So2H :: Action
b1So2H = nameAction "b1So2H" $ do
    twoLevelOvercall_ T.Spades


b1Co1N :: Action
b1Co1N = nameAction "b1Co1N" $ do
    overcall1N_ T.Clubs


b1Do1N :: Action
b1Do1N = nameAction "b1Do1N" $ do
    overcall1N_ T.Diamonds


b1Ho1N :: Action
b1Ho1N = nameAction "b1Ho1N" $ do
    overcall1N_ T.Hearts


b1So1N :: Action
b1So1N = nameAction "b1So1N" $ do
    overcall1N_ T.Spades
