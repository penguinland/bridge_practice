module Bids.Overcalls(
    b1C  -- re-exported from StandardOpenings
  , b1CoP
  , b1Co1D
  , b1Co1H
  , b1Co1S
  , b1Co1N
  , b1Co2D
  , b1Co2H
  , b1Co2S
  , b1Co3D
  , b1Co3H
  , b1Co3S
  , b1Co4D
  , b1Co4H
  , b1Co4S
  , b1D  -- re-exported from StandardOpenings
  , b1DoP
  , b1Do1H
  , b1Do1S
  , b1Do1N
  , b1Do2C
  , b1Do2H
  , b1Do2S
  , b1Do3C
  , b1Do3H
  , b1Do3S
  , b1Do4C
  , b1Do4H
  , b1Do4S
  , b1H  -- re-exported from StandardOpenings
  , b1HoP
  , b1Ho1S
  , b1Ho1N
  , b1Ho2C
  , b1Ho2D
  , b1Ho2S
  , b1Ho3C
  , b1Ho3D
  , b1Ho3S
  , b1Ho4C
  , b1Ho4D
  , b1Ho4S
  , b1S  -- re-exported from StandardOpenings
  , b1SoP
  , b1So1N
  , b1So2C
  , b1So2D
  , b1So2H
  , b1So3C
  , b1So3D
  , b1So3H
  , b1So4C
  , b1So4D
  , b1So4H
) where


import Action(Action)
import Bids.StandardOpenings(b1C, b1D, b1H, b1S)
import EDSL(makeCall, suitLength, minSuitLength, maxSuitLength, nameAction,
            forEach, longerThan, atLeastAsLong, pointRange, soundHolding,
            balancedHand, forbidAll)
import qualified Terminology as T


haveOvercall_ :: T.Suit -> Action
haveOvercall_ suit = do
    minSuitLength suit 5
    forbidAll $ map ($ suit) [weak2, preempt3, preempt4]
    soundHolding suit
    forEach T.allSuits (suit `atLeastAsLong`)


oneLevelOvercall_ :: T.Suit -> Action
oneLevelOvercall_ suit = do
    haveOvercall_ suit
    -- With too much strength, start with a power double.
    pointRange 8 16
    makeCall $ T.Bid 1 suit


twoLevelOvercall_ :: T.Suit -> Action
twoLevelOvercall_ suit = do
    haveOvercall_ suit
    -- With too much strength, start with a power double.
    pointRange 11 16
    makeCall $ T.Bid 2 suit


weak2 :: T.Suit -> Action
weak2 suit = nameAction ("weak2_overcall_" ++ show suit) $ do
    suitLength suit 6
    pointRange 5 11
    forEach (T.otherSuits suit) (suit `longerThan`)
    makeCall $ T.Bid 2 suit


preempt3 :: T.Suit -> Action
preempt3 suit = nameAction ("preempt3_overcall_" ++ show suit) $ do
    suitLength suit 7
    pointRange 5 9
    makeCall $ T.Bid 3 suit


preempt4 :: T.Suit -> Action
preempt4 suit = nameAction ("preempt4_overcall_" ++ show suit) $ do
    minSuitLength suit 8
    pointRange 5 13  -- TODO: figure out the correct point range
    makeCall $ T.Bid 4 suit


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


-- Overcalling 1N


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


-- Overcalling a weak 2


b1Co2D :: Action
b1Co2D = nameAction "b1Co2D" $ do
    weak2 T.Diamonds


b1Co2H :: Action
b1Co2H = nameAction "b1Co2H" $ do
    weak2 T.Hearts


b1Co2S :: Action
b1Co2S = nameAction "b1Co2S" $ do
    weak2 T.Spades


b1Do2H :: Action
b1Do2H = nameAction "b1Do2H" $ do
    weak2 T.Hearts


b1Do2S :: Action
b1Do2S = nameAction "b1Do2S" $ do
    weak2 T.Spades


b1Ho2S :: Action
b1Ho2S = nameAction "b1Ho2S" $ do
    weak2 T.Spades


-- Preempting at the 3 level


b1Co3D :: Action
b1Co3D = nameAction "b1Co3D" $ do
    preempt3 T.Diamonds


b1Co3H :: Action
b1Co3H = nameAction "b1Co3H" $ do
    preempt3 T.Hearts


b1Co3S :: Action
b1Co3S = nameAction "b1Co3S" $ do
    preempt3 T.Spades


b1Do3C :: Action
b1Do3C = nameAction "b1Do3C" $ do
    preempt3 T.Clubs


b1Do3H :: Action
b1Do3H = nameAction "b1Do3H" $ do
    preempt3 T.Hearts


b1Do3S :: Action
b1Do3S = nameAction "b1Do3S" $ do
    preempt3 T.Spades


b1Ho3C :: Action
b1Ho3C = nameAction "b1Ho3C" $ do
    preempt3 T.Clubs


b1Ho3D :: Action
b1Ho3D = nameAction "b1Ho3D" $ do
    preempt3 T.Diamonds


b1Ho3S :: Action
b1Ho3S = nameAction "b1Ho3S" $ do
    preempt3 T.Spades


b1So3C :: Action
b1So3C = nameAction "b1So3C" $ do
    preempt3 T.Clubs


b1So3D :: Action
b1So3D = nameAction "b1So3D" $ do
    preempt3 T.Diamonds


b1So3H :: Action
b1So3H = nameAction "b1So3H" $ do
    preempt3 T.Hearts


-- Preempting at the 4 level


b1Co4D :: Action
b1Co4D = nameAction "b1Co4D" $ do
    preempt4 T.Diamonds


b1Co4H :: Action
b1Co4H = nameAction "b1Co4H" $ do
    preempt4 T.Hearts


b1Co4S :: Action
b1Co4S = nameAction "b1Co4S" $ do
    preempt4 T.Spades


b1Do4C :: Action
b1Do4C = nameAction "b1Do4C" $ do
    preempt4 T.Clubs


b1Do4H :: Action
b1Do4H = nameAction "b1Do4H" $ do
    preempt4 T.Hearts


b1Do4S :: Action
b1Do4S = nameAction "b1Do4S" $ do
    preempt4 T.Spades


b1Ho4C :: Action
b1Ho4C = nameAction "b1Ho4C" $ do
    preempt4 T.Clubs


b1Ho4D :: Action
b1Ho4D = nameAction "b1Ho4D" $ do
    preempt4 T.Diamonds


b1Ho4S :: Action
b1Ho4S = nameAction "b1Ho4S" $ do
    preempt4 T.Spades


b1So4C :: Action
b1So4C = nameAction "b1So4C" $ do
    preempt4 T.Clubs


b1So4D :: Action
b1So4D = nameAction "b1So4D" $ do
    preempt4 T.Diamonds


b1So4H :: Action
b1So4H = nameAction "b1So4H" $ do
    preempt4 T.Hearts


-- Define passes as the inability to make any other bid.


b1CoP :: Action
b1CoP = nameAction "b1CoP" $ do
    forbidAll [ b1Co1D, b1Co2D, b1Co3D, b1Co4D
              , b1Co1H, b1Co2H, b1Co3H, b1Co4H
              , b1Co1S, b1Co2S, b1Co3S, b1Co4S
              , b1Co1N
              ]
    makeCall T.Pass


b1DoP :: Action
b1DoP = nameAction "b1DoP" $ do
    forbidAll [         b1Do2C, b1Do3C, b1Do4C
              , b1Do1H, b1Do2H, b1Do3H, b1Do4H
              , b1Do1S, b1Do2S, b1Do3S, b1Do4S
              , b1Do1N
              ]
    makeCall T.Pass


b1HoP :: Action
b1HoP = nameAction "b1HoP" $ do
    forbidAll [         b1Ho2C, b1Ho3C, b1Ho4C
              ,         b1Ho2D, b1Ho3D, b1Ho4D
              , b1Ho1S, b1Ho2S, b1Ho3S, b1Ho4S
              , b1Co1N
              ]
    makeCall T.Pass


b1SoP :: Action
b1SoP = nameAction "b1SoP" $ do
    forbidAll [         b1So2C, b1So3C, b1So4C
              ,         b1So2D, b1So3D, b1So4D
              ,         b1So2H, b1So3H, b1So4H
              , b1So1N
              ]
    makeCall T.Pass
