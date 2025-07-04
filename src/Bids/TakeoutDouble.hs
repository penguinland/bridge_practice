module Bids.TakeoutDouble(
    powerDouble
  , b1C  -- Re-exported from StandardOpenings
  , b1CoX
  , b1CoXo1D
  , b1CoXo1H
  , b1CoXo1S
  , b1CoXo1N
  , b1D  -- Re-exported from StandardOpenings
  , b1DoX
  , b1DoXo1H
  , b1DoXo1S
  , b1DoXo1N
  , b1DoXo2C
  , b1H  -- Re-exported from StandardOpenings
  , b1HoX
  , b1HoXo1S
  , b1HoXo1N
  , b1HoXo2C
  , b1HoXo2D
  , b1S  -- Re-exported from StandardOpenings
  , b1SoX
  , b1SoXo1N
  , b1SoXo2C
  , b1SoXo2D
  , b1SoXo2H
) where

-- TODO: include jumps to show stronger hands.

import Control.Monad(when)

import Action(Action)
import Bids.StandardOpenings(b1C, b1D, b1H, b1S)
import EDSL(minSuitLength, maxSuitLength, makeCall, pointRange, soundHolding, 
            forEach, nameAction, alternatives, strongerThan, hasStopper,
            balancedHand, longerThan)
import qualified Terminology as T


powerDouble :: T.Suit -> Action
powerDouble oppsSuit = nameAction ("power_double_" ++ show oppsSuit) $ do
    pointRange 18 40
    alternatives
        [ pointRange 19 40 >> balancedHand
        , alternatives $ map (`minSuitLength` 6) (T.otherSuits oppsSuit)
        ]
    makeCall T.Double


takeoutDouble :: String -> T.Suit -> Action
takeoutDouble name oppsSuit = nameAction name $ do
    alternatives [powerDouble oppsSuit, takeoutDouble']
    makeCall T.Double
  where
    otherSuits = T.otherSuits oppsSuit
    takeoutDouble' = do
        pointRange 11 40
        forEach otherSuits (`minSuitLength` 3)
        forEach otherSuits (`maxSuitLength` 5)
        forEach T.majorSuits (`maxSuitLength` 4)
        maxSuitLength oppsSuit 3
        alternatives [maxSuitLength oppsSuit 2, pointRange 14 40]

b1CoX :: Action
b1CoX = takeoutDouble "b1CoX" T.Clubs

b1DoX :: Action
b1DoX = takeoutDouble "b1DoX" T.Diamonds

b1HoX :: Action
b1HoX = takeoutDouble "b1HoX" T.Hearts

b1SoX :: Action
b1SoX = takeoutDouble "b1SoX" T.Spades


minimumResponse_ :: T.Suit -> T.Suit -> Int -> Action
minimumResponse_ oppsSuit ourSuit level = do
    forEach T.allSuits (ourSuit `strongerThan`)
    pointRange 0 10
    when (ourSuit > oppsSuit) (pointRange 0 7)
    makeCall $ T.Bid level ourSuit

b1CoXo1D :: Action
b1CoXo1D = nameAction "b1CoXo1D" $ do
    -- If you were 5-5 in diamonds and a major, you'd be sorely tempted to bid
    -- the major, I think. Same if you were 4-4.
    T.Diamonds `longerThan` T.Hearts
    T.Diamonds `longerThan` T.Spades
    minimumResponse_ T.Clubs T.Diamonds 1

b1CoXo1H :: Action
b1CoXo1H = nameAction "b1CoXo1H" $ do
    minimumResponse_ T.Clubs T.Hearts 1

b1CoXo1S :: Action
b1CoXo1S = nameAction "b1CoXo1S" $ do
    minimumResponse_ T.Clubs T.Spades 1

b1DoXo1H :: Action
b1DoXo1H = nameAction "b1DoXo1H" $ do
    minimumResponse_ T.Diamonds T.Hearts 1

b1DoXo1S :: Action
b1DoXo1S = nameAction "b1DoXo1S" $ do
    minimumResponse_ T.Diamonds T.Spades 1

b1DoXo2C :: Action
b1DoXo2C = nameAction "b1DoXo2C" $ do
    -- If you were 5-5 in clubs and a major, definitely bid the major, even if
    -- the clubs were slightly stronger. Same if you were 4-4.
    T.Clubs `longerThan` T.Hearts
    T.Clubs `longerThan` T.Spades
    minimumResponse_ T.Diamonds T.Clubs 2

b1HoXo1S :: Action
b1HoXo1S = nameAction "b1HoXo1S" $ do
    minimumResponse_ T.Hearts T.Spades 1

b1HoXo2C :: Action
b1HoXo2C = nameAction "b1HoXo2C" $ do
    T.Clubs `longerThan` T.Spades
    minimumResponse_ T.Hearts T.Clubs 2

b1HoXo2D :: Action
b1HoXo2D = nameAction "b1HoXo2D" $ do
    T.Diamonds `longerThan` T.Spades
    minimumResponse_ T.Hearts T.Diamonds 2

b1SoXo2C :: Action
b1SoXo2C = nameAction "b1HoXo2C" $ do
    T.Clubs `longerThan` T.Hearts
    minimumResponse_ T.Spades T.Clubs 2

b1SoXo2D :: Action
b1SoXo2D = nameAction "b1HoXo2D" $ do
    T.Diamonds `longerThan` T.Hearts
    minimumResponse_ T.Spades T.Diamonds 2

b1SoXo2H :: Action
b1SoXo2H = nameAction "b1HoXo2H" $ do
    minimumResponse_ T.Spades T.Hearts 2


oneNotrumpAdvance_ :: T.Suit -> Action
oneNotrumpAdvance_ oppsSuit = do
    balancedHand
    forEach (T.otherSuits oppsSuit) (`maxSuitLength` 4)
    alternatives [ minSuitLength oppsSuit 4 >> soundHolding oppsSuit
                 , minSuitLength oppsSuit 5 >> hasStopper oppsSuit
                 ]
    makeCall $ T.Bid 1 T.Notrump

b1CoXo1N :: Action
b1CoXo1N = nameAction "b1CoXo1N" $ oneNotrumpAdvance_ T.Clubs

b1DoXo1N :: Action
b1DoXo1N = nameAction "b1DoXo1N" $ oneNotrumpAdvance_ T.Diamonds

b1HoXo1N :: Action
b1HoXo1N = nameAction "b1HoXo1N" $ oneNotrumpAdvance_ T.Hearts

b1SoXo1N :: Action
b1SoXo1N = nameAction "b1SoXo1N" $ oneNotrumpAdvance_ T.Spades
