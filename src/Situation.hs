module Situation (
  Situation(..)
, situation
, base
, (<~)
) where


import System.Random(StdGen, next)

import Auction(Action, finish)
import DealerProg(DealerProg)
import Output(Commentary)
import Structures(Bidding)
import Terminology(Call, Direction, Vulnerability)


data Situation =
    Situation String Bidding DealerProg Call Commentary Vulnerability Direction


situation :: String -> Action -> Call -> Commentary -> Vulnerability ->
    Direction -> Situation
situation r a c s v d = Situation r bidding deal c s v d
  where
    (bidding, deal) = finish d a


-- If you have a function that takes arguments and creates a Situation, call
-- base with it to pass in options via (<~). Example syntax:
-- sits :: Topic.Situations
-- sits = wrap $ base makeSits <~ [opt1A, opt1B] <~ [opt2A, opt2B, opt2C]
base :: Optionable o => (a -> o) -> (StdGen -> a -> o)
base = const

class Optionable o where
    (<~) :: (StdGen -> a -> o) -> [a] -> (StdGen -> o)

instance Optionable Situation where
    (f <~ as) g = let
        (n, g') = next g
        i = n `mod` length as :: Int
      in
        f g' (as !! i)

instance (Optionable s) => Optionable (b -> s) where
    (f <~ as) g = let
        (n, g') = next g
        i = n `mod` length as :: Int
      in
        f g' (as !! i)
