module Situation (
  Situation(..)
, situation
, base
, (<~)
) where


import Data.Bifunctor(first)
import System.Random(StdGen, genWord64R)

import Auction(Action, finish, extractLastCall)
import DealerProg(DealerProg)
import Output(Commentary)
import Structures(Bidding)
import Terminology(CompleteCall, Direction, Vulnerability)


data Situation = Situation String Bidding DealerProg CompleteCall Commentary
                           Vulnerability Direction

-- The first action is the auction up until now. The second one is some snippet
-- of auction whose last bid is the intended answer to the situation. We make
-- this a convenience, so you can reuse Actions for both constructing the
-- auction and describing the next action that should follow.
situation :: String -> Action -> Action -> Commentary -> Vulnerability ->
    Direction -> Situation
situation r a c s v d = Situation r bidding deal answer s v d
  where
    (bidding, deal) = finish d a
    answer = extractLastCall c


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
        -- We use Int, but StdGen uses Word64. Cast between them via Integer.
        maxIndex = fromInteger . toInteger . subtract 1 . length $ as
        (i, g') = first (fromInteger . toInteger) . genWord64R maxIndex $ g
      in
        f g' (as !! i)

instance (Optionable s) => Optionable (b -> s) where
    (f <~ as) g = let
        maxIndex = fromInteger . toInteger . subtract 1 . length $ as
        (i, g') = first (fromInteger . toInteger) . genWord64R maxIndex $ g
      in
        f g' (as !! i)
