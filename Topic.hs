module Topic(
  Situations
, choose
, (<~)
, Situationable
, wrap
, base
, Topic(..)
) where

-- TODO: Use this for StdGen stuff.
import Control.Monad.Trans.State.Strict(State, runState, get, put)
import Data.Bifunctor(first)
import System.Random(RandomGen, StdGen, next, split, mkStdGen)

import qualified Situation as S


-- This is solely to get Haskell to figure out that the Situationable typeclass
-- can apply to functions that take in random number generators. It would be
-- nice to get rid of this.
class Randomizer r where
    make :: RandomGen g => g -> (r, g)

instance Randomizer StdGen where
    make = first mkStdGen . next


data Situations = RawSit S.Situation
                | SitList [Situations]
                | SitFun (StdGen -> Situations)


class Situationable s where
    wrap :: s -> Situations

instance Situationable S.Situation where
    wrap = RawSit
instance (Situationable s) => Situationable [s] where
    wrap = SitList . map wrap
-- TODO: figure out how to do this next line without using Randomizer.
instance (Situationable s, Randomizer r, RandomGen r) =>
        Situationable (r -> s) where
    wrap f = SitFun $ (\g -> let (g', _) = make g in wrap (f g'))
instance Situationable Situations where
    wrap = id


-- If you have a function that takes arguments and makes Situations, call base
-- with it to pass in options via (<~). Example syntax:
-- sits :: Situations
-- sits = wrap $ base makeSits <~ [opt1A, opt1B] <~ [opt2A, opt2B, opt2C]
base :: Optionable o => (a -> o) -> (StdGen -> a -> o)
base = const

class Optionable o where
    option :: [a] -> (StdGen -> a -> o) -> (StdGen -> o)

instance Optionable S.Situation where
    option as f g = let
        (n, g') = next g
        i = n `mod` length as :: Int
      in
        f g' $ (as !! i)

instance (Optionable s) => Optionable (b -> s) where
    option as f g = let
        (n, g') = next g
        i = n `mod` length as :: Int
      in
        f g' $ (as !! i)

-- This is a way of applying options to the base version.
(<~) :: Optionable o => (StdGen -> a -> o) -> [a] -> (StdGen -> o)
b <~ as = option as b


choose :: Topic -> StdGen -> S.Situation
choose = choose' . situations
  where
    choose' (RawSit s)   _ = s
    choose' (SitList ss) g = let
        (n, g') = next g
        i = n `mod` length ss :: Int
      in
        choose' (ss !! i) g'
    choose' (SitFun f)   g = let
        (g', g'') = split g
      in
        choose' (f g') g''


data Topic = Topic {topicName :: String, situations :: Situations}
