module Topic(
  Situations
, choose
, (<~)
, Situationable
, wrap
, base
, option
, Topic(..)
) where

import Data.Bifunctor(first)
import System.Random(RandomGen, StdGen, next, split, mkStdGen)

import qualified Situation as S


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
-- TODO: We only ever use (<~) with option. Roll them into a single function.
(<~) :: Optionable o => (StdGen -> a -> o) ->
    ((StdGen -> a -> o) -> StdGen -> o) -> (StdGen -> o)
b <~ o = o b


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
