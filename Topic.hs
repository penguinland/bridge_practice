module Topic(
  Choosable
, choice
) where

import Data.Bifunctor(first)
import System.Random(RandomGen, StdGen, next, split, mkStdGen)

import qualified Situation as S

data Topic = Topic String [S.Situation]

getName :: Topic -> String
getName (Topic s _) = s

-- TODO: make more random
getSituation :: Topic -> Int -> S.Situation
getSituation (Topic _ sits) i = sits !! i


class Choosable s where
    --choice :: RandomGen r => r -> s -> S.Situation
    choice :: StdGen -> s -> S.Situation

instance Choosable S.Situation where
    choice _ s = s

instance (Choosable c) => Choosable [c] where
    -- TODO: figure out how to choose a random element of a list canonically
    choice g l = let
        (i, g') = next g
        i' = i `mod` (length l) :: Int
      in
        choice g' (l !! i')

-- Here's what I wanted to do:
-- instance (Choosable c, Randomizer r, RandomGen r) => Choosable (r -> c) where
--     choice g f = let
--         (g', g'') = split g
--       in
--         choice g'' (f g')
-- However, Haskell got tripped up because it couldn't figure out that the first
-- argument of choice and the first argument of f could have the same type. So,
-- the thing below about Randomizer is a kludge to get around this. I'd like to
-- remove it some day, if possible.
class Randomizer r where
    make :: RandomGen g => g -> (r, g)

instance Randomizer StdGen where
    make = first mkStdGen . next

-- This is why I needed to create Randomizer.
instance (Choosable c, Randomizer r, RandomGen r) => Choosable (r -> c) where
    choice g f = let
        (g', g'') = make g  -- I wish this were just `split` instead.
      in
        choice g'' (f g')
