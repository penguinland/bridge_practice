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

--import Control.Monad.Fix(MonadFix)
import Data.Bifunctor(first)
import System.Random(RandomGen, StdGen, next, split, mkStdGen)

import qualified Situation as S


class Randomizer r where
    make :: RandomGen g => g -> (r, g)

instance Randomizer StdGen where
    make = first mkStdGen . next

data SituationsUnfixed next = RawSit S.Situation
                            | SitList [next]
                            | SitFun (StdGen -> next)

-- The "Haskell For All" post about free monads suggests that this already
-- exists somewhere, but I couldn't find it on Hoogle.
data Fix f = Fix (f (Fix f))

-- TODO: Make this into a data or newtype so that Situations can themselves be
-- Situationable below.
type Situations = Fix SituationsUnfixed


class Situationable s where
    wrap :: s -> Situations

instance Situationable S.Situation where
    wrap = Fix . RawSit
instance (Situationable s) => Situationable [s] where
    wrap = Fix . SitList . map wrap
instance (Situationable s, Randomizer r, RandomGen r) =>
        Situationable (r -> s) where
    wrap f = Fix . SitFun $ (\g -> let (g', _) = make g in wrap (f g'))
-- The following doesn't work because Situations is a type synonym and you can
-- only apply typeclasses to non-synonyms.
--instance Situationable Situations where
--    wrap = id


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
(<~) :: Optionable o => (StdGen -> a -> o) ->
    ((StdGen -> a -> o) -> StdGen -> o) -> (StdGen -> o)
b <~ o = o b


choose :: Situations -> StdGen -> S.Situation
choose (Fix (RawSit s))   _ = s
choose (Fix (SitList ss)) g = let
    (n, g') = next g
    i = n `mod` length ss :: Int
  in
    choose (ss !! i) g'
choose (Fix (SitFun f))   g = let
    (g', g'') = split g
  in
    choose (f g') g''


data Topic = Topic {name :: String, situations :: Situations}
