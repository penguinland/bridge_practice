module Topic(
  Situations  -- Note that constructors aren't public; use wrap instead.
, choose
, wrap
, wrapVulDlr
, stdWrap
, Topic(..)
) where

import Control.Monad.Trans.State.Strict(State)
import Data.Bifunctor(first)
import System.Random(RandomGen, StdGen, next, split, mkStdGen)

import Random(use, pickItem)
import Situation(Situation, base, (<~))
import Terminology(Direction, allDirections, Vulnerability, allVulnerabilities)


-- This is solely to get Haskell to figure out that the Situationable typeclass
-- can apply to functions that take in random number generators. It would be
-- nice to get rid of this.
class Randomizer r where
    make :: RandomGen g => g -> (r, g)

instance Randomizer StdGen where
    make = first mkStdGen . next


data Situations = RawSit Situation
                | SitList [Situations]
                | SitFun (StdGen -> Situations)


class Situationable s where
    wrap :: s -> Situations

instance Situationable Situation where
    wrap = RawSit
instance (Situationable s) => Situationable [s] where
    wrap = SitList . map wrap
-- TODO: figure out how to do this next line without using Randomizer.
instance (Situationable s, Randomizer r, RandomGen r) =>
        Situationable (r -> s) where
    wrap f = SitFun (\g -> let (g', _) = make g in wrap (f g'))
instance Situationable Situations where
    wrap = id


-- The most common Situation parameters are letting anyone be the dealer and
-- letting anyone be vulnerable. Make some syntactic sugar for that.
wrapVulDlr :: (StdGen -> Vulnerability -> Direction -> Situation) -> Situations
wrapVulDlr sit = wrap $ sit <~ allVulnerabilities <~ allDirections
-- and more syntactic sugar for a Situation that is _only_ parameterized on
-- those features.
stdWrap :: (Vulnerability -> Direction -> Situation) -> Situations
stdWrap = wrapVulDlr . base


data Topic = Topic {topicName :: String
                   , refName :: String
                   , topicSituations :: Situations}


choose :: Topic -> State StdGen Situation
choose = choose' . topicSituations
  where
    choose' (RawSit s)   = return s
    choose' (SitList ss) = pickItem ss >>= choose'
    choose' (SitFun f)   = use split >>= (choose' . f)
