-- The type signature of wrap needs to bind some but not all type parameters to
-- the Collectable typeclass; FlexibleContexts lets us do this.
{-# LANGUAGE FlexibleContexts #-}


module Topic(
  Situations  -- Note that constructors aren't public: use wrap instead.
, wrap
, wrapWeighted
, wrapDlr
, wrapNW
, wrapSE
, stdWrap
, stdWrapNW
, stdWrapSE
, Topic(..)
, makeTopic
) where

import Control.Monad.Trans.State.Strict(State)
import System.Random(StdGen)

import Collection(Collection, collect, Collectable, weightedList)
import Output(Description, toDescription, Showable)
import Situation(Situation, (<~))
import Terminology(
    Direction(..), allDirections, Vulnerability, allVulnerabilities)


type Situations = Collection Situation

-- Syntactic sugar: if you use `collect` nested multiple times to create a
-- `Topic`, the type checker gets stuck with ambiguous type variables. Using
-- `wrap` instead tells the type checker that we're wrapping a Situation,
-- without needing to copy and paste type signatures dozens of times.
wrap :: Collectable Situation c => c -> Situations
wrap = collect


wrapWeighted :: Collectable Situation c => [(Int, c)] -> Situations
wrapWeighted = collect . weightedList


-- The most common Situation parameters are letting anyone be vulnerable, and
-- either letting anyone be the dealer, ensuring North is an unpassed hand, or
-- ensuring South is an unpassed hand. Make some syntactic sugar for that.
wrapDlr :: State StdGen (Direction -> Vulnerability -> Situation) -> Situations
wrapDlr sit = wrap $ sit <~ allDirections <~ allVulnerabilities

-- By ensuring that only North or West could be dealer, if we make North open
-- the bidding, we ensure South is an unpassed hand. This is useful when
-- practicing game-forcing auctions when partner opens the bidding.
wrapNW :: State StdGen (Direction -> Vulnerability -> Situation) -> Situations
wrapNW sit = wrap $ sit <~ [North, West] <~ allVulnerabilities

-- By ensuring that only South or East could be dealer, if we make South open
-- the bidding, we ensure that North is an unpassed hand. This is useful when
-- practicing game-forcing auctions when we open the bidding.
wrapSE :: State StdGen (Direction -> Vulnerability -> Situation) -> Situations
wrapSE sit = wrap $ sit <~ [South, East] <~ allVulnerabilities

-- and more syntactic sugar for a Situation that is _only_ parameterized on
-- those features.
stdWrap :: (Direction -> Vulnerability -> Situation) -> Situations
stdWrap = wrapDlr . return

stdWrapNW :: (Direction -> Vulnerability -> Situation) -> Situations
stdWrapNW = wrapNW . return

stdWrapSE :: (Direction -> Vulnerability -> Situation) -> Situations
stdWrapSE = wrapSE . return


data Topic = Topic { topicName :: Description
                   , refName :: String
                   , topicSituations :: Situations
                   }


-- The intuitive name for this is `topic`, but most of the actual Topic values
-- in other files are named that. This is named as a verb to distinguish it from
-- the values it generates.
makeTopic :: Showable a => a -> String -> Situations -> Topic
makeTopic d n s = Topic (toDescription d) n s
