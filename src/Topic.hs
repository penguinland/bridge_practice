-- In order to add the State monad to the Collectable typeclass, we need this
-- pragma. State is an alias for a trivial version of a more complicated monad,
-- and the default compiler doesn't let that be in a type class because not all
-- its arguments are type variables.
{-# LANGUAGE FlexibleInstances #-}
-- The Collectable typeclass takes 2 type arguments: the thing being collected
-- and the thing doing the collecting. Consequently, we need to allow multiple
-- parameters in typeclasses.
{-# LANGUAGE MultiParamTypeClasses #-}
-- The type signature of wrap needs to bind some but not all type parameters to
-- the Collectable typeclass; FlexibleContexts lets us do this.
{-# LANGUAGE FlexibleContexts #-}


module Topic(
  Situations  -- Note that constructors aren't public: use wrap instead.
, choose
, wrap
, wrapDlr
, wrapNW
, wrapSE
, stdWrap
, stdWrapNW
, stdWrapSE
, Topic(..)
, makeTopic
, collect
) where

import Control.Monad.Trans.State.Strict(State, runState)
import System.Random(StdGen, mkStdGen)

import Output(Description, toDescription, Showable)
import Random(pickItem)
import Situation(Situation, (<~))
import Terminology(
    Direction(..), allDirections, Vulnerability, allVulnerabilities)


data Collection a = CollectionRaw a
                  | CollectionList [Collection a]
                  | CollectionState (State StdGen (Collection a))

type Situations = Collection Situation


class Collectable r c where
    _collect :: c -> Collection r

instance Collectable r r where
    _collect = CollectionRaw
instance (Collectable r c) => Collectable r [c] where
    _collect = CollectionList . map _collect
instance (Collectable r c) => Collectable r (State StdGen c) where
    _collect = CollectionState . fmap _collect
instance Collectable r (Collection r) where
    _collect = id

wrap :: Collectable Situation c => c -> Situations
wrap = _collect


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


choose :: Topic -> State StdGen Situation
choose = choose' . topicSituations
  where
    choose' (CollectionRaw s)   = return s
    choose' (CollectionList ss) = pickItem ss >>= choose'
    choose' (CollectionState f) = f >>= choose'


-- This is used during compile-time assertions to ensure that every Situation
-- within a Topic has a unique debug string.
collect :: (Situation -> a) -> Topic -> [a]
collect f = collect' . topicSituations
  where
    collect' (CollectionRaw s)  = [f s]
    collect' (CollectionList l) = concatMap collect' l
    -- We assume that all Situations you could generate from a CollectionState have the
    -- same value within. If this changes, revisit this.
    collect' (CollectionState s) = collect' . fst . flip runState (mkStdGen 0) $ s
