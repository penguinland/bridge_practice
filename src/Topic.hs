-- In order to add the State monad to the Situationable typeclass, we need this
-- pragma. State is an alias for a trivial version of a more complicated monad,
-- and the default compiler doesn't let that be in a type class because not all
-- its arguments are type variables.
{-# LANGUAGE FlexibleInstances #-}

module Topic(
  Situations  -- Note that constructors aren't public; use wrap instead.
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
) where

import Control.Monad.Trans.State.Strict(State)
import System.Random(StdGen)

import Output(Commentary, toCommentary, Showable)
import Random(pickItem)
import Situation(Situation, (<~))
import Terminology(
    Direction(..), allDirections, Vulnerability, allVulnerabilities)


data Situations = RawSit Situation
                | SitList [Situations]
                | SitState (State StdGen Situations)


class Situationable s where
    wrap :: s -> Situations

instance Situationable Situation where
    wrap = RawSit
instance (Situationable s) => Situationable [s] where
    wrap = SitList . map wrap
instance (Situationable s) => Situationable (State StdGen s) where
    wrap = SitState . fmap wrap
instance Situationable Situations where
    wrap = id


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


data Topic = Topic { topicName :: Commentary
                   , refName :: String
                   , topicSituations :: Situations }


-- The intuitive name for this is `topic`, but most of the actual Topic values
-- in other files are named that. This is named as a verb to distinguish it from
-- the values it generates.
makeTopic :: Showable a => a -> String -> Situations -> Topic
makeTopic c n s = Topic (toCommentary c) n s


choose :: Topic -> State StdGen Situation
choose = choose' . topicSituations
  where
    choose' (RawSit s)   = return s
    choose' (SitList ss) = pickItem ss >>= choose'
    choose' (SitState f) = f >>= choose'
