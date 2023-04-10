-- In order to add the State monad to the Situationable typeclass, we need this
-- pragma. State is an alias for a trivial version of a more complicated monad,
-- and the default compiler doesn't let that be in a type class because not all
-- its arguments are type variables.
{-# LANGUAGE FlexibleInstances #-}

module Topic(
  Situations  -- Note that constructors aren't public; use wrap instead.
, choose
, wrap
, wrapVulDlr
, stdWrap
, Topic(..)
, makeTopic
) where

import Control.Monad.Trans.State.Strict(State)
import System.Random(StdGen)

import Output(Commentary, toCommentary, Showable)
import Random(pickItem)
import Situation(Situation, (<~))
import Terminology(Direction, allDirections, Vulnerability, allVulnerabilities)


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


-- The most common Situation parameters are letting anyone be the dealer and
-- letting anyone be vulnerable. Make some syntactic sugar for that.
wrapVulDlr :: State StdGen (Vulnerability -> Direction -> Situation) ->
    Situations
wrapVulDlr sit = wrap $ sit <~ allVulnerabilities <~ allDirections
-- and more syntactic sugar for a Situation that is _only_ parameterized on
-- those features.
stdWrap :: (Vulnerability -> Direction -> Situation) -> Situations
stdWrap = wrapVulDlr . return


data Topic = Topic { topicName :: Commentary
                   , refName :: String
                   , topicSituations :: Situations}


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
