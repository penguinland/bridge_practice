module SituationHelpers (
  wrapVulDlr
, stdWrap
) where

import System.Random(StdGen)

import Situation(Situation)
import Topic(base, (<~), wrap, Situations)
import Terminology(Direction, allDirections, Vulnerability, allVulnerabilities)


-- Used for taking a situation that has already had some options applied to it,
-- and giving it the options of any vulnerability and dealer
wrapVulDlr :: (StdGen -> Vulnerability -> Direction -> Situation) -> Situations
wrapVulDlr sit = wrap $ sit <~ allVulnerabilities <~ allDirections


-- Used for making Situations where the only parameterization is the
-- vulnerability and dealer.
stdWrap :: (Vulnerability -> Direction -> Situation) -> Situations
stdWrap = wrapVulDlr . base
