module SituationHelpers (
  anyVulnerability
, anyVulDlr
, wrapVulDlr
, stdWrap
) where

import System.Random(StdGen)

import Auction(Action)
import Output(Commentary)
import Situation(situation, Situation)
import Topic(base, (<~), wrap, Situations)
import Terminology(Direction, allDirections, Vulnerability, allVulnerabilities, Call)


anyVulnerability :: Situation -> Situation
anyVulnerability = error "TODO later"

anyVulDlr :: String -> Action -> Call -> Commentary -> Situations
anyVulDlr debug action call commentary = let
    helper dir vul = situation debug dir vul action call commentary
  in
    wrap $ base helper <~ allDirections <~ allVulnerabilities


-- Used for taking a situation that has already had some options applied to it,
-- and giving it the options of any vulnerability and dealer
wrapVulDlr :: (StdGen -> Vulnerability -> Direction -> Situation) -> Situations
wrapVulDlr sit = wrap $ sit <~ allVulnerabilities <~ allDirections


-- Used for making Situations where the only parameterization is the
-- vulnerability and dealer.
stdWrap :: (Vulnerability -> Direction -> Situation) -> Situations
stdWrap = wrapVulDlr . base
