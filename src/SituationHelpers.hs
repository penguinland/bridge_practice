module SituationHelpers (
  anyVulnerability
, anyVulDlr
) where

import Auction(Action)
import Output(Commentary)
import Situation(situation, Situation)
import Topic(base, (<~), wrap, Situations)
import Terminology(allDirections, allVulnerabilities, Call)


anyVulnerability :: Situation -> Situation
anyVulnerability = error "TODO later"

anyVulDlr :: String -> Action -> Call -> Commentary -> Situations
anyVulDlr debug action call commentary = let
    helper dir vul = situation debug dir vul action call commentary
  in
    wrap $ base helper <~ allDirections <~ allVulnerabilities
