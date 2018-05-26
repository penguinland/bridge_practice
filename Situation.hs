module Situation (
  Situation(..)
, SituationInstance(..)
, instantiate
) where

import Data.List.Utils(join)

import DealerProg(DealerProg, eval, Deal)
import Output(Showable, toLatex)
import Structures(Bidding)
import Terminology(Call, Direction, Vulnerability)


data Situation = Situation Direction Vulnerability DealerProg Bidding Call String
data SituationInstance = SituationInstance Bidding Call String Deal

instance Showable SituationInstance where
    toLatex (SituationInstance b c s d) =
        "\\problem{" ++ join "%\n}{%\n" [toLatex d
                                        ,toLatex b
                                        ,toLatex c
                                        ,s] ++ "%\n}"


instantiate ::Situation -> Int -> IO (Maybe SituationInstance)
instantiate (Situation dn v dl b c s) seed = do
    maybeDeal <- eval dn v dl seed
    return (maybeDeal >>= return . SituationInstance b c s)
