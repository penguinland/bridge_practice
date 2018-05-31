module Situation (
  Situation
, situation
, SituationInstance(..)
, instantiate
) where

import Data.List.Utils(join)

import Auction(Action, finish)
import DealerProg(DealerProg, eval, Deal)
import Output(Showable, toLatex, OutputType(..))
import Structures(Bidding)
import Terminology(Call, Direction, Vulnerability)


data Situation =
    Situation Direction Vulnerability Bidding DealerProg Call
        (OutputType -> String)
data SituationInstance =
    SituationInstance Bidding Call (OutputType -> String) Deal

situation :: Direction -> Vulnerability -> Action -> Call ->
    (OutputType -> String) -> Situation
situation d v a c s = Situation d v bidding deal c s
  where
    (bidding, deal) = finish d a

instance Showable SituationInstance where
    toLatex (SituationInstance b c s d) =
        "\\problem{" ++ join "%\n}{%\n" [toLatex d
                                        ,toLatex b
                                        ,toLatex c
                                        ,s LaTeX] ++ "%\n}"


instantiate ::Situation -> Int -> IO (Maybe SituationInstance)
instantiate (Situation dn v b dl c s) seed = do
    maybeDeal <- eval dn v dl seed
    return (maybeDeal >>= return . SituationInstance b c s)
