module Situation (
  Situation
, situation
, SituationInstance(..)
, instantiate
) where

import Data.List.Utils(join)
import System.Random(StdGen, next)

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


instantiate ::Situation -> StdGen -> IO (Maybe SituationInstance)
instantiate (Situation dn v b dl c s) g = do
    maybeDeal <- eval dn v dl (fst . next $ g)
    return (maybeDeal >>= return . SituationInstance b c s)
