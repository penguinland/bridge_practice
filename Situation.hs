module Situation (
  Situation
, situation
, SituationInstance(..)
, instantiate
) where

import Data.List.Utils(join)
import System.Random(StdGen, next)

import Auction(Action, finish)
import DealerProg(DealerProg, eval)
import Output(Showable, toLatex, OutputType(..))
import Structures(Bidding, Deal)
import Terminology(Call, Direction, Vulnerability)


type Commentary = OutputType -> String

data Situation =
    Situation Direction Vulnerability Bidding DealerProg Call Commentary
data SituationInstance = SituationInstance Bidding Call Commentary Deal

situation :: Direction -> Vulnerability -> Action -> Call ->
    Commentary -> Situation
situation d v a c s = Situation d v bidding deal c s
  where
    (bidding, deal) = finish d a

instance Showable SituationInstance where
    toLatex (SituationInstance b c s d) =
        "\\problem{" ++
            join "%\n}{%\n" [toLatex d, toLatex b, toLatex c, s LaTeX] ++
            "%\n}"


instantiate ::Situation -> StdGen -> IO (Maybe SituationInstance)
instantiate (Situation dn v b dl c s) g = do
    maybeDeal <- eval dn v dl (fst . next $ g)
    -- The do notation takes care of the IO monad, and the binds take care of
    -- the Maybe monad.
    return (maybeDeal >>= return . SituationInstance b c s)
