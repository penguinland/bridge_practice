module Situation (
  Situation
, situation
, SituationInstance(..)
, instantiate
) where

import Control.Monad.Trans.State.Strict(State, get, put)
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


-- TODO: Find a way to make this cleaner. Monad transformers might be a relevant
-- thing here?
instantiate :: Situation -> State StdGen (IO (Maybe SituationInstance))
instantiate (Situation dn v b dl c s) = do
    g <- get
    let (n, g') = next g
        instantiate' :: IO (Maybe SituationInstance)
        instantiate' = do
            maybeDeal <- eval dn v dl n
            -- This do notation takes care of the IO monad, and the binds take
            -- care of the Maybe monad.
            return (maybeDeal >>= return . SituationInstance b c s)
    put g'
    return instantiate'
