module Situation (
  Situation
, sitRef
, situation
, SituationInstance(..)
, instantiate
) where

-- TODO: Attach some kind of debugging string to each SituationInstance
-- (Situation?) so it's easy to look into which one is generating
-- unexpected/unintiuitive/incorrect deals.

import Control.Monad.Trans.State.Strict(State)
import Data.List.Utils(join)
import System.Random(StdGen, next)

import Auction(Action, finish)
import DealerProg(DealerProg, eval)
import Output(Showable, toLatex, OutputType(..), Commentary)
import Random(use)
import Structures(Bidding, Deal)
import Terminology(Call, Direction, Vulnerability)


data Situation =
    Situation String Bidding DealerProg Call Commentary Vulnerability Direction
data SituationInstance = SituationInstance Bidding Call Commentary Deal String


sitRef :: Situation -> String
sitRef (Situation r _ _ _ _ _ _) = r


situation :: String -> Action -> Call -> Commentary -> Vulnerability ->
    Direction -> Situation
situation r a c s v d = Situation r bidding deal c s v d
  where
    (bidding, deal) = finish d a

instance Showable SituationInstance where
    toLatex (SituationInstance b c s d ds) =
        "\\problem{%\n" ++
            join "%\n}{%\n" [toLatex d, toLatex b, toLatex c, s LaTeX, ds] ++
            "%\n}"


-- TODO: Find a way to make this cleaner. Monad transformers might be a relevant
-- thing here?
instantiate :: String -> Situation ->
        State StdGen (IO (Maybe SituationInstance))
instantiate reference (Situation _ b dl c s v dn) = do
    n <- use next
    let instantiate' :: IO (Maybe SituationInstance)
        instantiate' = do
            maybeDeal <- eval dn v dl n
            -- This do notation takes care of the IO monad, and the binds take
            -- care of the Maybe monad.
            return (SituationInstance b c s <$> maybeDeal <*> Just (reference))
    return instantiate'
