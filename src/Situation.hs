module Situation (
  Situation(..)
, situation
) where

-- TODO: Attach some kind of debugging string to each SituationInstance
-- (Situation?) so it's easy to look into which one is generating
-- unexpected/unintiuitive/incorrect deals.

import Auction(Action, finish)
import DealerProg(DealerProg)
import Output(Commentary)
import Structures(Bidding)
import Terminology(Call, Direction, Vulnerability)


data Situation =
    Situation String Bidding DealerProg Call Commentary Vulnerability Direction


situation :: String -> Action -> Call -> Commentary -> Vulnerability ->
    Direction -> Situation
situation r a c s v d = Situation r bidding deal c s v d
  where
    (bidding, deal) = finish d a
