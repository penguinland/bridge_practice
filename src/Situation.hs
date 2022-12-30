module Situation (
  Situation(..)
, situation
, (<~)
) where


import Control.Monad.Trans.State.Strict(State)
import System.Random(StdGen)

import Auction(Action, finish, extractLastCall)
import DealerProg(DealerProg)
import Output(Commentary)
import Random(pickItem)
import Structures(Bidding)
import Terminology(CompleteCall, Direction, Vulnerability)


data Situation = Situation String Bidding DealerProg CompleteCall Commentary
                           Vulnerability Direction

-- The first action is the auction up until now. The second one is some snippet
-- of auction whose last bid is the intended answer to the situation. We make
-- this a convenience, so you can reuse Actions for both constructing the
-- auction and describing the next action that should follow.
situation :: String -> Action -> Action -> Commentary -> Vulnerability ->
    Direction -> Situation
situation r a c s v d = Situation r bidding deal answer s v d
  where
    (bidding, deal) = finish d a
    answer = extractLastCall c


(<~) :: State StdGen (a -> b) -> [a] -> State StdGen b
sf <~ as = sf <*> pickItem as
