module Situation (
  Situation(..)
, situation
, sitRef
, (<~)
) where


import Control.Monad.Trans.State.Strict(State)
import System.Random(StdGen)

import Action(Action, finish, extractLastCall, withholdBid)
import DealerProg(DealerProg)
import Output(Showable, Commentary, toCommentary)
import Random(pickItem)
import Structures(Bidding)
import Terminology(CompleteCall, Direction, Vulnerability)


data Situation = Situation String Bidding DealerProg CompleteCall Commentary
                           Direction Vulnerability

-- The first action is the auction up until now. The second one is some snippet
-- of auction whose last bid is the intended answer to the situation. We make
-- this a convenience, so you can reuse Actions for both constructing the
-- auction and describing the next action that should follow.
situation :: Showable s => String -> Action -> Action -> s -> Direction ->
    Vulnerability -> Situation
situation r a c s d v = Situation r bidding deal answer (toCommentary s) d v
  where
    (bidding, deal) = finish d (a >> withholdBid c)
    answer = extractLastCall c


sitRef :: Situation -> String
sitRef (Situation r _ _ _ _ _ _) = r


(<~) :: State StdGen (a -> b) -> [a] -> State StdGen b
sf <~ as = sf <*> pickItem as
