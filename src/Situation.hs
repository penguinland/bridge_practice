{-# LANGUAGE FlexibleInstances #-}  -- `State StdGen p` can be in a class
{-# LANGUAGE TypeFamilies #-}  -- Lets you define a type inside a class
--{-# LANGUAGE RankNTypes #-}  -- Lets you use `forall`

module Situation (
  Situation(..)
, situation
, sitRef
, Parameterizable
, (<~)
) where


import Control.Monad.Trans.State.Strict(State)
import System.Random(StdGen)

import Action(Action, finish, extractLastCall, withholdBid)
import DealerProg(DealerProg)
import Output(Showable, Description, toDescription)
import Random(pickItem)
import Structures(Bidding)
import Terminology(CompleteCall, Direction, Vulnerability)


data Situation = Situation String Bidding DealerProg CompleteCall Description
                           Direction Vulnerability

-- The first action is the auction up until now. The second one is some snippet
-- of auction whose last bid is the intended answer to the situation. We make
-- this a convenience, so you can reuse Actions for both constructing the
-- auction and describing the next action that should follow.
situation :: Showable s => String -> Action -> Action -> s -> Direction ->
    Vulnerability -> Situation
situation r a c s d v = Situation r bidding deal answer (toDescription s) d v
  where
    (bidding, deal) = finish d (a >> withholdBid c)
    answer = extractLastCall c


sitRef :: Situation -> String
sitRef (Situation r _ _ _ _ _ _) = r


class Parameterizable p where
    type StatedType p
    toPreSituation :: p -> StatedType p

instance Parameterizable Situation where
    type StatedType Situation = State StdGen Situation
    toPreSituation = return

instance (Parameterizable q) => Parameterizable (p -> q) where
    type StatedType (p -> q) = State StdGen (p -> q)
    toPreSituation = return

instance (Parameterizable p) => Parameterizable (State StdGen p) where
    type StatedType (State StdGen p) = State StdGen p
    toPreSituation = id


-- TODO: this is still wrong.
(<~) :: Parameterizable (a -> b) => (a -> b) -> [a] -> State StdGen b
sf <~ as = toPreSituation sf <*> pickItem as
