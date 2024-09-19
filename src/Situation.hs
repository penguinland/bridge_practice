{-# LANGUAGE RankNTypes #-}  -- Add the `forall` keyword
{-# LANGUAGE FlexibleInstances #-}  -- `State StdGen p` can be in a class
{-# LANGUAGE MultiParamTypeClasses #-}  -- Lets Parameterizable take 2 args
{-# LANGUAGE FlexibleContexts #-}  -- Lets you have `Parameterizable p (a -> b)`

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


class Parameterizable p q where
    toPreSituation :: p -> State StdGen q

instance Parameterizable (State StdGen p) p where
    toPreSituation = id

instance Parameterizable p p where
    toPreSituation = return


(<~) :: forall p a b. Parameterizable p (a -> b) => p -> [a] -> State StdGen b
sf <~ as = toPreSituation sf <*> pickItem as
