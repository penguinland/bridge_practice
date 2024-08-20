module Random(
  pickItem
) where

import Control.Monad.Trans.State.Strict(State, state)
import System.Random(StdGen, randomR)


pickItem :: [a] -> State StdGen a
pickItem [] = error "Picked item from empty list"
pickItem as = do
    i <- state $ randomR (0, length as - 1)
    return (as !! i)
