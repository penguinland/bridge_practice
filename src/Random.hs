module Random(
  pickItem
) where

import Control.Monad.Trans.State.Strict(StateT, state)
import System.Random(StdGen, randomR)


pickItem :: Monad m => [a] -> StateT StdGen m a
pickItem [] = error "Picked item from empty list"
pickItem as = do
    i <- state $ randomR (0, length as - 1)
    return (as !! i)
