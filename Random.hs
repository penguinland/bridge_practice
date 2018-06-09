module Random(
  use
, pickItem
) where

import Control.Monad.Trans.State.Strict(State, get, put)
import System.Random(StdGen, randomR)


use :: (StdGen -> (a, StdGen)) -> State StdGen a
use f = do
    g <- get
    let (a, g') = f g
    put g'
    return a


pickItem :: [a] -> State StdGen a
pickItem [] = error "Picked item from empty list"
pickItem as = do
  i <- use $ randomR (0, length as - 1)
  return (as !! i)
