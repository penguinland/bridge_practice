module Types(StIO) where

import Control.Monad.Trans.State.Strict(StateT)
import System.Random(StdGen)


type StIO = StateT StdGen IO
