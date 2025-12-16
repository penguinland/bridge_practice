module ProblemSet(
  generate
) where

import Control.Monad(when)
import Control.Monad.Trans.Class(lift)
import Control.Monad.Trans.State.Strict(get, mapStateT)
import Data.Functor.Identity(runIdentity)
import System.IO(hFlush, stdout)
import System.Random(StdGen)

import Random(pickItem)
import Situation(Situation(..))
import SituationInstance(instantiate, SituationInstance)
import Topic(Topic(..), refName, choose)
import Types(StIO)


reference :: String -> String -> StdGen -> String
reference topic sit g = topic ++ "." ++ sit ++ " " ++ randomSeed
  where
    -- The StdGen is something like "StdGen {unStdGen = SMGen 1 2}", but all we
    -- really need is the "SMGen 1 2".
    -- NOTE: if we ever change PRNG implementations, this might break!
    randomSeed = init . unwords . drop 3 . words . show $ g


generate :: Int -> [Topic] -> StIO [SituationInstance]
generate n topics = sequence . map getOneSituation $ [1..n]
  where
    -- We keep the index i around solely so we can print a dot every 10
    -- SituationInstances we generate.
    getOneSituation i = do
        topic <- pickItem topics
        gen <- get  -- Save a copy of the RNG to use in the debug string later.
        -- We use mapStateT to convert from a `State StdGen Situation` to a
        -- `StIO Situation`. This lets us keep the IO monad out of the rest of
        -- the code.
        situation <- mapStateT (return . runIdentity) . choose .
                     topicSituations $ topic
        let ref = reference (refName topic) (sitRef situation) gen
        maybeSit <- instantiate ref situation
        case maybeSit of
            Nothing -> getOneSituation i  -- Try again
            Just d  -> do
                when (i `mod` 10 == 0) (lift $ putStr "." >> hFlush stdout)
                return d
