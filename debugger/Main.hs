-- When something is going wrong with the system, give this program the debug
-- string of the problematic situation instance, and we'll print out information
-- about it. Example usage:
--     echo "wool.2D SMGen 14968322863915291149 13964134228407631271" | stack run debugger

import Control.Monad(when)
import Control.Monad.Trans.State.Strict(runState, runStateT)
import Data.List.Utils(join)
-- We import the Internal implementation of the RNG directly because it has
-- implementations of Read and ways to get at the inner guts of the StdGen that
-- the non-Internal version does not have. If we ever upgrade to a newer version
-- of System.Random, this might break.
import System.Random.Internal(StdGen(..))

import DealerProg(toProgram)
import Output(toMonospace)
import Situation(Situation(..))
import SituationInstance(instantiate)
import SupportedTopics(getNamedTopic)
import Topic(Topic(..), choose)


-- Example string to parse:
-- wool.2D SMGen 14968322863915291149 13964134228407631271
parse :: String -> (String, String, StdGen)
parse input = let
    pieces = words input
    fullSitName = head pieces
    innerRngString = join " " . tail $ pieces
    rng = StdGen {unStdGen = read innerRngString}
    (targetName, sitName') = break (== '.') fullSitName
    sitName = tail sitName'  -- Remove the period we broke on.
  in
    (targetName, sitName, rng)


debug :: Topic -> String -> StdGen -> IO String
debug topic sitName rng = do
    let (sit, rng') = runState (choose topic) rng
    when (sitName /= sitRef sit)
        (error "Assertion error: situation name doesn't match")
    (maybeSitInst, _) <- runStateT (instantiate "" sit) rng'
    case maybeSitInst of
        Nothing      -> error "Couldn't instantiate situation!?"
        Just sitInst -> return $ (toProgram . sitDealerProg $ sit) ++ "\n\n" ++
                                 toMonospace sitInst


main :: IO ()
main = do
    input <- getContents
    let (targetName, sitName, rng) = parse input
        maybeTopic = getNamedTopic targetName
    putStrLn targetName
    putStrLn sitName
    print rng
    putStrLn ""
    case maybeTopic of
        Nothing -> putStrLn $ "Unable to find Topic for " ++ targetName
        Just t -> debug t sitName rng >>= putStrLn
