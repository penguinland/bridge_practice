-- When something is going wrong with the system, give this program the debug
-- string of the problematic situation instance, and we'll print out information
-- about it. Example usage:
--     echo "wool.2D StdGen {unStdGen = SMGen 14968322863915291149 13964134228407631271}" | stack run debugger

import Control.Monad(when)
import Control.Monad.Trans.State.Strict(runState, runStateT)
import Data.List.Utils(join, split)
-- We import the Internal implementation of the RNG directly because it has
-- implementations of Read and ways to get at the inner guts of the StdGen that
-- the non-Internal version does not have. If we ever upgrade to a newer version
-- of System.Random, this might break.
import System.Random.Internal(StdGen(..))

import DealerProg(toProgram)
import Output(toLatex)
import Situation(Situation(..))
import SituationInstance(instantiate)
import SupportedTopics(getNamedTopic)
import Topic(Topic(..), choose)


-- Example string to parse:
-- wool.2D StdGen {unStdGen = SMGen 14968322863915291149 13964134228407631271}
parse :: String -> (String, String, StdGen)
parse input = let
    pieces = split " " input
    fullSitName = head pieces
    -- Confusingly, although StdGen is Show, it is not Read but all its internal
    -- pieces are. So, parse it more manually. We call tail 4 times to take off
    -- the topic name and "StdGen {unStdGen =", join the rest together, and then
    -- call init twice to remove the newline and '}' from the end.
    innerRngString = init . init . join " " . tail . tail . tail . tail $ pieces
    rng = StdGen {unStdGen = read innerRngString}
    namePieces = split "." fullSitName
    targetName = head namePieces
    sitName = join "." . tail $ namePieces
  in
    (targetName, sitName, rng)


debug :: Topic -> String -> StdGen -> IO String
debug topic sitName rng = do
    let (sit, rng') = runState (choose topic) rng
    assert $ subnameMatches sitName sit
    (maybeSitInst, _) <- runStateT (instantiate "" sit) rng'
    case maybeSitInst of
        Nothing      -> error "Couldn't instantiate situation!?"
        Just sitInst -> return $ showDealerProg sit ++ "\n\n" ++ toLatex sitInst
  where
    assert :: Bool -> IO ()
    assert value =
        when (not value) (error "Assertion error: situation name doesn't match")
    subnameMatches :: String -> Situation -> Bool
    subnameMatches expected (Situation r _ _ _ _ _ _) = r == expected
    showDealerProg :: Situation -> String
    showDealerProg (Situation _ _ dp _ _ _ _) = toProgram dp


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
