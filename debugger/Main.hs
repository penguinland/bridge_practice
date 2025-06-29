-- When something is going wrong with the system, give this program the debug
-- string of the problematic situation instance, and we'll print out information
-- about it.

import Control.Monad.Trans.State.Strict(runStateT)
import Data.List.Utils(join, split)
-- We import the Internal implementation of the RNG directly because it has
-- implementations of Read and ways to get at the inner guts of the StdGen than
-- the non-Internal version does not have. If we ever upgrade to a newer version
-- of System.Random, this might break.
import System.Random.Internal(StdGen(..))

import ProblemSet(generate)
import SupportedTopics(getNamedTopic)
import Topic(Topic(..))


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
    rng = StdGen { unStdGen = read innerRngString }
    namePieces = split "." fullSitName
    targetName = head namePieces
    sitName = join "." . tail $ namePieces
  in
    (targetName, sitName, rng)


debug :: Topic -> String -> StdGen -> IO String
debug topic sitName rng = do
    (sitInstList, _) <- runStateT (generate 1 [topic]) rng
    let sitInst = head sitInstList
    return "hello"



main :: IO ()
main = do
    input <- getContents
    let (targetName, sitName, rng) = parse input
        maybeTopic = getNamedTopic targetName
    print targetName
    print sitName
    print rng
    case maybeTopic of
        Nothing -> putStrLn $ "Unable to find Topic for " ++ targetName
        Just t -> debug t sitName rng >>= putStrLn
