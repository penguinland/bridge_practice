import System.Random(mkStdGen, StdGen, split, next)

import Output(toLatex)
import Situation(instantiate)
import Topic(choose, situations)
import qualified JacobyTransfers
import ProblemSet(generate)


main :: IO ()
{-
main = let
    g = mkStdGen 0
    (g', g'') = split g
    problem = choose JacobyTransfers.topic g'
  in do
    maybeSitInst <- instantiate problem g''
    case maybeSitInst of
        Nothing -> putStrLn "invalid problem"
        Just s -> putStrLn . toLatex $ s
-}
main = do
    probs <- generate 1 [JacobyTransfers.topic] (mkStdGen 0)
    putStrLn . toLatex . (!! 0) $ probs
