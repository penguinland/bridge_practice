import System.Random(mkStdGen, StdGen, split, next)

import Output(toLatex)
import Situation(instantiate)
import Topic(choose, situations)
import qualified JacobyTransfers


main :: IO ()
main = let
    g = mkStdGen 0
    (g', g'') = split g
    problem = choose (situations JacobyTransfers.topic) g'
  in do
    maybeSitInst <- instantiate problem (fst $ next g'')
    case maybeSitInst of
        Nothing -> putStrLn "invalid problem"
        Just s -> putStrLn . toLatex $ s
