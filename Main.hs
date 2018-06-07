import System.Random(mkStdGen, StdGen, split, next)

import Output(toLatex)
import Situation(instantiate)
import Topic(choose, situations)
import qualified JacobyTransfers
import ProblemSet(generate, outputLatex)


main :: IO ()
{-
main = do
    probs <- generate 2 [JacobyTransfers.topic] (mkStdGen 0)
    putStrLn . concat . map toLatex $ probs
-}
main = outputLatex 2 [JacobyTransfers.topic] "test" (mkStdGen 0) >>= putStrLn
