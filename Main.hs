import System.Random(mkStdGen)

import qualified Topics.JacobyTransfers as JacobyTransfers
import ProblemSet(outputLatex)


main :: IO ()
main = outputLatex 10 [JacobyTransfers.topic] "test" (mkStdGen 0) >> return ()
