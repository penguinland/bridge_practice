import System.Random(mkStdGen)

import qualified JacobyTransfers
import ProblemSet(outputLatex)


main :: IO ()
main = outputLatex 10 [JacobyTransfers.topic] "test" (mkStdGen 0) >> return ()
