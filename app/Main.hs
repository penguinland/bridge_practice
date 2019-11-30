import System.Random(mkStdGen)

--import qualified Topics.JacobyTransfers as JacobyTransfers
--import qualified Topics.MinorTransfersScott as MinorTransfers
--import qualified Topics.PrecisionOpeners as PrecisionOpeners
import qualified Topics.StandardModernPrecision.TwoDiamondOpeners as TwoDOpen
import ProblemSet(outputLatex)


main :: IO ()
main = outputLatex 20 [TwoDOpen.topic] "test" (mkStdGen 0) >> return ()
