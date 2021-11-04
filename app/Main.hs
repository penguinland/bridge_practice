import System.Random(mkStdGen)

--import qualified Topics.JacobyTransfers as JacobyTransfers
--import qualified Topics.MinorTransfersScott as MinorTransfers
--import qualified Topics.PrecisionOpeners as PrecisionOpeners
--import qualified Topics.StandardModernPrecision.TwoDiamondOpeners as TwoDOpen
--import qualified Topics.StandardOpeners as StandardOpeners
import qualified Topics.StandardModernPrecision.OpeningBids as SmpOpenings
import ProblemSet(outputLatex)


main :: IO ()
main = outputLatex 100 [SmpOpenings.topic] "test" (mkStdGen 0) >> return ()
