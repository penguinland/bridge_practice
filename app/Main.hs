import System.Random(mkStdGen)

--import qualified Topics.JacobyTransfers as JacobyTransfers
--import qualified Topics.MinorTransfersScott as MinorTransfers
--import qualified Topics.PrecisionOpeners as PrecisionOpeners
--import qualified Topics.StandardModernPrecision.TwoDiamondOpeners as TwoDOpen
--import qualified Topics.StandardOpeners as StandardOpeners
import qualified Topics.StandardModernPrecision.OpeningBids as SmpOpenings
import qualified Topics.StandardModernPrecision.ResponsesToStrongClub as Smp1CResponses
import qualified Topics.StandardModernPrecision.Mafia as Mafia
import ProblemSet(outputLatex)


main :: IO ()
main = let
    topics = [ Mafia.topic
             --, SmpOpenings.topic
             --, Smp1CResponses.topic
             ]
  in do
    outputLatex 100 topics "test" (mkStdGen 0)
    return ()
