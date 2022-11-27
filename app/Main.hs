import System.Random(mkStdGen)

--import qualified Topics.JacobyTransfers as JacobyTransfers
--import qualified Topics.MinorTransfersScott as MinorTransfers
--import qualified Topics.StandardOpeners as StandardOpeners

import qualified Topics.StandardModernPrecision.OpeningBids as SmpOpenings
import qualified Topics.StandardModernPrecision.ResponsesToStrongClub as Smp1CResponses
import qualified Topics.StandardModernPrecision.Mafia as Mafia
import qualified Topics.StandardModernPrecision.MafiaResponses as MafiaResponses
import qualified Topics.StandardModernPrecision.TwoDiamondOpeners as TwoDOpen

import ProblemSet(outputLatex)


main :: IO ()
main = let
    topics = [ SmpOpenings.topic
             , Smp1CResponses.topic
             , Mafia.topic
             , MafiaResponses.topic
             , TwoDOpen.topic
             ]
  in do
    outputLatex 100 topics "test" (mkStdGen 0)
    return ()
