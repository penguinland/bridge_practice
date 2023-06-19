import System.Random(mkStdGen)

--import qualified Topics.JacobyTransfers as JacobyTransfers
--import qualified Topics.MinorTransfersScott as MinorTransfers
--import qualified Topics.StandardOpeners as StandardOpeners
import qualified Topics.OneNotrump as OneNotrump

{-
import qualified Topics.StandardModernPrecision.OpeningBids as SmpOpenings
import qualified Topics.StandardModernPrecision.OneClubResponses as Smp1CResponses
import qualified Topics.StandardModernPrecision.OneDiamondResponses as Smp1DResponses
import qualified Topics.StandardModernPrecision.Mafia as Mafia
import qualified Topics.StandardModernPrecision.MafiaResponses as MafiaResponses
import qualified Topics.StandardModernPrecision.TwoDiamondOpeners as Smp2DOpen
-}

import ProblemSet(outputLatex)


main :: IO ()
main = let
    topics = [ OneNotrump.texasTransfers
               --StandardOpeners.topic
             --SmpOpenings.topic
             --, Smp1CResponses.topic
              --Smp1CResponses.topicExtras
             --, Mafia.topic
             --, MafiaResponses.topic
             --, Smp1DResponses.topic
             --, Smp2DOpen.topic
             ]
  in do
    outputLatex 100 topics "test" (mkStdGen 0)
    return ()
