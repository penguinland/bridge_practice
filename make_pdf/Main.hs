import Control.Monad.Trans.State.Strict(runStateT)
import System.Random(mkStdGen)

import qualified Topics.JacobyTransfers as JacobyTransfers
import qualified Topics.Stayman as Stayman
--import qualified Topics.MinorTransfersScott as MinorTransfers
import qualified Topics.StandardOpeners as StandardOpeners
import qualified Topics.TexasTransfers as TexasTransfers

import qualified Topics.StandardModernPrecision.OpeningBids as SmpOpenings
import qualified Topics.StandardModernPrecision.OneClubResponses as Smp1CResponses
import qualified Topics.StandardModernPrecision.OneDiamondResponses as Smp1DResponses
import qualified Topics.StandardModernPrecision.Lampe as Lampe
import qualified Topics.StandardModernPrecision.Mafia as Mafia
import qualified Topics.StandardModernPrecision.TwoDiamondOpeners as Smp2DOpen
import qualified Topics.StandardModernPrecision.MafiaResponses as MafiaResponses
import qualified Topics.StandardModernPrecision.TripleFourOne as TripleFourOne
import qualified Topics.MajorSuitRaises as MajorSuitRaises
import qualified Topics.Meckwell as Meckwell
import qualified Topics.Cappelletti as Cappelletti
import qualified Topics.Jacoby2NT as Jacoby2NT
import qualified Topics.ForcingOneNotrump as ForcingOneNotrump

import ProblemSet(outputLatex)


main :: IO ()
main = let
    _topics = [ StandardOpeners.topic
              , ForcingOneNotrump.topic
              , MajorSuitRaises.topic
              , JacobyTransfers.topic
              , Stayman.topic
              , TexasTransfers.topic
              , SmpOpenings.topic
              , Smp1CResponses.topic
              , Smp1CResponses.topicExtras
              , Mafia.topic
              , MafiaResponses.topic
              , Smp1DResponses.topic
              , Smp2DOpen.topic
              , Meckwell.topic
              , Cappelletti.topic
              , Jacoby2NT.topic
              , Lampe.topic
              , TripleFourOne.topic
              ]
    topics = [Cappelletti.topic]
  in do
    -- outputLatex returns a copy of the contents of the file it wrote, but we
    -- ignore that.
    _ <- runStateT (outputLatex 100 topics "test") (mkStdGen 0)
    return ()
