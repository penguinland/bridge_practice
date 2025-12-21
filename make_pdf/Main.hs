import Control.Monad.Trans.Class(lift)
import Control.Monad.Trans.State.Strict(runStateT)
import Data.List.Utils(join, replace)
import System.Random(mkStdGen)

import qualified Topics.JacobyTransfers as JacobyTransfers
import qualified Topics.Stayman as Stayman
--import qualified Topics.MinorTransfersScott as MinorTransfers
import qualified Topics.StandardOpeners as StandardOpeners
import qualified Topics.TexasTransfers as TexasTransfers
import qualified Topics.Lebensohl as Lebensohl

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
import qualified Topics.DONT as DONT
import qualified Topics.Cappelletti as Cappelletti
import qualified Topics.Jacoby2NT as Jacoby2NT
import qualified Topics.ForcingOneNotrump as ForcingOneNotrump
import qualified Topics.PuppetStayman as PuppetStayman
import qualified Topics.MuppetStayman as MuppetStayman
import qualified Topics.TransfersOver1MX as TransfersOver1MX
import qualified Topics.RomanKeycardBlackwood as RKC

import Output(toLatex)
import ProblemSet(generate)
import Topic(Topic, topicName)
import Types(StIO)


outputLatex :: Int -> [Topic] -> String -> StIO ()
outputLatex numHands topics filename = do
    problems <- generate numHands topics
    let topicNames = join ", " . map (toLatex . topicName) $ topics
        problemSet = unlines . map toLatex $ problems
    template <- lift $ readFile "template.tex"
    let doc = replace "%<TOPICS>" topicNames .
              replace "%<PROBLEMS>" problemSet $ template
    let fullFilename = filename ++ ".tex"
    lift $ writeFile fullFilename doc
    lift $ putStrLn ("Output written to " ++ fullFilename)


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
              , DONT.topic
              , Cappelletti.topic
              , Lebensohl.topic
              , Jacoby2NT.topic
              , Lampe.topic
              , TripleFourOne.topic
              , PuppetStayman.topic
              , MuppetStayman.topic
              , TransfersOver1MX.topic
              , RKC.topic1430Common
              ]
    topics = [ RKC.topic1430Common
             ]
  in do
    runStateT (outputLatex 100 topics "test") (mkStdGen 0) >>= return . fst
