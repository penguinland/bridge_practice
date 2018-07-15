module ProblemSet(
  generate
, outputLatex
) where

import Control.Monad.Trans.State.Strict(runState, get)
import Data.List(sort)
import Data.List.Utils(join, replace)
import System.IO(readFile, writeFile)
import System.Random(StdGen)

import Output(toLatex)
import Random(pickItem)
import Situation(instantiate, SituationInstance)
import Topic(Topic, topicName, choose)


generate :: Int -> [Topic] -> StdGen -> IO [SituationInstance]
generate 0 _      _ = return []
generate n topics g = let
    makeInst = do
        topic <- pickItem topics
        gen <- get
        situation <- choose topic
        instantiate gen situation
    (sitInst, g') = runState makeInst g
  in do
    maybeSit <- sitInst
    case maybeSit of
        Nothing -> generate n topics g'  -- Try again
        Just d  -> (d:) <$> generate (n - 1) topics g'


outputLatex :: Int -> [Topic] -> String -> StdGen -> IO String
outputLatex numHands topics filename g = do
    problems <- generate numHands topics g
    let topicNames = join ", " . sort . map topicName  $ topics
        problemSet = join "\n" . map toLatex $ problems
    template <- readFile "template.tex"
    let doc = replace "%<TOPICS>" topicNames .
              replace "%<PROBLEMS>" problemSet $ template
    writeFile (filename ++ ".tex") doc
    return doc
