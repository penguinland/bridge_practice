module ProblemSet(
  generate
, outputLatex
) where

import Control.Monad.Trans.State.Strict(runState, get)
import Data.List(sort)
import Data.List.Utils(join, replace)
import System.Random(StdGen)

import Output(toLatex)
import Random(pickItem)
import SituationInstance(instantiate, sitRef, SituationInstance)
import Topic(Topic, topicName, refName, choose)


reference :: String -> String -> StdGen -> String
reference topic sit g = topic ++ "." ++ sit ++ " " ++ (show g)


generate :: Int -> [Topic] -> StdGen -> IO [SituationInstance]
generate 0 _      _ = return []
generate n topics g = let
    makeInst = do
        topic <- pickItem topics
        gen <- get
        situation <- choose topic
        let ref = reference (refName topic) (sitRef situation) gen
        instantiate ref situation
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
