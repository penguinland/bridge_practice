module ProblemSet(
  generate
, outputLatex
) where

import Control.Monad.Trans.State.Strict(State, runState, get, put)
import Data.List(sort)
import Data.List.Utils(join, replace)
import System.IO(readFile, writeFile)
import System.Random(StdGen, next, split, randomR)

import Output(toLatex)
import Situation(instantiate, SituationInstance)
import Topic(Topic, topicName, choose)


-- TODO: move this somewhere more universal
pickItem :: [a] -> State StdGen a
pickItem [] = error "Picked item from empty list"
pickItem as = do
    g <- get
    let (n, g') = randomR (0, length as - 1) g
    put g'
    return (as !! n)


generate :: Int -> [Topic] -> StdGen -> IO [SituationInstance]
generate 0 _      _ = return []
generate n topics g = let
    (topic, g') = runState (pickItem topics) g
    (sit, g'') = choose topic g'
    (g3, g4) = split g''
    sitInst = instantiate sit g3
  in do
    maybeSit <- sitInst
    case maybeSit of
        Nothing -> generate n topics g4  -- Try again
        Just d -> generate (n - 1) topics g4 >>= (return . (d :))


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
