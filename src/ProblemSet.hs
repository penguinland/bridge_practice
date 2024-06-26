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
import Situation(sitRef)
import SituationInstance(instantiate, SituationInstance)
import Topic(Topic, topicName, refName, choose)


-- TODO: find a more compact way to `show g`.
reference :: String -> String -> StdGen -> String
reference topic sit _ = topic ++ "." ++ sit ++ " TODO"-- ++ show g


generate :: Int -> [Topic] -> StdGen -> IO ([SituationInstance], StdGen)
generate 0 _      g = return ([], g)
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
        Just d  -> do
            (rest, g'') <- generate (n - 1) topics g'
            return (d:rest, g'')


outputLatex :: Int -> [Topic] -> String -> StdGen -> IO String
outputLatex numHands topics filename g = do
    (problems, _) <- generate numHands topics g
    let topicNames = join ", " . sort . map (toLatex . topicName) $ topics
        problemSet = join "\n" . map toLatex $ problems
    template <- readFile "template.tex"
    let doc = replace "%<TOPICS>" topicNames .
              replace "%<PROBLEMS>" problemSet $ template
    writeFile (filename ++ ".tex") doc
    return doc
