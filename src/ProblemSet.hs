module ProblemSet(
  generate
, outputLatex
) where

import Control.Monad(when)
import Control.Monad.Trans.Class(lift)
import Control.Monad.Trans.State.Strict(runState, get, put, StateT)
import Data.List.Utils(join, replace)
import System.IO(hFlush, stdout)
import System.Random(StdGen)

import Output(toLatex)
import Random(pickItem)
import Situation(sitRef)
import SituationInstance(instantiate, SituationInstance)
import Topic(Topic, topicName, refName, choose)


-- TODO: find a more compact way to `show g`.
reference :: String -> String -> StdGen -> String
reference topic sit g = topic ++ "." ++ sit ++ " " ++ show g


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
    when (n `mod` 10 == 0) (putStr "." >> hFlush stdout)
    maybeSit <- sitInst
    case maybeSit of
        Nothing -> generate n topics g'  -- Try again
        Just d  -> do
            (rest, g'') <- generate (n - 1) topics g'
            return (d:rest, g'')


outputLatex :: Int -> [Topic] -> String -> StateT StdGen IO String
outputLatex numHands topics filename = do
    g <- get
    (problems, g') <- lift $ generate numHands topics g
    put g'
    let topicNames = join ", " . map (toLatex . topicName) $ topics
        problemSet = join "\n" . map toLatex $ problems
    template <- lift $ readFile "template.tex"
    let doc = replace "%<TOPICS>" topicNames .
              replace "%<PROBLEMS>" problemSet $ template
    let fullFilename = filename ++ ".tex"
    lift $ writeFile fullFilename doc
    lift $ putStrLn ("Output written to " ++ fullFilename)
    return doc
