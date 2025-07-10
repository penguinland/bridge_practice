module ProblemSet(
  generate
, outputLatex
) where

import Control.Monad(when)
import Control.Monad.Trans.Class(lift)
import Control.Monad.Trans.State.Strict(get, StateT, mapStateT)
import Data.Functor.Identity(runIdentity)
import Data.List.Utils(join, replace)
import System.IO(hFlush, stdout)
import System.Random(StdGen)

import Output(toLatex)
import Random(pickItem)
import Situation(Situation(..))
import SituationInstance(instantiate, SituationInstance)
import Topic(Topic, topicName, refName, choose)


-- TODO: find a more compact way to `show g`.
reference :: String -> String -> StdGen -> String
reference topic sit g = topic ++ "." ++ sit ++ " " ++ show g


generate :: Int -> [Topic] -> StateT StdGen IO [SituationInstance]
generate 0 _      = return []
generate n topics = do
    topic <- pickItem topics
    gen <- get
    -- We use mapStateT to convert from a `State StdGen Situation` to a `StateT
    -- StdGen IO Situation`. This lets us keep the IO monad out of the rest of
    -- the code.
    situation <- mapStateT (return . runIdentity) $ choose topic
    let ref = reference (refName topic) (sitRef situation) gen
    maybeSit <- instantiate ref situation
    when (n `mod` 10 == 0) (lift $ putStr "." >> hFlush stdout)
    case maybeSit of
        Nothing -> generate n topics  -- Try again
        Just d  -> do
            rest <- generate (n - 1) topics
            return $ d:rest


outputLatex :: Int -> [Topic] -> String -> StateT StdGen IO String
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
    return doc
