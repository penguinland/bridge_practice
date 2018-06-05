module ProblemSet(generate) where

import System.Random(StdGen, next, split, randomR)

import Situation(instantiate, SituationInstance)
import Topic(Topic, choose)


-- TODO: move this somewhere more universal
pickItem :: StdGen -> [a] -> (a, StdGen)
pickItem g [] = error "Picked item from empty list"
pickItem g as = let
    (n, g') = randomR (0, length as - 1) g
  in
    (as !! n, g')

generate :: Int -> [Topic] -> StdGen -> IO [SituationInstance]
generate 0 _      _ = return []
generate n topics g = let
    (topic, g') = pickItem g topics
    (g'', g''') = split g'
    sit = choose topic g''
    (g4, g5) = split g'''
    sitInst = instantiate sit g4
  in do
    maybeSit <- sitInst
    case maybeSit of
        Nothing -> generate n topics g5  -- Try again
        Just d -> generate (n - 1) topics g5 >>= (return . (d :))
