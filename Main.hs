import Data.Maybe
import Data.List.Utils
--import Control.Monad.Trans.State.Strict(execState)
import System.Random(mkStdGen, StdGen)

import Structures(Hand(..), Bidding, startBidding, (>-))
import Output(toLatex, Showable, OutputType(..), output)
import qualified Terminology as T
import DealerProg
import Auction
import Situation
import Topic(wrap, choose, (<~), base, option, Topic(..), Situations)
import qualified JacobyTransfers

main :: IO ()
--main = putStrLn . toLatex $ Hand "A K Q" "J 10 9" "8 7 6" "5 4 3 2"
--main = putStrLn . toLatex $ T.Spades
--main = putStrLn . toLatex $ Bidding T.Neither T.North [[Nothing, Just T.Double], [Just T.Redouble]]
{-
main = putStrLn . toLatex $ b
  where
    b = startBidding T.West >- T.Pass
        >- T.Bid 1 T.Spades >- T.Double
        >- T.Redouble >- T.Bid 2 T.Clubs
-}
{-
main = do
  let deal = addNewReq "one" "1" newDeal
  maybeDeal <- eval T.North T.Both deal 0
  case maybeDeal of
    Nothing -> putStrLn "unknown"
    Just d -> putStrLn . toLatex $ d
-}
--main = putStrLn . toLatex . fst . newAuction $ T.South
main = let
    --scenario = strong1NT >> makePass >> jacobyTransfer T.Spades >> makePass
    --(bidding, deal) = finish T.South situation
    --maybeDeal = eval T.South T.Both deal 0
    --problem' = situation T.South T.Both scenario (T.Bid 2 T.Spades)
    --    "It's a Jacoby transfer."
    --topic :: Situations
    --topic = sitFun (\_ -> sitList [rawSit problem'])

    problemMaker suit = let
        scenario = strong1NT >> makePass >> jacobyTransfer suit >> makePass
      in
        situation T.South T.Both scenario (T.Bid 2 suit)
            (const "It's still a Jacoby transfer.")
    topic' = base problemMaker <~ option [T.Spades, T.Hearts]
    -- Note that the call to `wrap` in here is optional! It's idempotent.
    topic'' :: StdGen -> Situations --[StdGen -> Situation]
    topic'' _ = wrap [topic']
    topic = Topic "Jacoby transfers" (wrap topic'')
    --problem = choose (situations topic) (mkStdGen 0)
    problem = choose (situations JacobyTransfers.topic) (mkStdGen 2)
    tryShow :: OutputType -> IO ()
    tryShow f = putStrLn $ output f T.Pass ++ output f T.Hearts
  in do
    --putStrLn . toLatex $ bidding
    --putStrLn ""
    --putStrLn . toProgram $ deal
    --putStrLn ""
    {-
    deal <- maybeDeal
    case deal of
        Nothing -> putStrLn "invalid deal"
        Just d -> putStrLn . toLatex $ d
    putStrLn ""
    -}
    maybeSitInst <- instantiate problem 0
    case maybeSitInst of
        Nothing -> putStrLn "invalid problem"
        Just s -> putStrLn . toLatex $ s
    putStrLn ""
    tryShow LaTeX
