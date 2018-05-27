import Data.Maybe
import Data.List.Utils
--import Control.Monad.Trans.State.Strict(execState)

import Structures(Hand(..), Bidding, startBidding, (>-))
import Output(toLatex)
import qualified Terminology as T
import DealerProg
import Auction
import Situation

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
    scenario = strong1NT >> makePass >> jacobyTransfer T.Spades >> makePass
    --(bidding, deal) = finish T.South situation
    --maybeDeal = eval T.South T.Both deal 0
    problem = situation T.South T.Both scenario (T.Bid 2 T.Spades)
        "It's a Jacoby transfer."
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
