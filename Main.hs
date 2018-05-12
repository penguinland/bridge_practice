import Data.Maybe
import Data.List.Utils

import Structures(Hand(..), Bidding, startBidding, (>-))
import Output(toLatex)
import qualified Terminology
import Dealer

main :: IO ()
--main = putStrLn . toLatex $ Hand "A K Q" "J 10 9" "8 7 6" "5 4 3 2"
--main = putStrLn . toLatex $ Terminology.Spades
--main = putStrLn . toLatex $ Bidding Terminology.Neither Terminology.North [[Nothing, Just Terminology.Double], [Just Terminology.Redouble]]
{-
main = putStrLn . toLatex $ b
  where
    b = startBidding Terminology.West >- Terminology.Pass
        >- Terminology.Bid 1 Terminology.Spades >- Terminology.Double
        >- Terminology.Redouble >- Terminology.Bid 2 Terminology.Clubs
-}
main = do
  let deal = addNewReq "one" "1" newDeal
  maybeDeal <- eval Terminology.North Terminology.Both deal 0
  case maybeDeal of
    Nothing -> putStrLn "unknown"
    Just d -> putStrLn . toLatex $ d
