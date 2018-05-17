import Data.Maybe
import Data.List.Utils

import Structures(Hand(..), Bidding, startBidding, (>-))
import Output(toLatex)
import qualified Terminology as T
import Dealer
import Auction

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
main = let
    situation = strong1NT &> makePass &> jacobyTransfer T.Spades
    ((bidding, deal), _) = situation $ newAuction T.East
    maybeDeal = eval T.East T.Both deal 0
  in
    (putStrLn . toLatex $ bidding) >> putStrLn "" >> maybeDeal >>=
    (\deal -> case deal of
        Nothing -> putStrLn "unknown"
        Just d -> putStrLn . toLatex $ d)
