module Auction (
  Auction
, Action
, newAuction
, finish
, forbid
, constrain
, define
, makeCall
, makePass
, pointRange
, balancedHand
, suitLength
, minSuitLength
, maxSuitLength
, hasTopN
, withholdBid
) where

import Control.Monad.Trans.State.Strict(State, execState, get, put, modify)
import Data.Bifunctor(first, second)
import Data.List.Utils(join)

import DealerProg(DealerProg, newDeal, addNewReq, addDefn, invert)
import Structures(Bidding, startBidding, (>-), currentBidder)
import qualified Terminology as T

type Auction = (Bidding, DealerProg)
type Action = State Auction ()


newAuction :: T.Direction -> Auction
newAuction dealer = (startBidding dealer, newDeal)


finish :: T.Direction -> Action -> Auction
finish firstBidder = flip execState (newAuction firstBidder)


forbid :: Action -> Action
forbid action = do
    (bidding, dealerProg) <- get
    let freshAuction = newAuction . currentBidder $ bidding
        (_, dealerToInvert) = execState action freshAuction
    put (bidding, dealerProg `mappend` invert dealerToInvert)


-- modifyDealerProg takes the name of a constraint and pieces of a definition
-- that should be joined together with the name of the bidder.
-- TODO: consider making the pieces a String -> String function instead?
modifyDealerProg :: (String -> String -> DealerProg -> DealerProg) ->
        String -> [String] -> Action
modifyDealerProg op name defnPieces = do
    (bidding, dealerProg) <- get
    let bidderName = show . currentBidder $ bidding
        fullName = name ++ "_" ++ bidderName
        fullDefn = join bidderName defnPieces
    put (bidding, op fullName fullDefn dealerProg)

constrain :: String -> [String] -> Action
constrain = modifyDealerProg addNewReq

define :: String -> [String] -> Action
define = modifyDealerProg addDefn


makeCall :: T.Call -> Action
makeCall call = modify $ first (>- call)


makePass :: Action
makePass = makeCall T.Pass


balancedHand :: Action
balancedHand =
    constrain "balanced" ["shape(", ", any 4333 + any 5332 + any 4432)"]


pointRange :: Int -> Int -> Action
pointRange min max =
    constrain (join "_" ["range", show min, show max])
              ["hcp(", ") >= " ++ show min ++ " && hcp(", ") <= " ++ show max]


suitLengthOp :: String -> String -> T.Suit -> Int -> Action
suitLengthOp op suffix suit length =
    constrain (join "_" [show suit, suffix, show length])
              [show suit ++ "(", ") " ++ op ++ " " ++ show length]

suitLength = suitLengthOp "==" "eq"

minSuitLength = suitLengthOp ">=" "ge"

maxSuitLength = suitLengthOp "<=" "le"


hasTopN :: T.Suit -> Int -> Int -> Action
hasTopN suit range minCount = do
    constrain (join "_" [show suit, show minCount, "of", "top", show range])
              ["top" ++ show range ++ "(", ", " ++ show suit ++
               ") >= " ++ show minCount]


withholdBid :: Action -> Action
withholdBid action = do
    (bidding, dealerProg) <- get
    let freshAuction = newAuction . currentBidder $ bidding
        (_, dealerToWithhold) = execState action freshAuction
    put (bidding, dealerProg `mappend` dealerToWithhold)

-- TODO: hasCard
