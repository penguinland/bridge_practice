module Auction (
  Auction
, Action
, newAuction
, finish
, forbid
, alternatives
, constrain
, define
, makeCall
, makeAlertableCall
, makePass
, pointRange
, balancedHand
, suitLength
, minSuitLength
, maxSuitLength
, hasTopN
, withholdBid
, compareSuitLength
, SuitLengthComparator(..)
) where

import Control.Monad.Trans.State.Strict(State, execState, get, put, modify)
import Data.Bifunctor(first)
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


alternatives :: [Action] -> Action
-- We use deMorgan's laws. (A || B || C) becomes !(!A && !B && !C)
alternatives = forbid . sequence_ . map forbid


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
makeCall call = modify $ first (>- T.CompleteCall call T.Unalerted)

makeAlertableCall :: T.Call -> String -> Action
makeAlertableCall call alert = do
    (bidding, dealerProg) <- get
    let bidder = currentBidder bidding
    put (bidding >- T.CompleteCall call (T.alertFor bidder alert), dealerProg)


makePass :: Action
makePass = makeCall T.Pass


balancedHand :: Action
balancedHand =
    constrain "balanced" ["shape(", ", any 4333 + any 5332 + any 4432)"]


pointRange :: Int -> Int -> Action
pointRange minHcp maxHcp =
    constrain (join "_" ["range", show minHcp, show maxHcp])
              ["hcp(", ") >= " ++ show minHcp ++ " && " ++
               "hcp(", ") <= " ++ show maxHcp]


suitLengthOp :: String -> String -> T.Suit -> Int -> Action
suitLengthOp op suffix suit len =
    constrain (join "_" [show suit, suffix, show len])
              [show suit ++ "(", ") " ++ op ++ " " ++ show len]

suitLength :: T.Suit -> Int -> Action
suitLength = suitLengthOp "==" "eq"

minSuitLength :: T.Suit -> Int -> Action
minSuitLength = suitLengthOp ">=" "ge"

maxSuitLength :: T.Suit -> Int -> Action
maxSuitLength = suitLengthOp "<=" "le"


hasTopN :: T.Suit -> Int -> Int -> Action
hasTopN suit range minCount = do
    constrain (join "_" [show suit, show minCount, "of", "top", show range])
              ["top" ++ show range ++ "(", ", " ++ show suit ++
               ") >= " ++ show minCount]


-- Define the constraints in this action without modifying the current Auction.
withholdBid :: Action -> Action
withholdBid action = do
    (bidding, dealerProg) <- get
    let freshAuction = newAuction . currentBidder $ bidding
        (_, dealerToWithhold) = execState action freshAuction
    put (bidding, dealerProg `mappend` dealerToWithhold)


data SuitLengthComparator = Longer | Equal | Shorter
instance Show SuitLengthComparator where
    show Longer = ">"
    show Equal = "=="
    show Shorter = "<"

compareSuitLength :: T.Suit -> SuitLengthComparator -> T.Suit -> Action
compareSuitLength suitA op suitB = let
    toWord Longer = "longer"
    toWord Equal = "equal"
    toWord Shorter = "shorter"
    name = join "_" [show suitA, toWord op, show suitB, "length"]
  in
    constrain name [show suitA ++ "(", ") " ++ show op ++ " " ++
                    show suitB ++ "(", ")"]

-- TODO: hasCard
