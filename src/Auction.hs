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
, longerThan
, shorterThan
, equalLength
, atLeastAsLong
, atMostAsLong
, extractLastCall
, displayLastCall
) where

import Control.Monad.Trans.State.Strict(State, execState, get, put, modify)
import Data.Bifunctor(first)
import Data.List.Utils(join)

import DealerProg(DealerProg, addNewReq, addDefn, invert)
import Output(Commentary, toCommentary)
import Structures(Bidding, startBidding, (>-), lastCall, currentBidder)
import qualified Terminology as T

type Auction = (Bidding, DealerProg)
type Action = State Auction ()


newAuction :: T.Direction -> Auction
newAuction dealer = (startBidding dealer, mempty)


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
alternatives = forbid . mapM_ forbid


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
makeCall call = modify $ first (>- T.CompleteCall call Nothing)

makeAlertableCall :: T.Call -> String -> Action
makeAlertableCall call alert =
    modify $ first (>- T.CompleteCall call (Just alert))


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

_compareSuitLength :: String -> String -> T.Suit -> T.Suit -> Action
_compareSuitLength name op suitA suitB = let
    fullName = join "_" [show suitA, name, show suitB, "length"]
  in
    constrain fullName [show suitA ++ "(", ") " ++ op ++ " " ++
                        show suitB ++ "(", ")"]

longerThan :: T.Suit -> T.Suit -> Action
longerThan = _compareSuitLength "longer" ">"

shorterThan :: T.Suit -> T.Suit -> Action
shorterThan = _compareSuitLength "shorter" "<"

equalLength :: T.Suit -> T.Suit -> Action
equalLength = _compareSuitLength "equal" "=="

atLeastAsLong :: T.Suit -> T.Suit -> Action
atLeastAsLong = _compareSuitLength "ge" ">="

atMostAsLong :: T.Suit -> T.Suit -> Action
atMostAsLong = _compareSuitLength "le" "<="

extractLastCall :: Action -> T.CompleteCall
extractLastCall =
    -- It doesn't matter who was dealer: use North just to extract the bidding
    -- from the action.
    lastCall . fst . finish T.North

-- displayLastCall is for use in explanations: it formats the most recent call
-- from an action while stripping out any alerts it might have
displayLastCall :: Action -> Commentary
displayLastCall = toCommentary . T.removeAlert . extractLastCall

-- TODO: hasCard
