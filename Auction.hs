module Auction (
  Auction
, Action
, newAuction
, finish
, forbid
, makeCall
, makePass
, strong1NT
, texasTransfer
, jacobyTransfer
) where

import Control.Monad.Trans.State.Strict(State, execState, get, put, modify)
import Data.Bifunctor(first, second)
import Data.List.Utils(join)

import DealerProg(DealerProg, newDeal, addNewReq, forbidNewReq, invert)
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


-- constrain takes the name of a constraint and pieces of a definition that
-- should be joined together with the name of the bidder.
-- TODO: consider making the pieces a String -> String function instead?
constrain :: String -> [String] -> Action
constrain name defnPieces = do
    (bidding, dealerProg) <- get
    let bidderName = show . currentBidder $ bidding
        fullName = name ++ "_" ++ bidderName
        fullDefn = join bidderName defnPieces
    put (bidding, addNewReq fullName fullDefn dealerProg)


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


strong1NT :: Action
strong1NT = do
    balancedHand
    pointRange 15 17
    makeCall $ T.Bid 1 T.Notrump


majorTransferSuit :: T.Suit -> T.Suit
majorTransferSuit T.Spades = T.Hearts
majorTransferSuit T.Hearts = T.Diamonds
majorTransferSuit _        = error "Major transfer of non-major!"


texasTransfer :: T.Suit -> Action
texasTransfer suit = do
    minSuitLength suit 6
    pointRange 10 15
    makeCall (T.Bid 4 $ majorTransferSuit suit)


jacobyTransfer :: T.Suit -> Action
jacobyTransfer suit = do
    minSuitLength suit 5
    forbid (texasTransfer suit)
    makeCall (T.Bid 2 $ majorTransferSuit suit)
