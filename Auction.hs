module Auction (
  Auction
, Action
, newAuction
, (&>)
, forbid
, makeCall
, makePass
, strong1NT
, texasTransfer
, jacobyTransfer
) where

import Control.Monad.Trans.State.Strict(State, state, evalState, get, put)
import Data.Bifunctor(first, second)
import Data.List.Utils(join)

import Dealer(Dealer, newDeal, addNewReq, forbidNewReq, invert)
import Structures(Bidding, startBidding, (>-), currentBidder)
import qualified Terminology as T

-- The Bool is whether subsequent actions are to be taken (True) or forbidden
-- (False).
type Auction = (Bidding, Dealer)
type Action = Auction -> Auction

{-
-- This doesn't work because type synonyms like Action cannot be part of type
-- classes without using the TypeSynonymInstances extension.
instance Monoid Action where
    mempty = id
    mappend = flip (.)
-}

(&>) :: Action -> Action -> Action
(&>) = flip (.)  -- Think of this as mappend


-- ERROR: this doesn't forbid groups of things together. Example: if Jacoby
-- Transfers need to forbid Texas Transfers, we need to forbid having both a
-- 6-card suit and game-going strength at the same time, even though a 6 card
-- suit on its own is fine and game-going strength on its own is also fine.
forbid :: Action -> Action
forbid action (bidding, dealer) = let
    (_, dealerToInvert) = action . newAuction . currentBidder $ bidding
  in
    (bidding, dealer `mappend` invert dealerToInvert)


newAuction :: T.Direction -> Auction
newAuction dealer = (startBidding dealer, newDeal)


-- constrain takes the name of a constraint and pieces of a definition that
-- should be joined together with the name of the bidder.
-- TODO: consider making the pieces a String -> String function instead?
constrain :: String -> [String] -> Action
constrain name defnPieces (bidding, dealer) =
  let
    toName T.North = "north"
    toName T.East  = "east"
    toName T.South = "south"
    toName T.West  = "west"
    bidderName = toName . currentBidder $ bidding
    fullName = name ++ "_" ++ bidderName
    fullDefn = join bidderName defnPieces
  in
    (bidding, addNewReq fullName fullDefn dealer)


makeCall :: T.Call -> Action
makeCall call = first (>- call)


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
suitLengthOp op suffix suit length = let
    toName T.Spades   = "spades"
    toName T.Hearts   = "hearts"
    toName T.Diamonds = "diamonds"
    toName T.Clubs    = "clubs"
    suitName = toName suit
  in
    constrain (join "_" [suitName, suffix, show length])
              [suitName ++ "(", ") " ++ op ++ " " ++ show length]

suitLengthEq = suitLengthOp "==" "eq"

suitLengthMin = suitLengthOp ">=" "gt"


strong1NT :: Action
strong1NT = balancedHand &> pointRange 15 17 &> makeCall (T.Bid 1 T.Notrump)


texasTransfer :: T.Suit -> Action
texasTransfer suit =
  let
    transferSuit T.Spades = T.Hearts
    transferSuit T.Hearts = T.Diamonds
    transferSuit _        = error "Texas transfer of non-major!"
    suitName T.Spades = "spades"
    suitName T.Hearts = "hearts"
    suitName _        = error "Texas transfer of non-major!"
  in
    suitLengthMin suit 6 &> pointRange 10 15 &>
    makeCall (T.Bid 4 $ transferSuit suit)


jacobyTransfer :: T.Suit -> Action
jacobyTransfer suit =
  let
    transferSuit T.Spades = T.Hearts
    transferSuit T.Hearts = T.Diamonds
    transferSuit _        = error "Jacoby transfer of non-major!"
    suitName T.Spades = "spades"
    suitName T.Hearts = "hearts"
    suitName _        = error "Jacoby transfer of non-major!"
  in
    suitLengthMin suit 5 &> forbid (texasTransfer suit) &>
    makeCall (T.Bid 2 $ transferSuit suit)
