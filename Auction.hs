module Auction (
  Auction
, Action
, newAuction
, (&>)
, rawCall
, rawPass
, strong1NT
, jacobyTransfer
) where

import Data.Bifunctor(first, second)

import Dealer(Dealer, newDeal, addNewReq)
import Structures(Bidding, startBidding, (>-), currentBidder)
import qualified Terminology as T

type Auction = (Bidding, Dealer)
type Action = String -> Auction -> Auction


newAuction :: T.Direction -> Auction
newAuction dealer = (startBidding dealer, newDeal)


auction &> action = let
    toName T.North = "north"
    toName T.East  = "east"
    toName T.South = "south"
    toName T.West  = "west"
  in
    action (toName . currentBidder . fst $ auction) auction


rawCall :: T.Call -> Action  -- No constraints on the deal
rawCall c _ = first (>- c)

rawPass :: Action
rawPass = rawCall T.Pass


strong1NT :: Action
strong1NT caller =
    first (>- T.Bid 1 T.Notrump) .
    second (addNewReq (caller ++ "_balanced")
                       ("shape(" ++ caller ++
                        ", any 4333 + any 5332 + any 4432)")) .
    second (addNewReq (caller ++ "_strong1nt_strength")
                      ("hcp(" ++ caller ++ ") >= 15 && hcp(" ++
                        caller ++ ") <= 17"))

jacobyTransfer :: T.Suit -> Action
jacobyTransfer suit caller =
  let
    transferSuit T.Spades = T.Hearts
    transferSuit T.Hearts = T.Diamonds
    transferSuit _        = error "Jacoby transfer of non-major!"
    suitName T.Spades = "spades"
    suitName T.Hearts = "hearts"
    suitName _        = error "Jacoby transfer of non-major!"
  in
    first (>- (T.Bid 2 $ transferSuit suit)) .
    second (addNewReq (caller ++ "_jacoby_transfer_" ++ suitName suit)
                      (suitName suit ++ "(" ++ caller ++ ") >= 5"))
