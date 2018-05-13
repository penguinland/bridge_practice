module Auction (
  Bids
, Action
, newAuction
, (&>)
, rawCall
, rawPass
, strong1NT
) where

import Data.Bifunctor

import Dealer(Dealer, newDeal, addNewReq)
import Structures(Bidding, startBidding, (>-), currentBidder)
import qualified Terminology as T

type Bids = (Bidding, Dealer)

type Action = String -> Bids -> Bids


newAuction :: T.Direction -> Bids
newAuction dealer = (startBidding dealer, newDeal)


bids &> action = let
    toName T.North = "north"
    toName T.East  = "east"
    toName T.South = "south"
    toName T.West  = "west"
  in
    action (toName . currentBidder . fst $ bids) bids


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
