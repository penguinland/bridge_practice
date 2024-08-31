-- We want to put Action in the Showable typeclass, but it's an alias for a
-- particular State monad, and the default compiler doesn't let that be in a
-- typeclass because not all its arguments are type variables.
{-# LANGUAGE FlexibleInstances #-}
-- Putting Action in the Showable typeclass gives warnings about an orphan
-- instance, because this file is neither the place where Showable is defined,
-- nor the place where State is defined. but it *is* the place where Action is
-- defined, so suppress the warning.
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Action (
  Auction
, Action
, newAuction
, finish
, constrain
, define
, withholdBid
, extractLastCall
) where

import Control.Monad.Trans.State.Strict(State, execState, get, put)
import Data.List.Utils(join)

import DealerProg(DealerProg, addNewReq, addDefn)
import Output(Showable(..))
import Structures(Bidding, startBidding, lastCall, currentBidder)
import qualified Terminology as T


type Auction = (Bidding, DealerProg)


newAuction :: T.Direction -> Auction
newAuction dealer = (startBidding dealer, mempty)


type Action = State Auction ()

instance Showable Action where
    toLatex = toLatex . T.removeAlert . extractLastCall
    toHtml = toHtml . T.removeAlert . extractLastCall

instance T.SuitBid Action where
    suitBid = T.suitBid . extractLastCall


finish :: T.Direction -> Action -> Auction
finish firstBidder = flip execState (newAuction firstBidder)


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


-- Define the constraints in this action without modifying the current Auction.
withholdBid :: Action -> Action
withholdBid action = do
    (bidding, dealerProg) <- get
    let freshAuction = newAuction . currentBidder $ bidding
        (_, dealerToWithhold) = execState action freshAuction
    put (bidding, dealerProg `mappend` dealerToWithhold)


extractLastCall :: Action -> T.CompleteCall
extractLastCall =
    -- It doesn't matter who was dealer: use North just to extract the bidding
    -- from the action.
    lastCall . fst . finish T.North
