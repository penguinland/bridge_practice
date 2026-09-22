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
, predealLength
, predealCard
, withholdBid
, extractLastCall
) where

import Control.Monad.Trans.State.Strict(State, execState, get, put)
import Data.List.Utils(join)
import Data.Tuple.Extra(fst3)

import Bidding(Bidding, startBidding, lastCall, currentBidder)
import DealerProg(DealerProg, addNewReq, addDefn, Predeal(..), addNewPredeal)
import Output(Showable(..))
import qualified Terminology as T


type Auction = (Bidding, DealerProg, T.Vulnerability)


newAuction :: T.Vulnerability -> T.Direction -> Auction
newAuction vul dealer = (startBidding dealer, mempty, vul)


type Action = State Auction ()

instance Showable Action where
    toLatex     = toLatex     . T.removeAlert . extractLastCall
    toHtml      = toHtml      . T.removeAlert . extractLastCall
    toMonospace = toMonospace . T.removeAlert . extractLastCall

-- AWKWARD TRICK ALERT: Unless you specify the types explicitly, options passed
-- to `<~` come out as the more general `State Auction a` instead of the more
-- specific `Action`. In order to use `suitBid` with them, we need this instance
-- (and all functions it uses, including `extractLastCall` and `finish`) to have
-- this more general type.
-- TODO: There's probably some way to convince the type checker to generalize
-- less, which possibly involves changing `Action` from a `type` to something
-- else (`newtype`?). See if you can figure it out...
instance T.SuitBid (State Auction a) where
    suitBid = T.suitBid . extractLastCall


-- This isn't the more specific type `T.Direction -> Action -> T.CompleteCall`
-- in order to get SuitBid to work; see the "awkward trick alert" above.
finish :: T.Vulnerability -> T.Direction -> State Auction a -> Auction
finish vul dealer = flip execState (newAuction vul dealer)


constrain :: String -> [String] -> Action
define    :: String -> [String] -> Action
(constrain, define) = let
    -- The helper takes the name of a constraint and pieces of a definition
    -- that should be joined together with the name of the bidder.
    -- TODO: consider making the pieces a String -> String function instead?
    helper fn name defnPieces = do
        (bidding, dealerProg, vul) <- get
        let bidderName = show . currentBidder $ bidding
            fullName = name ++ "_" ++ bidderName
            fullDefn = join bidderName defnPieces
        put (bidding, fn fullName fullDefn dealerProg, vul)
  in (helper addNewReq, helper addDefn)


predealLength :: T.Suit -> Int -> Action
predealCard :: T.Suit -> Char -> Action
(predealLength, predealCard) = let
    helper fn suit val = do
        (bidding, dealerProg, vul) <- get
        let pd = fn suit (currentBidder bidding) val
        put (bidding, addNewPredeal pd dealerProg, vul)
  in (helper PredealLength, helper PredealCard)


-- Add the constraints in this action without modifying the current Bidding.
withholdBid :: Action -> Action
withholdBid action = do
    (bidding, dealerProg, vul) <- get
    let freshAuction = newAuction vul . currentBidder $ bidding
        (_, dealerToWithhold, _) = execState action freshAuction
    put (bidding, dealerProg <> dealerToWithhold, vul)


-- This isn't the more specific type `Action -> T.CompleteCall` in order to get
-- SuitBid to work; see the "awkward trick alert" above.
extractLastCall :: State Auction a -> T.CompleteCall
extractLastCall =
    -- It doesn't matter who was dealer or what the vulnerability was: use dummy
    -- values just to extract the bidding from the action.
    lastCall . fst3 . finish T.None T.North
