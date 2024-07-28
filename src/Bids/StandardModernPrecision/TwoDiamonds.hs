module Bids.StandardModernPrecision.TwoDiamonds(
  noDirectOvercall
, b2D  -- Re-exported from BasicBids
, b2D2H
, bP2D2H
, b2D2H2S
, b2D2S
, bP2D2S
, b2D2N
, b2D3C
, bP2D3C
, b2D3H
, b2D3S
, b2D3N
) where


import Action(Action)
import Bids.StandardModernPrecision.BasicBids(b2D, lessThanInvitational)
import CommonBids(cannotPreempt)
import EDSL(suitLength, minSuitLength, maxSuitLength, makeCall, pointRange,
            makeAlertableCall, atLeastAsLong, longerThan, forbid, forbidAll,
            balancedHand, soundHolding, makePass, alternatives, forEach)
import Output(Punct(..), (.+))
import qualified Terminology as T


noDirectOvercall :: Action
noDirectOvercall = do
    cannotPreempt
    -- Either you don't have enough strength or enough shape to overcall
    alternatives [ pointRange 0 10
                 , pointRange 11 16 >> forEach T.allSuits (`maxSuitLength` 4)
                 ]
    makePass


-- unexported helper functions
heartsSignoff :: Action
heartsSignoff = do
    lessThanInvitational
    forbid b2D3H  -- Prefer a mixed raise
    minSuitLength T.Hearts 3
    maxSuitLength T.Diamonds 6
    T.Hearts `atLeastAsLong` T.Spades
    maxSuitLength T.Clubs 4

spadesSignoff :: Action
spadesSignoff = do
    lessThanInvitational
    forbid b2D3S  -- Prefer a mixed raise
    forbid heartsSignoff
    -- With 3253 shape, I'd sign off in 3C, rather than risk a 6-card fit
    -- TODO: is the above comment right? Maybe not...
    minSuitLength T.Spades 4
    T.Spades `longerThan` T.Hearts
    maxSuitLength T.Diamonds 6
    maxSuitLength T.Clubs 4

clubsSignoff :: Action
clubsSignoff = do
    lessThanInvitational
    forbid heartsSignoff
    forbid spadesSignoff
    minSuitLength T.Clubs 3
    maxSuitLength T.Diamonds 6
    maxSuitLength T.Hearts 2
    maxSuitLength T.Spades 3


b2D2H :: Action
b2D2H = do
    heartsSignoff
    makeAlertableCall (T.Bid 2 T.Hearts) "nonforcing, likely signoff"

bP2D2H :: Action
bP2D2H = do
    heartsSignoff
    -- When you're a passed hand, bidding a new suit is always nonforcing, so
    -- that's no longer alertable.
    makeCall $ T.Bid 2 T.Hearts


b2D2H2S :: Action
b2D2H2S = do
    suitLength T.Hearts 3
    makeAlertableCall (T.Bid 2 T.Spades) "exactly 4315 shape"


b2D2S :: Action
b2D2S = do
    spadesSignoff
    makeAlertableCall (T.Bid 2 T.Spades) "nonforcing, likely signoff"

bP2D2S :: Action
bP2D2S = do
    spadesSignoff
    -- When you're a passed hand, bidding a new suit is always nonforcing, so
    -- that's no longer alertable.
    makeCall $ T.Bid 2 T.Spades


b2D3C :: Action
b2D3C = do
    clubsSignoff
    makeAlertableCall (T.Bid 3 T.Clubs) "nonforcing, likely signoff"

bP2D3C :: Action
bP2D3C = do
    clubsSignoff
    -- When you're a passed hand, bidding a new suit is always nonforcing, so
    -- that's no longer alertable.
    makeCall $ T.Bid 3 T.Clubs


b2D3H :: Action
b2D3H = do
    pointRange 7 9
    minSuitLength T.Hearts 5
    -- TODO: is this actually alertable? Maybe not...
    makeAlertableCall (T.Bid 3 T.Hearts)
                      ("mixed raise: 7" .+ NDash .+ "9 HCP, 5 hearts")


b2D3S :: Action
b2D3S = do
    pointRange 7 9
    minSuitLength T.Spades 5
    -- TODO: is this actually alertable? Maybe not...
    makeAlertableCall (T.Bid 3 T.Spades)
                      ("mixed raise: 7" .+ NDash .+ "9 HCP, 5 spades")


b2D3N :: Action
b2D3N = do
    pointRange 14 15
    soundHolding T.Diamonds
    balancedHand
    forEach T.majorSuits (`maxSuitLength` 3)
    maxSuitLength T.Clubs 4
    makeCall $ T.Bid 3 T.Notrump


b2D2N :: Action
b2D2N = do
    forbidAll [b2D3N]
    pointRange 11 40
    makeAlertableCall (T.Bid 2 T.Notrump) "inv+, asks for strength and majors"
