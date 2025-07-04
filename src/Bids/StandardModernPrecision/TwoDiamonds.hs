module Bids.StandardModernPrecision.TwoDiamonds(
  name44Rkc
, noDirectOvercall
, b2D  -- Re-exported from BasicBids
, b2D2H
, bP2D2H
, b2D2H2S
, b2D2S
, bP2D2S
, b2D2N
, b2D2N3C
, b2D2N3CP
, b2D2N3C3D
, b2D2N3C3H
, b2D2N3C3S
, b2D2N3C3D3H
, b2D2N3C3D3S
, b2D2N3C3D3N
, b2D2N3D
, b2D2N3H
, b2D2N3S
, b2D3C
, bP2D3C
, b2D3H
, b2D3S
, b2D3N
, b2D4H
, b2D4S
) where


import Action(Action)
import Bids.StandardModernPrecision.BasicBids(b2D, lessThanInvitational)
import CommonBids(cannotPreempt)
import EDSL(suitLength, minSuitLength, maxSuitLength, makeCall, pointRange,
            makeAlertableCall, atLeastAsLong, longerThan, forbid, forbidAll,
            balancedHand, soundHolding, makePass, alternatives, forEach,
            minLoserCount, maxLoserCount, nameAction)
import Output(Punct(..), (.+), Description)
import qualified Terminology as T


-- This name is too long to write over and over.
name44Rkc :: Description
name44Rkc = T.Bid 4 T.Clubs .+ "/" .+ T.Bid 4 T.Diamonds .+ "/RKC"


noDirectOvercall :: Action
noDirectOvercall = nameAction "no_overcall" $ do
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
b2D2H = nameAction "smp_b2D2H" $ do
    heartsSignoff
    makeAlertableCall (T.Bid 2 T.Hearts) "nonforcing, likely signoff"

bP2D2H :: Action
bP2D2H = nameAction "smp_bP2D2H" $ do
    heartsSignoff
    -- When you're a passed hand, bidding a new suit is always nonforcing, so
    -- that's no longer alertable.
    makeCall $ T.Bid 2 T.Hearts


b2D2H2S :: Action
b2D2H2S = nameAction "smp_b2D2H2S" $ do
    suitLength T.Hearts 3
    makeAlertableCall (T.Bid 2 T.Spades) "exactly 4315 shape"


b2D2S :: Action
b2D2S = nameAction "smp_b2D2S" $ do
    spadesSignoff
    makeAlertableCall (T.Bid 2 T.Spades) "nonforcing, likely signoff"

bP2D2S :: Action
bP2D2S = nameAction "smp_bP2D2S" $ do
    spadesSignoff
    -- When you're a passed hand, bidding a new suit is always nonforcing, so
    -- that's no longer alertable.
    makeCall $ T.Bid 2 T.Spades


b2D3C :: Action
b2D3C = nameAction "smp_b2D3C" $ do
    clubsSignoff
    makeAlertableCall (T.Bid 3 T.Clubs) "nonforcing, likely signoff"

bP2D3C :: Action
bP2D3C = nameAction "smp_bP2D3C" $ do
    clubsSignoff
    -- When you're a passed hand, bidding a new suit is always nonforcing, so
    -- that's no longer alertable.
    makeCall $ T.Bid 3 T.Clubs


b2D3H :: Action
b2D3H = nameAction "smp_b2D3H" $ do
    pointRange 7 10
    minSuitLength T.Hearts 5
    -- NOTE: is this *not* alertable, per
    -- https://web2.acbl.org/documentLibrary/play/Alert-Procedures.pdf
    makeCall $ T.Bid 3 T.Hearts


b2D3S :: Action
b2D3S = nameAction "smp_b2D3S" $ do
    pointRange 7 10
    minSuitLength T.Spades 5
    -- NOTE: is this *not* alertable, either
    makeCall $ T.Bid 3 T.Spades


b2D3N :: Action
b2D3N = nameAction "smp_b2D3N" $ do
    pointRange 14 15
    soundHolding T.Diamonds
    balancedHand
    forEach T.majorSuits (`maxSuitLength` 3)
    maxSuitLength T.Clubs 4
    makeCall $ T.Bid 3 T.Notrump


b2D4H :: Action
b2D4H = nameAction "smp_b2D4H" $ do
    pointRange 14 15
    minSuitLength T.Hearts 5
    -- If we have a double fit, you might consider looking for slam. Let's avoid
    -- those possibilities.
    maxSuitLength T.Spades 3
    maxSuitLength T.Clubs 2
    makeCall $ T.Bid 4 T.Hearts


b2D4S :: Action
b2D4S = nameAction "smp_b2D4S" $ do
    pointRange 14 15
    minSuitLength T.Spades 5
    -- If we have a double fit, you might consider looking for slam. Let's avoid
    -- those possibilities.
    maxSuitLength T.Hearts 3
    maxSuitLength T.Clubs 2
    makeCall $ T.Bid 4 T.Spades


b2D2N :: Action
b2D2N = nameAction "smp_b2D2N" $ do
    -- If you know the final contract, jump to it immediately.
    forbidAll [b2D3N, b2D4H, b2D4S]
    pointRange 11 40
    makeAlertableCall (T.Bid 2 T.Notrump) "inv+, asks for strength and majors"


b2D2N3C :: Action
b2D2N3C = nameAction "smp_b2D2N3C" $ do
    -- You should have the lower half of your strength to not accept an invite.
    alternatives [pointRange 10 12, pointRange 10 13 >> minLoserCount 7]
    makeAlertableCall (T.Bid 3 T.Clubs)
                      ("minimum, likely 10" .+ NDash .+ "12 HCP")


b2D2N3D :: Action
b2D2N3D = nameAction "smp_b2D2N3D" $ do
    forbid b2D2N3C  -- We'd accept an invite to game
    forEach T.majorSuits (`suitLength` 4)
    makeAlertableCall (T.Bid 3 T.Diamonds)
                      ("maximum strength, 4" .+ NDash .+ "4 in the majors")


b2D2N3H :: Action
b2D2N3H = nameAction "smp_b2D2N3H" $ do
    forbid b2D2N3C  -- We'd accept an invite to game
    suitLength T.Hearts 3
    makeAlertableCall (T.Bid 3 T.Hearts) "maximum strength, 4315 shape exactly"


b2D2N3S :: Action
b2D2N3S = nameAction "smp_b2D2N3S" $ do
    forbid b2D2N3C  -- We'd accept an invite to game
    suitLength T.Spades 3
    makeAlertableCall (T.Bid 3 T.Spades) "maximum strength, 3415 shape exactly"


b2D2N3C3D :: Action
b2D2N3C3D = nameAction "smp_b2D2N3C3D" $ do
    -- Even if you don't have enough HCPs to be game forcing, if you're shapely
    -- enough, you might be game forcing anyway.
    alternatives [ pointRange 14 40
                 , maxLoserCount 6 >> maxSuitLength T.Diamonds 5
                 ]
    makeAlertableCall (T.Bid 3 T.Diamonds) "artificial ask about majors"


b2D2N3C3D3H :: Action
b2D2N3C3D3H = nameAction "smp_b2D2N3C3D3H" $ do
    suitLength T.Hearts 3
    makeAlertableCall (T.Bid 3 T.Hearts) "shorter hearts: 4315 shape exactly"


b2D2N3C3D3S :: Action
b2D2N3C3D3S = nameAction "smp_b2D2N3C3D3S" $ do
    suitLength T.Spades 3
    makeAlertableCall (T.Bid 3 T.Spades) "shorter spades: 3415 shape exactly"


b2D2N3C3D3N :: Action
b2D2N3C3D3N = nameAction "smp_b2D2N3C3D3N" $ do
    forEach T.majorSuits (`suitLength` 4)
    makeAlertableCall (T.Bid 3 T.Notrump) ("4" .+ NDash .+ "4 in the majors")


b2D2N3CP :: Action
b2D2N3CP = nameAction "smp_b2D2N3CP" $ do
    forbid b2D2N3C3D  -- Not game-forcing
    minSuitLength T.Clubs 4
    -- Would you risk a 4-3 major-suit fit when you knew about a 4-4 club fit?
    forEach T.majorSuits (`maxSuitLength` 4)
    maxSuitLength T.Diamonds 6
    makePass


b2D2N3C3H :: Action
b2D2N3C3H = nameAction "smp_b2D2N3C3H" $ do
    forbid b2D2N3C3D  -- Not game-forcing
    minSuitLength T.Hearts 5
    maxSuitLength T.Clubs 3
    maxSuitLength T.Diamonds 6
    T.Hearts `atLeastAsLong` T.Spades
    makeCall $ T.Bid 3 T.Hearts


b2D2N3C3S :: Action
b2D2N3C3S = nameAction "smp_b2D2N3C3S" $ do
    forbid b2D2N3C3D  -- Not game-forcing
    minSuitLength T.Spades 5
    maxSuitLength T.Clubs 3
    maxSuitLength T.Diamonds 6
    T.Spades `longerThan` T.Hearts
    makeCall $ T.Bid 3 T.Spades
