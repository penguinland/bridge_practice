module Bids.StandardModernPrecision.Mulberry(
    b2D2N3C3D3H4D
  , b2D2N3C3D3H4D4H
  , b2D2N3C3D3H4D4HP
  , b2D2N3C3D3H4D4H4S  -- Avoid: might have bid 3S instead of 4D
  , b2D2N3C3D3H4D4H5C
  , b2D2N3C3D3H4H
  , b2D2N3C3D3H4S
  , b2D2N3C3D3H4N
  , b2D2N3C3D3S4D
  , b2D2N3C3D3S4D4H
  , b2D2N3C3D3S4D4HP
  , b2D2N3C3D3S4D4H4S
  , b2D2N3C3D3S4D4H5C
  , b2D2N3C3D3S4H
  , b2D2N3C3D3S4S
  , b2D2N3C3D3S4N
  , b2D2N3C3D3N4D
  , b2D2N3C3D3N4D4H
  , b2D2N3C3D3N4D4HP
  , b2D2N3C3D3N4D4H4S
  , b2D2N3C3D3N4D4H5C
  , b2D2N3C3D3N4H
  , b2D2N3C3D3N4S
  , b2D2N3C3D3N4N
  , b2D2N3D4D
  , b2D2N3D4D4H
  , b2D2N3D4D4HP       -- Avoid: might have bid 3H instead of 4D
  , b2D2N3D4D4H4S      -- Avoid: might have bid 3S instead of 4D
  , b2D2N3D4D4H5C
  , b2D2N3D4H
  , b2D2N3D4S          -- Avoid: might have bid 3H instead of 4S
  , b2D2N3D4N          -- Avoid: might have bid 3S instead of 4N
  , b2D2N3H4D
  , b2D2N3H4D4H
  , b2D2N3H4D4HP
  , b2D2N3H4D4H4S      -- Avoid: might have bid 3S instead of 4D
  , b2D2N3H4D4H5C
  , b2D2N3H4H
  , b2D2N3H4S
  , b2D2N3H4N          -- Avoid: might have bid 3S instead of 4N
  , b2D2N3S4D
  , b2D2N3S4D4H
  , b2D2N3S4D4HP
  , b2D2N3S4D4H4S
  , b2D2N3S4D4H5C
  , b2D2N3S4H
  , b2D2N3S4S
  , b2D2N3S4N
  -- Keycard responses: don't bother customizing on the start of the auction
  , bKCC4H4S  -- Naming scheme: "smp_bKC" <trump suit> <asking bid> <response>
  , bKCC4H4N
  , bKCC4H5C
  , bKCC4H5D
  , bKCH4S4N
  , bKCH4S5C
  , bKCH4S5D
  , bKCH4S5H
  , bKCS4N5C
  , bKCS4N5D
  , bKCS4N5H
  , bKCS4N5S
) where


import Action(Action, withholdBid)
import qualified EDSL as E
import Output((.+))
import qualified Terminology as T


slamInterestOver2DMin_ :: Action
slamInterestOver2DMin_ = do
    E.maxLoserCount 5
    E.pointRange 20 40

slamInterestOver2DMax_ :: Action
slamInterestOver2DMax_ = do
    E.maxLoserCount 5
    E.pointRange 17 40

-- 4D

b2D4D_ :: Action
b2D4D_ = do
    -- A reason not to prefer notrump
    -- TODO: make this reason more nuanced. If you have a balanced hand with a
    -- sound diamond holding and 5 hearts and partner has shown 4 hearts, you'd
    -- want to play in 4H and not 3N.
    E.forbid (E.soundHolding T.Diamonds)
    E.makeAlertableCall
        (T.Bid 4 T.Diamonds)
        ("(delayed alert) no slam interest, prompts " .+ T.Bid 4 T.Hearts)


b2D4DMin_ :: Action
b2D4DMin_ = E.nameAction "smp_b2D2N3C3D3X4D" $ do
    E.forbid slamInterestOver2DMin_
    b2D4D_

b2D2N3C3D3H4D, b2D2N3C3D3S4D, b2D2N3C3D3N4D :: Action
b2D2N3C3D3H4D = b2D4DMin_
b2D2N3C3D3S4D = b2D4DMin_
b2D2N3C3D3N4D = b2D4DMin_


b2D4DMax_ :: Action
b2D4DMax_ = E.nameAction "smp_b2D2N3X4D" $ do
    E.forbid slamInterestOver2DMax_
    b2D4D_

b2D2N3H4D, b2D2N3S4D, b2D2N3D4D :: Action
b2D2N3H4D = b2D4DMax_
b2D2N3S4D = b2D4DMax_
b2D2N3D4D = b2D4DMax_


-- 4D-4H

b4D4H_ :: Action
b4D4H_ =
    E.makeAlertableCall (T.Bid 4 T.Hearts) "(delayed alert) pass or correct"

b2D2N3C3D3H4D4H, b2D2N3C3D3S4D4H, b2D2N3C3D3N4D4H :: Action
b2D2N3H4D4H, b2D2N3S4D4H, b2D2N3D4D4H :: Action
b2D2N3C3D3H4D4H = b4D4H_
b2D2N3C3D3S4D4H = b4D4H_
b2D2N3C3D3N4D4H = b4D4H_
b2D2N3H4D4H = b4D4H_
b2D2N3S4D4H = b4D4H_
b2D2N3D4D4H = b4D4H_


-- 4D-4H-P

b2D2N3C3D3H4D4HP :: Action
b2D2N3C3D3H4D4HP = E.nameAction "smp_b2D2N3C3D3H4D4HP" $ do
    E.minSuitLength T.Hearts 5
    E.maxSuitLength T.Spades 3
    E.maxSuitLength T.Clubs 2
    E.makeCall T.Pass

b2D2N3H4D4HP :: Action
b2D2N3H4D4HP = b2D2N3C3D3H4D4HP


b2D2N3C3D3S4D4HP :: Action
b2D2N3C3D3S4D4HP = E.nameAction "smp_b2D2N3C3D3S4D4HP" $ do
    E.minSuitLength T.Hearts 4
    E.maxSuitLength T.Spades 4
    E.maxSuitLength T.Clubs 2
    E.makeCall T.Pass

b2D2N3S4D4HP :: Action
b2D2N3S4D4HP = b2D2N3C3D3S4D4HP


b2D2N3C3D3N4D4HP :: Action
b2D2N3C3D3N4D4HP = E.nameAction "smp_b2D2N3C3D3N4D4HP" $ do
    E.minSuitLength T.Hearts 4
    E.maxSuitLength T.Spades 3
    E.maxSuitLength T.Clubs 2  -- Might bid this with 3 clubs, too
    E.makeCall T.Pass

b2D2N3D4D4HP :: Action
b2D2N3D4D4HP = b2D2N3C3D3N4D4HP


-- 4D-4H-4S

b2D2N3C3D3H4D4H4S :: Action
b2D2N3C3D3H4D4H4S = E.nameAction "smp_b2D2N3C3D3H4D4H4S" $ do
    E.minSuitLength T.Spades 4
    E.maxSuitLength T.Hearts 4
    E.maxSuitLength T.Clubs 3
    E.makeCall $ T.Bid 4 T.Spades

b2D2N3H4D4H4S :: Action
b2D2N3H4D4H4S = b2D2N3C3D3H4D4H4S


b2D2N3C3D3S4D4H4S :: Action
b2D2N3C3D3S4D4H4S = E.nameAction "smp_b2D2N3C3D3S4D4H4S" $ do
    E.minSuitLength T.Spades 5
    E.maxSuitLength T.Hearts 3
    E.maxSuitLength T.Clubs 3
    E.makeCall $ T.Bid 4 T.Spades

b2D2N3S4D4H4S :: Action
b2D2N3S4D4H4S = b2D2N3C3D3S4D4H4S


b2D2N3C3D3N4D4H4S :: Action
b2D2N3C3D3N4D4H4S = E.nameAction "smp_b2D2N3C3D3N4D4H4S" $ do
    E.minSuitLength T.Spades 4
    E.maxSuitLength T.Hearts 3
    E.maxSuitLength T.Clubs 3
    E.makeCall $ T.Bid 4 T.Spades

b2D2N3D4D4H4S :: Action
b2D2N3D4D4H4S = b2D2N3C3D3N4D4H4S


-- 4D-4H-5C

b2D2N3C3D3H4D4H5C :: Action
b2D2N3C3D3H4D4H5C = E.nameAction "smp_b2D2N3C3D3H4D4H5C" $ do
    E.minSuitLength T.Clubs 3
    E.maxSuitLength T.Hearts 4
    E.maxSuitLength T.Spades 3
    E.forbid $ E.hasStopper T.Diamonds
    E.makeCall $ T.Bid 5 T.Clubs

b2D2N3H4D4H5C :: Action
b2D2N3H4D4H5C = b2D2N3C3D3H4D4H5C


b2D2N3C3D3S4D4H5C :: Action
b2D2N3C3D3S4D4H5C = E.nameAction "smp_b2D2N3C3D3S4D4H5C" $ do
    E.minSuitLength T.Clubs 3
    E.maxSuitLength T.Spades 4
    E.maxSuitLength T.Hearts 3
    E.forbid $ E.hasStopper T.Diamonds
    E.makeCall $ T.Bid 5 T.Clubs

b2D2N3S4D4H5C :: Action
b2D2N3S4D4H5C = b2D2N3C3D3S4D4H5C


b2D2N3C3D3N4D4H5C :: Action
b2D2N3C3D3N4D4H5C = E.nameAction "smp_b2D2N3C3D3N4D4H5C" $ do
    E.minSuitLength T.Clubs 4
    E.maxSuitLength T.Spades 3
    E.maxSuitLength T.Hearts 3
    E.forbid $ E.hasStopper T.Diamonds
    E.makeCall $ T.Bid 5 T.Clubs

b2D2N3D4D4H5C :: Action
b2D2N3D4D4H5C = b2D2N3C3D3N4D4H5C


-- Keycard asks
-- These are all the same shape as the signoff bids, just different strength

b2D2N3C3D3H4H :: Action
b2D2N3C3D3H4H = E.nameAction "smp_b2D2N3C3D3H4H" $ do
    withholdBid b2D2N3C3D3H4D4H5C
    slamInterestOver2DMin_
    E.makeAlertableCall (T.Bid 4 T.Hearts)
                        "(delayed alert) keycard ask in clubs"


b2D2N3C3D3H4S :: Action
b2D2N3C3D3H4S = E.nameAction "smp_b2D2N3C3D3H4S" $ do
    withholdBid b2D2N3C3D3H4D4HP
    slamInterestOver2DMin_
    E.makeAlertableCall (T.Bid 4 T.Spades)
                        "(delayed alert) keycard ask in hearts"


b2D2N3C3D3H4N :: Action
b2D2N3C3D3H4N = E.nameAction "smp_b2D2N3C3D3H4N" $ do
    withholdBid b2D2N3C3D3H4D4H4S
    slamInterestOver2DMin_
    E.makeAlertableCall (T.Bid 4 T.Notrump)
                        "(delayed alert) keycard ask in spades"



b2D2N3C3D3S4H :: Action
b2D2N3C3D3S4H = E.nameAction "smp_b2D2N3C3D3S4H" $ do
    withholdBid b2D2N3C3D3S4D4H5C
    slamInterestOver2DMin_
    E.makeAlertableCall (T.Bid 4 T.Hearts)
                        "(delayed alert) keycard ask in clubs"


b2D2N3C3D3S4S :: Action
b2D2N3C3D3S4S = E.nameAction "smp_b2D2N3C3D3S4S" $ do
    withholdBid b2D2N3C3D3S4D4HP
    slamInterestOver2DMin_
    E.makeAlertableCall (T.Bid 4 T.Spades)
                        "(delayed alert) keycard ask in hearts"


b2D2N3C3D3S4N :: Action
b2D2N3C3D3S4N = E.nameAction "smp_b2D2N3C3D3S4N" $ do
    withholdBid b2D2N3C3D3S4D4H4S
    slamInterestOver2DMin_
    E.makeAlertableCall (T.Bid 4 T.Notrump)
                        "(delayed alert) keycard ask in spades"


b2D2N3C3D3N4H :: Action
b2D2N3C3D3N4H = E.nameAction "smp_b2D2N3C3D3N4H" $ do
    withholdBid b2D2N3C3D3N4D4H5C
    slamInterestOver2DMin_
    E.makeAlertableCall (T.Bid 4 T.Hearts)
                        "(delayed alert) keycard ask in clubs"


b2D2N3C3D3N4S :: Action
b2D2N3C3D3N4S = E.nameAction "smp_b2D2N3C3D3N4S" $ do
    withholdBid b2D2N3C3D3N4D4HP
    slamInterestOver2DMin_
    E.makeAlertableCall (T.Bid 4 T.Spades)
                        "(delayed alert) keycard ask in hearts"


b2D2N3C3D3N4N :: Action
b2D2N3C3D3N4N = E.nameAction "smp_b2D2N3C3D3N4N" $ do
    withholdBid b2D2N3C3D3N4D4H4S
    slamInterestOver2DMin_
    E.makeAlertableCall (T.Bid 4 T.Notrump)
                        "(delayed alert) keycard ask in spades"


b2D2N3D4H :: Action
b2D2N3D4H = E.nameAction "smp_b2D2N3D4H" $ do
    withholdBid b2D2N3D4D4H5C
    slamInterestOver2DMax_
    E.makeAlertableCall (T.Bid 4 T.Hearts)
                        "(delayed alert) keycard ask in clubs"


b2D2N3D4S :: Action
b2D2N3D4S = E.nameAction "smp_b2D2N3D4S" $ do
    withholdBid b2D2N3D4D4HP
    slamInterestOver2DMax_
    E.makeAlertableCall (T.Bid 4 T.Spades)
                        "(delayed alert) keycard ask in hearts"


b2D2N3D4N :: Action
b2D2N3D4N = E.nameAction "smp_b2D2N3D4N" $ do
    withholdBid b2D2N3D4D4H4S
    slamInterestOver2DMax_
    E.makeAlertableCall (T.Bid 4 T.Notrump)
                        "(delayed alert) keycard ask in spades"


b2D2N3H4H :: Action
b2D2N3H4H = E.nameAction "smp_b2D2N3H4H" $ do
    withholdBid b2D2N3H4D4H5C
    slamInterestOver2DMax_
    E.makeAlertableCall (T.Bid 4 T.Hearts)
                        "(delayed alert) keycard ask in clubs"


b2D2N3H4S :: Action
b2D2N3H4S = E.nameAction "smp_b2D2N3H4S" $ do
    withholdBid b2D2N3H4D4HP
    slamInterestOver2DMax_
    E.makeAlertableCall (T.Bid 4 T.Spades)
                        "(delayed alert) keycard ask in hearts"


b2D2N3H4N :: Action
b2D2N3H4N = E.nameAction "smp_b2D2N3H4N" $ do
    withholdBid b2D2N3H4D4H4S
    slamInterestOver2DMax_
    E.makeAlertableCall (T.Bid 4 T.Notrump)
                        "(delayed alert) keycard ask in spades"


b2D2N3S4H :: Action
b2D2N3S4H = E.nameAction "smp_b2D2N3S4H" $ do
    withholdBid b2D2N3S4D4H5C
    slamInterestOver2DMax_
    E.makeAlertableCall (T.Bid 4 T.Hearts)
                        "(delayed alert) keycard ask in clubs"


b2D2N3S4S :: Action
b2D2N3S4S = E.nameAction "smp_b2D2N3S4S" $ do
    withholdBid b2D2N3S4D4HP
    slamInterestOver2DMax_
    E.makeAlertableCall (T.Bid 4 T.Spades)
                        "(delayed alert) keycard ask in hearts"


b2D2N3S4N :: Action
b2D2N3S4N = E.nameAction "smp_b2D2N3S4N" $ do
    withholdBid b2D2N3S4D4H4S
    slamInterestOver2DMax_
    E.makeAlertableCall (T.Bid 4 T.Notrump)
                        "(delayed alert) keycard ask in spades"


bKCC4H4S :: Action
bKCC4H4S = E.nameAction "smp_bKCC4H4S" $ do
    E.keycardCount T.Clubs 1 4
    E.makeAlertableCall (T.Bid 4 T.Spades)
                        "(delayed alert) 1 or 4 keycards"


bKCC4H4N :: Action
bKCC4H4N = E.nameAction "smp_bKCC4H4N" $ do
    E.keycardCount T.Clubs 3 0
    E.makeAlertableCall (T.Bid 4 T.Notrump)
                        "(delayed alert) 3 or 0 keycards"


bKCC4H5C :: Action
bKCC4H5C = E.nameAction "smp_bKCC4H5C" $ do
    E.keycardCount T.Clubs 2 5
    E.forbid $ E.hasCard T.Clubs 'Q'
    E.makeAlertableCall (T.Bid 5 T.Clubs)
                        "(delayed alert) 2 or 5 keycards w/o queen"


bKCC4H5D :: Action
bKCC4H5D = E.nameAction "smp_bKCC4H5D" $ do
    E.keycardCount T.Clubs 2 5
    E.hasCard T.Clubs 'Q'
    E.makeAlertableCall (T.Bid 5 T.Diamonds)
                        "(delayed alert) 2 or 5 keycards with queen"


bKCH4S4N :: Action
bKCH4S4N = E.nameAction "smp_bKCH4S4N" $ do
    E.keycardCount T.Hearts 1 4
    E.makeAlertableCall (T.Bid 4 T.Notrump)
                        "(delayed alert) 1 or 4 keycards"


bKCH4S5C :: Action
bKCH4S5C = E.nameAction "smp_bKCH4S5C" $ do
    E.keycardCount T.Hearts 3 0
    E.makeAlertableCall (T.Bid 5 T.Clubs)
                        "(delayed alert) 3 or 0 keycards"


bKCH4S5D :: Action
bKCH4S5D = E.nameAction "smp_bKCH4S5D" $ do
    E.keycardCount T.Hearts 2 5
    E.forbid $ E.hasCard T.Hearts 'Q'
    E.makeAlertableCall (T.Bid 5 T.Diamonds)
                        "(delayed alert) 2 or 5 keycards w/o queen"


bKCH4S5H :: Action
bKCH4S5H = E.nameAction "smp_bKCH4S5H" $ do
    E.keycardCount T.Hearts 2 5
    E.hasCard T.Hearts 'Q'
    E.makeAlertableCall (T.Bid 5 T.Hearts)
                        "(delayed alert) 2 or 5 keycards with queen"


bKCS4N5C :: Action
bKCS4N5C = E.nameAction "smp_bKCS4N5C" $ do
    E.keycardCount T.Spades 1 4
    E.makeAlertableCall (T.Bid 5 T.Clubs)
                        "(delayed alert) 1 or 4 keycards"


bKCS4N5D :: Action
bKCS4N5D = E.nameAction "smp_bKCS4N5D" $ do
    E.keycardCount T.Spades 3 0
    E.makeAlertableCall (T.Bid 5 T.Diamonds)
                        "(delayed alert) 3 or 0 keycards"


bKCS4N5H :: Action
bKCS4N5H = E.nameAction "smp_bKCS4N5H" $ do
    E.keycardCount T.Spades 2 5
    E.forbid $ E.hasCard T.Spades 'Q'
    E.makeAlertableCall (T.Bid 5 T.Hearts)
                        "(delayed alert) 2 or 5 keycards w/o queen"


bKCS4N5S :: Action
bKCS4N5S = E.nameAction "smp_bKCS4N5S" $ do
    E.keycardCount T.Spades 2 5
    E.hasCard T.Spades 'Q'
    E.makeAlertableCall (T.Bid 5 T.Spades)
                        "(delayed alert) 2 or 5 keycards with queen"
