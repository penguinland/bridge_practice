module Bids.StandardModernPrecision.Mulberry(
    -- If there's a way to do this without doing it separately for every earlier
    -- auction, I can't see it.
    b2D2N3C3D3H4D
  , b2D2N3C3D3H4D4H
  , b2D2N3C3D3H4D4HP
  , b2D2N3C3D3H4D4H4S  -- might have bid 3S instead of 4D
  , b2D2N3C3D3H4D4H5C

  , b2D2N3C3D3S4D
  , b2D2N3C3D3S4D4H
  , b2D2N3C3D3S4D4HP
  , b2D2N3C3D3S4D4H4S
  , b2D2N3C3D3S4D4H5C

  , b2D2N3C3D3N4D
  , b2D2N3C3D3N4D4H
  , b2D2N3C3D3N4D4HP
  , b2D2N3C3D3N4D4H4S
  , b2D2N3C3D3N4D4H5C

  , b2D2N3D4D
  , b2D2N3D4D4H
  , b2D2N3D4D4HP       -- might have bid 3H instead of 4D
  , b2D2N3D4D4H4S      -- might have bid 3S instead of 4D
  , b2D2N3D4D4H5C

  , b2D2N3H4D
  , b2D2N3H4D4H
  , b2D2N3H4D4HP
  , b2D2N3H4D4H4S      -- might have bid 3S instead of 4D
  , b2D2N3H4D4H5C

  , b2D2N3S4D
  , b2D2N3S4D4H
  , b2D2N3S4D4HP
  , b2D2N3S4D4H4S
  , b2D2N3S4D4H5C
) where


import Action(Action)
import qualified EDSL as E
import Output((.+))
import qualified Terminology as T


slamInterestOver2DMin_ :: Action
slamInterestOver2DMin_ = E.nameAction "slam_interest" $ do
    E.alternatives [E.pointRange 20 40, E.maxLoserCount 5]


slamInterestOver2DMax_ :: Action
slamInterestOver2DMax_ = E.nameAction "slam_interest" $ do
    E.alternatives [E.pointRange 17 40, E.maxLoserCount 5]


-- 4D

b4DMin_ :: Action
b4DMin_ = E.nameAction "b2D2N3C3D3X4D" $ do
    E.forbid slamInterestOver2DMin_
    E.makeAlertableCall
        (T.Bid 4 T.Diamonds)
        ("(delayed alert) no slam interest, prompts " .+ T.Bid 4 T.Hearts)

b4DMax_ :: Action
b4DMax_ = E.nameAction "b2D2N3X4D" $ do
    E.forbid slamInterestOver2DMax_
    E.makeAlertableCall
        (T.Bid 4 T.Diamonds)
        ("(delayed alert) no slam interest, prompts " .+ T.Bid 4 T.Hearts)

b2D2N3C3D3H4D, b2D2N3C3D3S4D, b2D2N3C3D3N4D :: Action
b2D2N3H4D, b2D2N3S4D, b2D2N3D4D :: Action
b2D2N3C3D3H4D = b4DMin_
b2D2N3C3D3S4D = b4DMin_
b2D2N3C3D3N4D = b4DMin_
b2D2N3H4D = b4DMax_
b2D2N3S4D = b4DMax_
b2D2N3D4D = b4DMax_


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
b2D2N3C3D3H4D4HP = E.nameAction "b2D2N3C3D3H4D4HP"$ do
    E.minSuitLength T.Hearts 5
    E.maxSuitLength T.Spades 3
    E.maxSuitLength T.Clubs 2
    E.makeCall T.Pass

b2D2N3H4D4HP :: Action
b2D2N3H4D4HP = b2D2N3C3D3H4D4HP


b2D2N3C3D3S4D4HP :: Action
b2D2N3C3D3S4D4HP = E.nameAction "b2D2N3C3D3S4D4HP"$ do
    E.minSuitLength T.Hearts 4
    E.maxSuitLength T.Spades 4
    E.maxSuitLength T.Clubs 2
    E.makeCall T.Pass

b2D2N3S4D4HP :: Action
b2D2N3S4D4HP = b2D2N3C3D3S4D4HP


b2D2N3C3D3N4D4HP :: Action
b2D2N3C3D3N4D4HP = E.nameAction "b2D2N3C3D3N4D4HP"$ do
    E.minSuitLength T.Hearts 4
    E.maxSuitLength T.Spades 3
    E.maxSuitLength T.Clubs 2  -- Might bid this with 3 clubs, too
    E.makeCall T.Pass

b2D2N3D4D4HP :: Action
b2D2N3D4D4HP = b2D2N3C3D3N4D4HP


-- 4D-4H-4S

b2D2N3C3D3H4D4H4S :: Action
b2D2N3C3D3H4D4H4S = E.nameAction "b2D2N3C3D3H4D4H4S"$ do
    E.minSuitLength T.Spades 4
    E.maxSuitLength T.Hearts 4
    E.maxSuitLength T.Clubs 3
    E.makeCall $ T.Bid 4 T.Spades

b2D2N3H4D4H4S :: Action
b2D2N3H4D4H4S = b2D2N3C3D3H4D4H4S


b2D2N3C3D3S4D4H4S :: Action
b2D2N3C3D3S4D4H4S = E.nameAction "b2D2N3C3D3S4D4H4S"$ do
    E.minSuitLength T.Spades 5
    E.maxSuitLength T.Hearts 3
    E.maxSuitLength T.Clubs 3
    E.makeCall $ T.Bid 4 T.Spades

b2D2N3S4D4H4S :: Action
b2D2N3S4D4H4S = b2D2N3C3D3S4D4H4S


b2D2N3C3D3N4D4H4S :: Action
b2D2N3C3D3N4D4H4S = E.nameAction "b2D2N3C3D3N4D4H4S"$ do
    E.minSuitLength T.Spades 4
    E.maxSuitLength T.Hearts 3
    E.maxSuitLength T.Clubs 3
    E.makeCall $ T.Bid 4 T.Spades

b2D2N3D4D4H4S :: Action
b2D2N3D4D4H4S = b2D2N3C3D3N4D4H4S


-- 4D-4H-5C

b2D2N3C3D3H4D4H5C :: Action
b2D2N3C3D3H4D4H5C = E.nameAction "b2D2N3C3D3H4D4H5C"$ do
    E.minSuitLength T.Clubs 3
    E.maxSuitLength T.Hearts 4
    E.maxSuitLength T.Spades 3
    E.forbid $ E.hasStopper T.Diamonds
    E.makeCall $ T.Bid 5 T.Clubs

b2D2N3H4D4H5C :: Action
b2D2N3H4D4H5C = b2D2N3C3D3H4D4H5C


b2D2N3C3D3S4D4H5C :: Action
b2D2N3C3D3S4D4H5C = E.nameAction "b2D2N3C3D3S4D4H5C"$ do
    E.minSuitLength T.Clubs 3
    E.maxSuitLength T.Spades 4
    E.maxSuitLength T.Hearts 3
    E.forbid $ E.hasStopper T.Diamonds
    E.makeCall $ T.Bid 5 T.Clubs

b2D2N3S4D4H5C :: Action
b2D2N3S4D4H5C = b2D2N3C3D3S4D4H5C


b2D2N3C3D3N4D4H5C :: Action
b2D2N3C3D3N4D4H5C = E.nameAction "b2D2N3C3D3N4D4H5C"$ do
    E.minSuitLength T.Clubs 4
    E.maxSuitLength T.Spades 3
    E.maxSuitLength T.Hearts 3
    E.forbid $ E.hasStopper T.Diamonds
    E.makeCall $ T.Bid 5 T.Clubs

b2D2N3D4D4H5C :: Action
b2D2N3D4D4H5C = b2D2N3C3D3N4D4H5C
