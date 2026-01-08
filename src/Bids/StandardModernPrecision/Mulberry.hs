module Bids.StandardModernPrecision.Mulberry(
    -- If there's a way to do this without doing it separately for every earlier
    -- auction, I can't see it.
    b2D2N3C3D3H4D
  , b2D2N3C3D3S4D
  , b2D2N3C3D3N4D
  , b2D2N3H4D
  , b2D2N3S4D
  , b2D2N3N4D

  , b2D2N3C3D3H4D4H
  , b2D2N3C3D3S4D4H
  , b2D2N3C3D3N4D4H
  , b2D2N3H4D4H
  , b2D2N3S4D4H
  , b2D2N3N4D4H

  , b2D2N3C3D3H4D4HP
  , b2D2N3C3D3S4D4HP
  , b2D2N3C3D3N4D4HP
  , b2D2N3H4D4HP
  , b2D2N3S4D4HP
  , b2D2N3N4D4HP

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


b2D2N3C3D3H4D :: Action
b2D2N3C3D3H4D = E.nameAction "b2D2N3C3D3H4D" $ do
    E.forbid slamInterestOver2DMin_
    E.makeAlertableCall
        (T.Bid 4 T.Diamonds)
        ("(delayed alert) no slam interest, prompts " .+ T.Bid 4 T.Hearts)


b2D2N3C3D3S4D :: Action
b2D2N3C3D3S4D = E.nameAction "b2D2N3C3D3S4D"$ do
    E.forbid slamInterestOver2DMin_
    E.makeAlertableCall
        (T.Bid 4 T.Diamonds)
        ("(delayed alert) no slam interest, prompts " .+ T.Bid 4 T.Hearts)


b2D2N3C3D3N4D :: Action
b2D2N3C3D3N4D = E.nameAction "b2D2N3C3D3N4D"$ do
    E.forbid slamInterestOver2DMin_
    E.makeAlertableCall
        (T.Bid 4 T.Diamonds)
        ("(delayed alert) no slam interest, prompts " .+ T.Bid 4 T.Hearts)


b2D2N3H4D :: Action
b2D2N3H4D = E.nameAction "b2D2N3H4D"$ do
    E.forbid slamInterestOver2DMax_
    E.makeAlertableCall
        (T.Bid 4 T.Diamonds)
        ("(delayed alert) no slam interest, prompts " .+ T.Bid 4 T.Hearts)


b2D2N3S4D :: Action
b2D2N3S4D = E.nameAction "b2D2N3S4D"$ do
    E.forbid slamInterestOver2DMax_
    E.makeAlertableCall
        (T.Bid 4 T.Diamonds)
        ("(delayed alert) no slam interest, prompts " .+ T.Bid 4 T.Hearts)


b2D2N3N4D :: Action
b2D2N3N4D = E.nameAction "b2D2N3N4D"$ do
    E.forbid slamInterestOver2DMax_
    E.makeAlertableCall
        (T.Bid 4 T.Diamonds)
        ("(delayed alert) no slam interest, prompts " .+ T.Bid 4 T.Hearts)


b4D4H_ :: Action
b4D4H_ = do
    E.makeAlertableCall (T.Bid 4 T.Hearts) "(delayed alert) pass or correct"

b2D2N3C3D3H4D4H, b2D2N3C3D3S4D4H, b2D2N3C3D3N4D4H :: Action
b2D2N3H4D4H, b2D2N3S4D4H, b2D2N3N4D4H :: Action
b2D2N3C3D3H4D4H = b4D4H_
b2D2N3C3D3S4D4H = b4D4H_
b2D2N3C3D3N4D4H = b4D4H_
b2D2N3H4D4H = b4D4H_
b2D2N3S4D4H = b4D4H_
b2D2N3N4D4H = b4D4H_


b2D2N3C3D3H4D4HP :: Action
b2D2N3C3D3H4D4HP = E.nameAction "b2D2N3C3D3H4D4HP"$ do
    minSuitLength T.Hearts 5
    maxSuitLength T.Spades 3
    maxSuitLength T.Clubs 2


b2D2N3C3D3S4D4HP :: Action
b2D2N3C3D3S4D4HP = E.nameAction "b2D2N3C3D3S4D4HP"$ do
    minSuitLength T.Hearts 4
    maxSuitLength T.Spades 4
    maxSuitLength T.Clubs 2


b2D2N3C3D3N4D4HP :: Action
b2D2N3C3D3N4D4HP = E.nameAction "b2D2N3C3D3N4D4HP"$ do
    minSuitLength T.Hearts 4
    maxSuitLength T.Spades 3
    maxSuitLength T.Clubs 2  -- Might bid this with 3 clubs, too


b2D2N3H4D4HP :: Action
b2D2N3H4D4HP = E.nameAction "b2D2N3H4D4HP"$ do
    minSuitLength T.Hearts 5
    maxSuitLength T.Spades 3
    maxSuitLength T.Clubs 2


b2D2N3S4D4HP :: Action
b2D2N3S4D4HP = E.nameAction "b2D2N3S4D4HP"$ do
    minSuitLength T.Hearts 4
    maxSuitLength T.Spades 4
    maxSuitLength T.Clubs 2


b2D2N3N4D4HP :: Action
b2D2N3N4D4HP = E.nameAction "b2D2N3N4D4HP"$ do
    minSuitLength T.Hearts 4
    maxSuitLength T.Spades 3
    maxSuitLength T.Clubs 2  -- Might bid this with 3 clubs, too
