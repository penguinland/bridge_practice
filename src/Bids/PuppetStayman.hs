module Bids.PuppetStayman(
    noInterference
  , b2N  -- re-exported from StandardOpenings
--  , b2N3C
  , b2N3C3D
--  , b2N3C3D3H
--  , b2N3C3D3H3N
--  , b2N3C3D3H4S
--  , b2N3C3D3S
--  , b2N3C3D3S3N
--  , b2N3C3D3S4H
--  , b2N3C3D3N
--  , b2N3C3D4D  -- TODO: when do you bid 4C, and when 4D?
--  , b2N3C3D4C4H
--  , b2N3C3D4C4S
  , b2N3C3H
  , b2N3C3H3S
  , b2N3C3H3N
  , b2N3C3H4H
  , b2N3C3S
  , b2N3C3S3N
  , b2N3C3S4H
  , b2N3C3S4S
  , b2N3C3N
-- TODO: Texas transfers of b2N3C3N4D, showing 6-4 shape? Does that belong here?
) where


import Action(Action)
import qualified Bids.Meckwell as MW  -- Used in noInterference 
import Bids.StandardOpenings(b2N)
import CommonBids(cannotPreempt)      -- Used in noInterference 
import EDSL(makeCall, makeAlertableCall, forbid, forbidAll, forEach,
            suitLength, minSuitLength, maxSuitLength, alternatives,
            nameAction, pointRange, maxLoserCount)
--import Output((.+))
import qualified Terminology as T


noInterference :: Action
noInterference = do
    -- TODO: this is good enough for now, but eventually distinguish hands that
    -- should pass over 2N from ones that should pass over 1N.
    cannotPreempt
    -- If the opponents can't bid Meckwell, they probably can't bid anything.
    forbidAll [MW.b1NoX, MW.b1No2C, MW.b1No2D, MW.b1No2H, MW.b1No2S, MW.b1No2N]
    makeCall T.Pass


slamInterest_ :: Action
slamInterest_ = alternatives [pointRange 12 40, maxLoserCount 7]


b2N3C3D :: Action
b2N3C3D = nameAction "puppet_b2N3C3D" $ do
    forEach T.majorSuits (`maxSuitLength` 4)
    alternatives [suitLength T.Hearts 4, suitLength T.Spades 4]
    makeAlertableCall (T.Bid 3 T.Diamonds) "has at least one 4-card major"


b2N3C3H :: Action
b2N3C3H = nameAction "puppet_b2N3C3H" $ do
    suitLength T.Hearts 5
    makeAlertableCall (T.Bid 3 T.Hearts) "5-card heart suit"


b2N3C3H3S :: Action
b2N3C3H3S = nameAction "puppet_b2N3C3H3S" $ do
    slamInterest_
    minSuitLength T.Hearts 3
    makeAlertableCall (T.Bid 3 T.Spades) "slam interest in hearts"


b2N3C3H3N :: Action
b2N3C3H3N = nameAction "puppet_b2N3C3H3N" $ do
    maxSuitLength T.Hearts 2
    makeCall $ T.Bid 3 T.Notrump


b2N3C3H4H :: Action
b2N3C3H4H = nameAction "puppet_b2N3C3H4H" $ do
    forbid slamInterest_
    minSuitLength T.Hearts 3
    makeCall $ T.Bid 4 T.Hearts


b2N3C3S :: Action
b2N3C3S = nameAction "puppet_b2N3C3S" $ do
    suitLength T.Spades 5
    makeAlertableCall (T.Bid 3 T.Spades) "5-card spade suit"


b2N3C3S3N :: Action
b2N3C3S3N = nameAction "puppet_b2N3C3S3N" $ do
    maxSuitLength T.Spades 2
    makeCall $ T.Bid 3 T.Notrump


b2N3C3S4H :: Action
b2N3C3S4H = nameAction "puppet_b2N3C3S4H" $ do
    slamInterest_
    minSuitLength T.Spades 3
    makeAlertableCall (T.Bid 4 T.Hearts) "slam interest in spades"


b2N3C3S4S :: Action
b2N3C3S4S = nameAction "puppet_b2N3C3S4S" $ do
    forbid slamInterest_
    minSuitLength T.Spades 3
    makeCall $ T.Bid 4 T.Spades


b2N3C3N :: Action
b2N3C3N = nameAction "puppet_b2N3C3N" $ do
    forEach T.majorSuits (`maxSuitLength` 3)
    makeAlertableCall (T.Bid 3 T.Notrump) "no 4-card major"
