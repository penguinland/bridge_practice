module Bids.PuppetStayman(
    noInterference
  , b2N  -- re-exported from StandardOpenings
  , b2N3C  -- TODO: do you bid this or a transfer if you're 5-4 in the majors?
  , b2N3C3D
  , b2N3C3D3H
  , b2N3C3D3H3N
  , b2N3C3D3H4S
  , b2N3C3D3S
  , b2N3C3D3S3N
  , b2N3C3D3S4H
  , b2N3C3D3N
  , b2N3C3D4D  -- TODO: when do you bid 4C, and when 4D?
  , b2N3C3D4D4H
  , b2N3C3D4D4S
  , b2N3C3H
  , b2N3C3H3S
  , b2N3C3H3N
  , b2N3C3H4H
  , b2N3C3S
  , b2N3C3S3N
  , b2N3C3S4H
  , b2N3C3S4S
  , b2N3C3N
  , b2N3C3N4D
  , b2N3C3N4D4H
  , b2N3C3N4H
  , b2N3C3N4H4S
) where


import Action(Action)
import qualified Bids.Meckwell as MW  -- Used in noInterference 
import Bids.NaturalOneNotrumpDefense(singleSuited, twoSuited)
import Bids.StandardOpenings(b2N)
import CommonBids(cannotPreempt)      -- Used in noInterference 
import EDSL(makeCall, makeAlertableCall, forbid, forbidAll, forEach,
            suitLength, minSuitLength, maxSuitLength, alternatives,
            nameAction, pointRange, maxLoserCount, strongerThan, impliesThat)
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


b2N3C :: Action
b2N3C = nameAction "puppet_b2N3C" $ do
    pointRange 5 40  -- game forcing
    alternatives [minSuitLength T.Hearts 3, minSuitLength T.Spades 3]
    -- If you could have made a transfer, prefer Puppet Stayman only if you've
    -- got both majors.
    minSuitLength T.Hearts 5 `impliesThat` minSuitLength T.Spades 4
    minSuitLength T.Spades 5 `impliesThat` minSuitLength T.Hearts 4
    -- Don't be interested in a minor to bid this.
    forbidAll [ singleSuited T.Clubs
              , singleSuited T.Diamonds
              , twoSuited T.Clubs T.Diamonds
              ]
    -- TODO: would you bid Puppet Stayman with a void? Not sure...
    makeCall $ T.Bid 3 T.Clubs


b2N3C3D :: Action
b2N3C3D = nameAction "puppet_b2N3C3D" $ do
    forbidAll [b2N3C3H, b2N3C3S]
    alternatives [suitLength T.Hearts 4, suitLength T.Spades 4]
    makeAlertableCall (T.Bid 3 T.Diamonds) "has at least one 4-card major"


b2N3C3D3H :: Action
b2N3C3D3H = nameAction "puppet_b2N3C3D3H" $ do
    minSuitLength T.Spades 4
    maxSuitLength T.Hearts 3
    makeAlertableCall (T.Bid 3 T.Hearts) "4+ spades, at most 3 hearts"


b2N3C3D3H3N :: Action
b2N3C3D3H3N = nameAction "puppet_b2N3C3D3H3N" $ do
    forbid b2N3C3D3H4S
    makeCall $ T.Bid 3 T.Notrump


b2N3C3D3H4S :: Action
b2N3C3D3H4S = nameAction "puppet_b2N3C3D3H4S" $ do
    minSuitLength T.Spades 4
    makeCall $ T.Bid 4 T.Spades


b2N3C3D3S :: Action
b2N3C3D3S = nameAction "puppet_b2N3C3D3S" $ do
    minSuitLength T.Hearts 4
    maxSuitLength T.Spades 3
    makeAlertableCall (T.Bid 3 T.Spades) "4+ hearts, at most 3 spades"


b2N3C3D3S3N :: Action
b2N3C3D3S3N = nameAction "puppet_b2N3C3D3S3N" $ do
    forbid b2N3C3D3S4H
    makeCall $ T.Bid 3 T.Notrump


b2N3C3D3S4H :: Action
b2N3C3D3S4H = nameAction "puppet_b2N3C3D3S4H" $ do
    minSuitLength T.Hearts 4
    makeCall $ T.Bid 4 T.Hearts


b2N3C3D3N :: Action
b2N3C3D3N = nameAction "puppet_b2N3C3D3N" $ do
    forEach T.majorSuits (`maxSuitLength` 3)
    makeCall $ T.Bid 3 T.Notrump


b2N3C3D4D :: Action
b2N3C3D4D = nameAction "puppet_b2N3C3D4D" $ do
    minSuitLength T.Hearts 4
    minSuitLength T.Spades 4
    makeAlertableCall (T.Bid 4 T.Diamonds) "at least 4-4 in the majors"


b2N3C3D4D4H :: Action
b2N3C3D4D4H = nameAction "puppet_b2N3C3D4D4H" $ do
    minSuitLength T.Hearts 4
    T.Hearts `strongerThan` T.Spades
    makeCall $ T.Bid 4 T.Hearts


b2N3C3D4D4S :: Action
b2N3C3D4D4S = nameAction "puppet_b2N3C3D4D4S" $ do
    minSuitLength T.Spades 4
    T.Spades `strongerThan` T.Hearts
    makeCall $ T.Bid 4 T.Spades


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


b2N3C3N4D :: Action
b2N3C3N4D = nameAction "puppet_b2N3C3N4D" $ do
    minSuitLength T.Hearts 6
    makeAlertableCall (T.Bid 4 T.Diamonds) "transfer to hearts"


b2N3C3N4D4H :: Action
b2N3C3N4D4H = nameAction "puppet_b2N3C3N4D4H" $ makeCall $ T.Bid 4 T.Hearts


b2N3C3N4H :: Action
b2N3C3N4H = nameAction "puppet_b2N3C3N4H" $ do
    minSuitLength T.Spades 6
    makeAlertableCall (T.Bid 4 T.Hearts) "transfer to spades"


b2N3C3N4H4S :: Action
b2N3C3N4H4S = nameAction "puppet_b2N3C3N4H4S" $ makeCall $ T.Bid 4 T.Spades
