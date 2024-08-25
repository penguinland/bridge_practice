module Bids.StandardModernPrecision.OneClub(
    b1C  -- Copied from BasicBids
  , b1C1D
  , startOfMafia
  , b1C1D1H
  , b1C1D1H1S
  , b1C1D1H1N
  , b1C1D1H2C
  , b1C1D1H2D
  , b1C1D1H2H
  , b1C1D1H2N
  , b1C1D1H3H
  -- TODO: jumps and double jumps in MaFiA
  , b1C1D1S
  , b1C1D1S1N
  , b1C1D1S2C
  , b1C1D1S2D
  , b1C1D1S2H
  , b1C1D1S2S
  , b1C1D1S2N
  , b1C1D1S3S
  , b1C1D1N
  , b1C1D2C
  , b1C1D2D
  , b1C1D2H
  , b1C1D2S
  , b1C1D2N
  , b1C1D3C
  , b1C1D3D
  , b1C1H
  , b1C1Hnos
  , b1C1H1S
  , b1C1H1S3C
  , b1C1H1N
  , b1C1H2C
  , b1C1H2C3D
  , b1C1H2D
  , b1C1H2D3H
  , b1C1H2H
  , b1C1H2H3S
  , b1C1H2S
  , b1C1H2S2N
  , b1C1H2S2N3C
  , b1C1H2S2N3D
  , b1C1H2S2N3H
  , b1C1H2S2N3S
  , b1C1H2N
  , b1C1H3N
  , b1C1S
  , b1C1Sgf
  , b1C1S3C
  , b1C1N
  , b1C1Nalt
  , b1C2C
  , b1C2C3D
  , b1C2D
  , b1C2D3H
  , b1C2H
  , b1C2Halt
  , b1C2H3S
  , b1C2S
  , b1C2S2N
  , b1C2S2N3C
  , b1C2S2N3D
  , b1C2S2N3H
  , b1C2S2N3S
  , b1C2Nalt
  , bP1C1H
  , bP1C1S
  , bP1C1N
  , bP1C2C
  , bP1C2D
  , bP1C2S  -- Note that the auction P-1C-2H cannot occur!
  , tripleFourOneShape  -- For use when defining other bids
) where

import Action(Action, constrain)
import Bids.StandardModernPrecision.BasicBids(b1C, oppsPass)
import EDSL(forbid, pointRange, suitLength, minSuitLength, maxSuitLength,
            balancedHand, makeCall, makeAlertableCall, alternatives, longerThan,
            atLeastAsLong, forEach, forbidAll, maxLoserCount, minLoserCount)
import Output((.+), Punct(..))
import qualified Terminology as T


b1C1D :: Action
b1C1D = do
    pointRange 0 7
    makeAlertableCall (T.Bid 1 T.Diamonds) ("0" .+ NDash .+ "7 HCP, any shape")


_gameForcing :: Action
_gameForcing = pointRange 8 11

_slamInterest :: Action
_slamInterest = pointRange 12 40


tripleFourOneShape :: Action
tripleFourOneShape = constrain "any4441" ["shape(", ", any 4441)"]


b1C1H :: Action
b1C1H = do
    _gameForcing
    makeAlertableCall (T.Bid 1 T.Hearts) ("8" .+ NDash .+ "11 HCP, any shape")


b1C1Hnos :: Action
b1C1Hnos = do
    _gameForcing
    maxSuitLength T.Spades 4
    makeAlertableCall (T.Bid 1 T.Hearts)
                      ("8" .+ NDash .+ "11 HCP, any shape without 5+ spades")


b1C1S :: Action
b1C1S = do
    _slamInterest
    minSuitLength T.Spades 5
    makeCall $ T.Bid 1 T.Spades


b1C1Sgf :: Action
b1C1Sgf = do
    pointRange 8 40
    minSuitLength T.Spades 5
    makeCall $ T.Bid 1 T.Spades


b1C2C :: Action
b1C2C = do
    _slamInterest
    minSuitLength T.Clubs 5
    makeCall $ T.Bid 2 T.Clubs


b1C2D :: Action
b1C2D = do
    _slamInterest
    minSuitLength T.Diamonds 5
    makeCall $ T.Bid 2 T.Diamonds


b1C2H :: Action
b1C2H = do
    _slamInterest
    minSuitLength T.Hearts 5
    makeCall $ T.Bid 2 T.Hearts


b1C2Halt :: Action  -- Alternative choice: swap the meanings of b1C1N and b1C2H
b1C2Halt = do
    pointRange 14 40
    balancedHand
    forEach T.allSuits (`maxSuitLength` 4)
    makeAlertableCall (T.Bid 2 T.Hearts) "14+ HCP, any 4333 or 4432 shape"


b1C1N :: Action
b1C1N = do
    _slamInterest
    balancedHand
    forEach T.allSuits (`maxSuitLength` 4)
    makeCall $ T.Bid 1 T.Notrump


b1C1Nalt :: Action  -- Alternative choice: swap the meanings of b1C1N and b1C2H
b1C1Nalt = do
    _slamInterest
    minSuitLength T.Hearts 5
    makeAlertableCall (T.Bid 1 T.Notrump) "12+ HCP, 5+ hearts"


b1C2S :: Action
b1C2S = do
    _slamInterest
    tripleFourOneShape
    makeAlertableCall (T.Bid 2 T.Spades) "12+ HCP, any 4441 shape"


b1C2Nalt :: Action
b1C2Nalt = do
    pointRange 12 13
    balancedHand
    forEach T.allSuits (`maxSuitLength` 4)
    makeAlertableCall (T.Bid 2 T.Notrump)
                      ("12" .+ NDash .+ "13 HCP, any 4333 or 4432 shape")


bP1C1H :: Action
bP1C1H = do
    _gameForcing
    minSuitLength T.Hearts 5
    makeCall $ T.Bid 1 T.Hearts


bP1C1S :: Action
bP1C1S = do
    _gameForcing
    minSuitLength T.Spades 5
    makeCall $ T.Bid 1 T.Spades


bP1C2C :: Action
bP1C2C = do
    _gameForcing
    minSuitLength T.Clubs 5
    makeCall $ T.Bid 2 T.Clubs


bP1C2D :: Action
bP1C2D = do
    _gameForcing
    minSuitLength T.Diamonds 5
    makeCall $ T.Bid 2 T.Diamonds


bP1C1N :: Action
bP1C1N = do
    _gameForcing
    balancedHand
    forEach T.allSuits (`maxSuitLength` 4)
    makeCall $ T.Bid 1 T.Notrump


bP1C2S :: Action
bP1C2S = do
    _gameForcing
    tripleFourOneShape
    makeAlertableCall (T.Bid 2 T.Spades) "game forcing, any 4441 shape"


-----------
-- MaFiA --
-----------
startOfMafia :: Action
startOfMafia = do
    b1C
    oppsPass
    b1C1D
    oppsPass


-- Do the game-forcing bids first
b1C1D2N :: Action
b1C1D2N = do
    pointRange 21 23
    balancedHand
    makeCall $ T.Bid 2 T.Notrump


_makeJumpBid :: Int -> T.Suit -> Action
_makeJumpBid level suit = do
    pointRange 22 40
    forbid b1C1D2N
    -- Forbidding balanced hands should be redundant: the 1C bid forbade a 2N or
    -- 3N opener, the point range forbids a 1N rebid, and we've just forbidden a
    -- 2N rebid. but it can't hurt to make this constraint explicit (for
    -- instance, I had not noticed until adding it that we hadn't precluded a 3N
    -- opener in the 1C bid).
    forbid balancedHand
    minSuitLength suit 5
    -- This should be your longest suit
    -- TODO: if you're 5-5, which suit do you bid first?
    forEach (filter (/= suit) T.allSuits) (suit `longerThan`)
    makeCall $ T.Bid level suit


b1C1D2H :: Action
b1C1D2H = _makeJumpBid 2 T.Hearts


b1C1D2S :: Action
b1C1D2S = _makeJumpBid 2 T.Spades


b1C1D3C :: Action
b1C1D3C = _makeJumpBid 3 T.Clubs


b1C1D3D :: Action
b1C1D3D = _makeJumpBid 3 T.Diamonds


_notGameForcing :: Action
_notGameForcing = forbidAll [b1C1D2N, b1C1D2H, b1C1D2S, b1C1D3C, b1C1D3D]


b1C1D1N :: Action
b1C1D1N = do
    pointRange 17 18
    balancedHand
    makeCall $ T.Bid 1 T.Notrump


b1C1D1H :: Action
b1C1D1H = do
    forbid b1C1D1N
    _notGameForcing
    minSuitLength T.Hearts 4
    T.Hearts `atLeastAsLong` T.Spades
    -- If you're 4-4 in the majors, start with 1H, and you'll find your spade
    -- fit if partner doesn't show a heart fit. but if you're 5-5 in the majors,
    -- start with 1S, planning to rebid 2H. If you were thinking of reversing,
    -- you're strong enough to jump straight to 2S and rebid 3H, instead of
    -- bidding at the 1 level.
    maxSuitLength T.Spades 4
    makeCall $ T.Bid 1 T.Hearts


b1C1D1S :: Action
b1C1D1S = do
    forbid b1C1D1N
    forbid b1C1D1H
    _notGameForcing
    minSuitLength T.Spades 4
    makeCall $ T.Bid 1 T.Spades


b1C1D2C :: Action
b1C1D2C = do
    forbidAll [b1C1D1N, b1C1D1H, b1C1D1S]
    _notGameForcing
    -- Clubs must be longer than diamonds: with equal lengths, bid diamonds.
    T.Clubs `longerThan` T.Diamonds
    alternatives [ minSuitLength T.Clubs 6
                 , minSuitLength T.Clubs 5 >> minSuitLength T.Diamonds 4
                 ]
    makeCall $ T.Bid 2 T.Clubs


b1C1D2D :: Action
b1C1D2D = do
    forbidAll [b1C1D1N, b1C1D1H, b1C1D1S, b1C1D2C]
    _notGameForcing
    T.Diamonds `atLeastAsLong` T.Clubs
    alternatives [ minSuitLength T.Diamonds 6
                 , minSuitLength T.Diamonds 5 >> minSuitLength T.Clubs 4
                 ]
    makeCall $ T.Bid 2 T.Diamonds

-------------------------------
-- MaFiA rebids by responder --
-------------------------------
b1C1D1H2H :: Action
b1C1D1H2H = do
    minSuitLength T.Hearts 4
    maxSuitLength T.Hearts 5
    alternatives [pointRange 0 4, pointRange 0 5 >> minLoserCount 11]
    makeCall $ T.Bid 2 T.Hearts


b1C1D1H3H :: Action
b1C1D1H3H = do
    minSuitLength T.Hearts 4
    pointRange 5 7
    forEach T.allSuits (`minSuitLength` 2)
    maxLoserCount 10
    makeCall $ T.Bid 3 T.Hearts


b1C1D1H2N :: Action
b1C1D1H2N = do
    suitLength T.Hearts 4
    pointRange 5 7
    -- The 2N bid is for hands with a singleton or void, so forbid the
    -- semibalanced response.
    forbid b1C1D1H3H
    maxLoserCount 10
    makeAlertableCall (T.Bid 2 T.Notrump)
                      ("5" .+ NDash .+ "7 HCP, undisclosed splinter for hearts")


b1C1D1H1S :: Action
b1C1D1H1S = do
    forbid b1C1D1H2H
    forbid b1C1D1H3H
    minSuitLength T.Spades 4
    makeCall $ T.Bid 1 T.Spades


b1C1D1H1N :: Action
b1C1D1H1N = do
    forbidAll [b1C1D1H2H, b1C1D1H3H, b1C1D1H1S]
    pointRange 0 5
    makeAlertableCall (T.Bid 1 T.Notrump)
                      ("0" .+ NDash .+ "5 HCP, at most 3 hearts")


b1C1D1H2D :: Action
b1C1D1H2D = do
    forbidAll [b1C1D1H2H, b1C1D1H3H, b1C1D1H1S, b1C1D1H1N]
    suitLength T.Hearts 3
    -- This next line is redundant with forbidding a 1N rebid, but it's nice to
    -- be explicit about it.
    pointRange 6 7
    makeAlertableCall (T.Bid 2 T.Diamonds)
                      ("6" .+ NDash .+ "7 HCP, 3-card heart support")


b1C1D1H2C :: Action
b1C1D1H2C = do
    forbidAll [b1C1D1H2H, b1C1D1H3H, b1C1D1H1S, b1C1D1H1N, b1C1D1H2D]
    -- There are other splinter bids that haven't been created yet let alone
    -- forbidden here, so just explicitly point out the maximum lengths of the
    -- majors.
    maxSuitLength T.Hearts 2
    maxSuitLength T.Spades 3
    pointRange 6 7  -- Redundant with forbidding a 1N rebid, but explicit
    makeAlertableCall (T.Bid 2 T.Clubs)
                      ("6" .+ NDash .+ "7 HCP, at most 2 hearts and 3 spades")


b1C1D1S2S :: Action
b1C1D1S2S = do
    minSuitLength T.Spades 4
    maxSuitLength T.Spades 5
    alternatives [pointRange 0 4, pointRange 0 5 >> minLoserCount 11]
    makeCall $ T.Bid 2 T.Spades


b1C1D1S3S :: Action
b1C1D1S3S = do
    minSuitLength T.Spades 4
    pointRange 5 7
    forEach T.allSuits (`minSuitLength` 2)
    maxLoserCount 10
    makeCall $ T.Bid 3 T.Spades


b1C1D1S2N :: Action
b1C1D1S2N = do
    suitLength T.Spades 4
    pointRange 5 7
    -- The 2N bid is for hands with a singleton or void, so forbid the
    -- semibalanced response.
    forbid b1C1D1S3S
    maxLoserCount 10
    makeAlertableCall (T.Bid 2 T.Notrump)
                      ("5" .+ NDash .+ "7 HCP, undisclosed splinter for spades")


b1C1D1S1N :: Action
b1C1D1S1N = do
    forbid b1C1D1S2S
    forbid b1C1D1S3S
    -- Note that we might have 5+ hearts, but are too weak to show it.
    pointRange 0 5
    makeAlertableCall (T.Bid 1 T.Notrump)
                      ("0" .+ NDash .+ "5 HCP, at most 3 spades")


b1C1D1S2D :: Action
b1C1D1S2D = do
    forbidAll [b1C1D1S2S, b1C1D1S3S, b1C1D1S1N]
    pointRange 6 7 -- Redundant with forbidding a 1N rebid, but explicit
    suitLength T.Spades 3
    makeAlertableCall (T.Bid 2 T.Diamonds)
                      ("6" .+ NDash .+ "7 HCP, 3-card spade support")


b1C1D1S2H :: Action
b1C1D1S2H = do
    -- Prefer showing 3-card spade support over your own 5-card heart suit.
    forbidAll [b1C1D1S2S, b1C1D1S3S, b1C1D1S2D, b1C1D1S2N]
    pointRange 6 7
    -- We need at least 5 hearts, since opener has at most 3 (unless they're
    -- two-suited with both majors and longer spades, but that's rare and we'll
    -- figure it out next bid anyway).
    minSuitLength T.Hearts 5
    makeCall $ T.Bid 2 T.Hearts


b1C1D1S2C :: Action
b1C1D1S2C = do
    forbidAll [b1C1D1S2S, b1C1D1S3S, b1C1D1S2H, b1C1D1S1N, b1C1D1S2D]
    -- There are other splinter bids that haven't been created yet let alone
    -- forbidden here, so just explicitly point out the maximum lengths of the
    -- majors.
    maxSuitLength T.Hearts 4
    maxSuitLength T.Spades 2
    pointRange 6 7 -- Redundant with forbidding a 1N rebid, but explicit
    makeAlertableCall (T.Bid 2 T.Clubs)
                      ("6" .+ NDash .+ "7 HCP, at most 2 spades")


---------------------
-- Rebids after 1H --
---------------------
b1C1H1S :: Action
b1C1H1S = do
    forbidAll [b1C1H1N, b1C1H2N, b1C1H3N]
    minSuitLength T.Spades 5
    T.Spades `atLeastAsLong` T.Clubs
    T.Spades `atLeastAsLong` T.Diamonds
    T.Spades `atLeastAsLong` T.Hearts
    makeCall (T.Bid 1 T.Spades)


b1C1H1N :: Action
b1C1H1N = do
    balancedHand
    pointRange 17 18
    makeCall (T.Bid 1 T.Notrump)


b1C1H2N :: Action
b1C1H2N = do
    balancedHand
    pointRange 21 23
    makeCall (T.Bid 2 T.Notrump)


-- TODO: is this right? Maybe this shouldn't exist at all and be rolled into the
-- 2N rebid. We're already in a game-forcing auction, after all.
b1C1H3N :: Action
b1C1H3N = do
    balancedHand
    pointRange 24 40
    makeCall (T.Bid 3 T.Notrump)


b1C1H2C :: Action
b1C1H2C = do
    forbidAll [b1C1H1N, b1C1H2N, b1C1H3N]
    minSuitLength T.Clubs 5
    -- Given 5-5 in the minors, start with diamonds, and bid clubs later.
    T.Clubs `longerThan` T.Diamonds
    T.Clubs `longerThan` T.Spades
    T.Clubs `longerThan` T.Hearts
    makeCall (T.Bid 2 T.Clubs)


b1C1H2D :: Action
b1C1H2D = do
    forbidAll [b1C1H1N, b1C1H2N, b1C1H3N]
    minSuitLength T.Diamonds 5
    T.Diamonds `atLeastAsLong` T.Clubs
    T.Diamonds `longerThan` T.Hearts
    T.Diamonds `longerThan` T.Spades
    makeCall (T.Bid 2 T.Diamonds)


b1C1H2H :: Action
b1C1H2H = do
    forbidAll [b1C1H1N, b1C1H2N, b1C1H3N]
    minSuitLength T.Hearts 5
    T.Hearts `atLeastAsLong` T.Clubs
    T.Hearts `atLeastAsLong` T.Diamonds
    T.Hearts `longerThan` T.Spades
    makeCall (T.Bid 2 T.Hearts)


b1C1H2S :: Action
b1C1H2S = do
    tripleFourOneShape
    makeAlertableCall (T.Bid 2 T.Spades) "any 4441 hand"


b1C1H2S2N :: Action
b1C1H2S2N = makeAlertableCall (T.Bid 2 T.Notrump) "what is your singleton?"


b1C1H2S2N3C :: Action
b1C1H2S2N3C = do
    suitLength T.Clubs 1
    makeAlertableCall (T.Bid 3 T.Clubs) "singleton club"


b1C1H2S2N3D :: Action
b1C1H2S2N3D = do
    suitLength T.Diamonds 1
    makeAlertableCall (T.Bid 3 T.Diamonds) "singleton diamond"


b1C1H2S2N3H :: Action
b1C1H2S2N3H = do
    suitLength T.Hearts 1
    makeAlertableCall (T.Bid 3 T.Hearts) "singleton heart"


b1C1H2S2N3S :: Action
b1C1H2S2N3S = do
    suitLength T.Spades 1
    makeAlertableCall (T.Bid 3 T.Spades) "singleton spade"


b1C2S2N :: Action
b1C2S2N = b1C1H2S2N

b1C2S2N3C :: Action
b1C2S2N3C = b1C1H2S2N3C

b1C2S2N3D :: Action
b1C2S2N3D = b1C1H2S2N3D

b1C2S2N3H :: Action
b1C2S2N3H = b1C1H2S2N3H

b1C2S2N3S :: Action
b1C2S2N3S = b1C1H2S2N3S


-- Cheapest jump-shift showing 4441 with a singleton in partner's suit

b1C1H1S3C :: Action
b1C1H1S3C = do
    tripleFourOneShape
    suitLength T.Spades 1
    makeAlertableCall (T.Bid 3 T.Clubs) "1444 shape with singleton spade"


b1C1H2C3D :: Action
b1C1H2C3D = do
    tripleFourOneShape
    suitLength T.Clubs 1
    makeAlertableCall (T.Bid 3 T.Diamonds) "4441 shape with singleton club"


b1C1H2D3H :: Action
b1C1H2D3H = do
    tripleFourOneShape
    suitLength T.Diamonds 1
    makeAlertableCall (T.Bid 3 T.Hearts) "4414 shape with singleton diamond"


b1C1H2H3S :: Action
b1C1H2H3S = do
    tripleFourOneShape
    suitLength T.Hearts 1
    makeAlertableCall (T.Bid 3 T.Spades) "4144 shape with singleton heart"


b1C1S3C :: Action
b1C1S3C = b1C1H1S3C

b1C2C3D :: Action
b1C2C3D = b1C1H2C3D

b1C2D3H :: Action
b1C2D3H = b1C1H2D3H

-- TODO: if you're using the alternative responses, how do you show a 4144 shape
-- over 1C-1N? Presumably it's 1C-1N-3C. Is it worth having a topic just for
-- this one change?
b1C2H3S :: Action
b1C2H3S = b1C1H2H3S
