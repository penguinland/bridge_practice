module Topics.StandardModernPrecision.Bids1C(
    b1C  -- Copied from BasicBids
  -- Responses to 1C
  , b1C1D
  , b1C1H
  , b1C1S
  , b1C1N
  , b1C2C
  , b1C2D
  , b1C2H
  , b1C2S
  , bP1C1H
  , bP1C1S
  , bP1C1N
  , bP1C2C
  , bP1C2D
  , bP1C2S  -- Note that the auction P-1C-2H cannot occur!
  -- rebids after 1C-1D
  , startOfMafia
  , b1C1D1H
  , b1C1D1S
  , b1C1D1N
  , b1C1D2C
  , b1C1D2D
  , b1C1D2H
  , b1C1D2S
  , b1C1D2N
  , b1C1D3C
  , b1C1D3D
  -- rebids after 1C-1D-1M
  , b1C1D1H1S
  , b1C1D1H1N
  , b1C1D1H2C
  , b1C1D1H2D
  , b1C1D1H2H
  , b1C1D1H2N
  , b1C1D1H3H
  , b1C1D1S1N
  , b1C1D1S2C
  , b1C1D1S2D
  , b1C1D1S2H
  , b1C1D1S2S
  , b1C1D1S2N
  , b1C1D1S3S
  -- TODO: jumps and double jumps in MaFiA
) where

import Auction(forbid, pointRange, suitLength, minSuitLength, maxSuitLength,
               Action, balancedHand, constrain, makeCall, makeAlertableCall,
               alternatives, SuitLengthComparator(..), compareSuitLength)
import qualified Terminology as T
import Topics.StandardModernPrecision.BasicBids(b1C, firstSeatOpener, oppsPass)


b1C1D :: Action
b1C1D = do
    pointRange 0 7
    makeAlertableCall (T.Bid 1 T.Diamonds) "0-7 HCP, any shape"


_gameForcing :: Action
_gameForcing = pointRange 8 11

_slamInterest :: Action
_slamInterest = pointRange 12 40


b1C1H :: Action
b1C1H = do
    _gameForcing
    makeAlertableCall (T.Bid 1 T.Hearts) "8-11 HCP, any shape"


b1C1S :: Action
b1C1S = do
    _slamInterest
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


b1C1N :: Action
b1C1N = do
    _slamInterest
    balancedHand
    mapM_ (\s -> maxSuitLength s 4) T.allSuits
    makeCall $ T.Bid 1 T.Notrump


b1C2S :: Action
b1C2S = do
    _slamInterest
    constrain "triple41" ["shape(", ", 4441 + 4414 + 4144 + 1444)"]
    makeAlertableCall (T.Bid 2 T.Spades) "12+ HCP, any 4441 shape"


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
    mapM_ (\s -> maxSuitLength s 4) T.allSuits
    makeCall $ T.Bid 1 T.Notrump


bP1C2S :: Action
bP1C2S = do
    _gameForcing
    constrain "triple41" ["shape(", ", 4441 + 4414 + 4144 + 1444)"]
    makeAlertableCall (T.Bid 2 T.Spades) "Game forcing, any 4441 shape"


-----------
-- MaFiA --
-----------
startOfMafia :: Action
startOfMafia = do
    firstSeatOpener
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
    mapM_ (forbid . compareSuitLength suit Shorter) . filter (/= suit) $
        T.allSuits
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
_notGameForcing = do
    forbid b1C1D2N
    forbid b1C1D2H
    forbid b1C1D2S
    forbid b1C1D3C
    forbid b1C1D3D


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
    forbid b1C1D1N
    forbid b1C1D1H
    forbid b1C1D1S
    _notGameForcing
    -- Clubs must be longer than diamonds: with equal lengths, bid diamonds.
    compareSuitLength T.Clubs Longer T.Diamonds
    alternatives [ minSuitLength T.Clubs 6
                 , minSuitLength T.Clubs 5 >> minSuitLength T.Diamonds 4
                 ]
    makeCall $ T.Bid 2 T.Clubs


b1C1D2D :: Action
b1C1D2D = do
    forbid b1C1D1N
    forbid b1C1D1H
    forbid b1C1D1S
    forbid b1C1D2C
    _notGameForcing
    -- diamonds are at least as long as clubs
    forbid $ compareSuitLength T.Diamonds Shorter T.Clubs
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
    pointRange 0 4
    makeCall $ T.Bid 2 T.Hearts


b1C1D1H3H :: Action
b1C1D1H3H = do
    suitLength T.Hearts 4
    pointRange 5 7
    mapM_ (\s -> minSuitLength s 2) T.allSuits
    makeCall $ T.Bid 3 T.Hearts


b1C1D1H2N :: Action
b1C1D1H2N = do
    suitLength T.Hearts 4
    pointRange 5 7
    -- The 2N bid is for hands with a singleton or void, so forbid the
    -- semibalanced response.
    forbid b1C1D1H3H
    makeAlertableCall (T.Bid 2 T.Notrump)
                      "5-7 HCP, undisclosed splinter for hearts"


b1C1D1H1S :: Action
b1C1D1H1S = do
    forbid b1C1D1H2H
    forbid b1C1D1H3H
    minSuitLength T.Spades 4
    makeCall $ T.Bid 1 T.Spades


b1C1D1H1N :: Action
b1C1D1H1N = do
    forbid b1C1D1H2H
    forbid b1C1D1H3H
    forbid b1C1D1H1S
    pointRange 0 5
    makeAlertableCall (T.Bid 1 T.Notrump) "0-5 HCP, at most 3 hearts"


b1C1D1H2D :: Action
b1C1D1H2D = do
    forbid b1C1D1H2H
    forbid b1C1D1H3H
    forbid b1C1D1H1S
    forbid b1C1D1H1N
    suitLength T.Hearts 3
    -- This next line is redundant with forbidding a 1N rebid, but it's nice to
    -- be explicit about it.
    pointRange 6 7
    makeAlertableCall (T.Bid 2 T.Diamonds) "6-7 HCP, 3-card heart support"


b1C1D1H2C :: Action
b1C1D1H2C = do
    forbid b1C1D1H2H
    forbid b1C1D1H3H
    forbid b1C1D1H1S
    forbid b1C1D1H1N
    forbid b1C1D1H2D
    -- There are other splinter bids that haven't been created yet let alone
    -- forbidden here, so just explicitly point out the maximum lengths of the
    -- majors.
    maxSuitLength T.Hearts 2
    maxSuitLength T.Spades 3
    pointRange 6 7  -- Redundant with forbidding a 1N rebid, but explicit
    makeAlertableCall (T.Bid 2 T.Clubs) "6-7 HCP, at most 2 hearts and 3 spades"


b1C1D1S2S :: Action
b1C1D1S2S = do
    minSuitLength T.Spades 4
    maxSuitLength T.Spades 5
    pointRange 0 4
    makeCall $ T.Bid 2 T.Spades


b1C1D1S3S :: Action
b1C1D1S3S = do
    suitLength T.Spades 4
    pointRange 5 7
    mapM_ (\s -> minSuitLength s 2) T.allSuits
    makeCall $ T.Bid 3 T.Spades


b1C1D1S2N :: Action
b1C1D1S2N = do
    suitLength T.Spades 4
    pointRange 5 7
    -- The 2N bid is for hands with a singleton or void, so forbid the
    -- semibalanced response.
    forbid b1C1D1S3S
    makeAlertableCall (T.Bid 2 T.Notrump)
                      "5-7 HCP, undisclosed splinter for spades"


b1C1D1S1N :: Action
b1C1D1S1N = do
    forbid b1C1D1S2S
    forbid b1C1D1S3S
    -- Note that we might have 5+ hearts, but are too weak to show it.
    pointRange 0 5
    makeAlertableCall (T.Bid 1 T.Notrump) "0-5 HCP, at most 3 spades"


b1C1D1S2D :: Action
b1C1D1S2D = do
    forbid b1C1D1S2S
    forbid b1C1D1S3S
    forbid b1C1D1S1N
    pointRange 6 7 -- Redundant with forbidding a 1N rebid, but explicit
    suitLength T.Spades 3
    makeAlertableCall (T.Bid 2 T.Diamonds) "6-7 HCP, 3-card spade support"


b1C1D1S2H :: Action
b1C1D1S2H = do
    forbid b1C1D1S2S
    forbid b1C1D1S3S
    -- Prefer showing 3-card spade support over your own 5-card heart suit.
    forbid b1C1D1S2D
    pointRange 6 7
    -- We need at least 5 hearts, since opener has at most 3 (unless they're
    -- two-suited with both majors and longer spades, but that's rare and we'll
    -- figure it out next bid anyway).
    minSuitLength T.Hearts 5
    makeCall $ T.Bid 2 T.Hearts


b1C1D1S2C :: Action
b1C1D1S2C = do
    forbid b1C1D1S2S
    forbid b1C1D1S3S
    forbid b1C1D1S2H
    forbid b1C1D1S1N
    forbid b1C1D1S2D
    -- There are other splinter bids that haven't been created yet let alone
    -- forbidden here, so just explicitly point out the maximum lengths of the
    -- majors.
    maxSuitLength T.Hearts 4
    maxSuitLength T.Spades 2
    pointRange 6 7 -- Redundant with forbidding a 1N rebid, but explicit
    makeAlertableCall (T.Bid 2 T.Clubs) "6-7 HCP, at most 2 spades"
