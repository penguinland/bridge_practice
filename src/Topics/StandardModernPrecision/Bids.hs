module Topics.StandardModernPrecision.Bids(
    firstSeatOpener
  , oppsPass
  -- Opening bids
  , b1C
  , b1D
  , b1M
  , b1N
  , b2C
  , b2D
  , b2N
  , b3N
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
  , b1C1D1S1N
  , b1C1D1S2C
  , b1C1D1S2D
  , b1C1D1S2H
  , b1C1D1S2S
  -- TODO: jumps and double jumps in MaFiA
  -- syntactic sugar
  , smpWrapN
  , smpWrapS
) where

import System.Random(StdGen)

import Topic(wrap, Situations)
import Auction(forbid, pointRange, suitLength, minSuitLength, maxSuitLength,
               Action, balancedHand, constrain, makeCall, makePass,
               alternatives, SuitLengthComparator(..), compareSuitLength)
import Situation(Situation, (<~))
import CommonBids(cannotPreempt)
import qualified Terminology as T


-- Because we're not using the Rule of 20 and its ilk, we're going to skip the
-- auctions that start with other folks passing for now, and maybe come back to
-- those later.
firstSeatOpener :: Action
firstSeatOpener = do
    pointRange 11 40  -- Open any good 10 count, too. but that's hard to codify


oppsPass :: Action
oppsPass = do
    cannotPreempt
    makePass


------------------
-- Opening bids --
------------------
b1N :: Action
b1N = do
    pointRange 14 16
    balancedHand
    makeCall $ T.Bid 1 T.Notrump


b2N :: Action
b2N = do
    pointRange 19 21  -- A modification from Part 1: it's really 19 to a bad 21
    balancedHand
    makeCall $ T.Bid 2 T.Notrump


b3N :: Action
b3N = do
    pointRange 25 40  -- Technically only 25-27, but close enough
    balancedHand
    makeCall $ T.Bid 3 T.Notrump


b1C :: Action
b1C = do
    pointRange 16 40
    sequence_ . map forbid $ [b1N, b2N, b3N]
    makeCall $ T.Bid 1 T.Clubs


b1M :: T.Suit -> Action
b1M suit = do
    forbid b1C
    forbid b1N
    minSuitLength suit 5
    makeCall $ T.Bid 1 suit


b2C :: Action
b2C = do
    forbid b1C
    forbid (b1M T.Hearts)
    forbid (b1M T.Spades)
    minSuitLength T.Clubs 6
    makeCall $ T.Bid 2 T.Clubs


b2D :: Action
b2D = do
    forbid b1C
    constrain "two_diamond_opener" ["shape(", ", 4414 + 4405 + 4315 + 3415)"]
    makeCall $ T.Bid 2 T.Diamonds


b1D :: Action
b1D = do
    sequence_ . map forbid $ [ b1C
                             , b1N
                             , b1M T.Hearts
                             , b1M T.Spades
                             , b2C
                             , b2D
                             ]
    -- The next line is commented out because if it can be violated, we're gonna
    -- have a bad day. Make sure that it's never violated in the results even if
    -- it's not explicitly required.
    --minSuitLength T.Diamonds 2
    makeCall $ T.Bid 1 T.Diamonds


---------------------
-- Responses to 1C --
---------------------
b1C1D :: Action
b1C1D = do
    pointRange 0 7
    makeCall $ T.Bid 1 T.Diamonds

_gameForcing :: Action
_gameForcing = pointRange 8 11

_slamInterest :: Action
_slamInterest = pointRange 12 40


b1C1H :: Action
b1C1H = do
    _gameForcing
    makeCall $ T.Bid 1 T.Hearts


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
    sequence_ . map (\s -> maxSuitLength s 4) $ T.allSuits
    makeCall $ T.Bid 1 T.Notrump


b1C2S :: Action
b1C2S = do
    _slamInterest
    constrain "triple41" ["shape(", ", 4441 + 4414 + 4144 + 1444)"]
    makeCall $ T.Bid 2 T.Spades


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
    sequence_ . map (\s -> maxSuitLength s 4) $ T.allSuits
    makeCall $ T.Bid 1 T.Notrump


bP1C2S :: Action
bP1C2S = do
    _gameForcing
    constrain "triple41" ["shape(", ", 4441 + 4414 + 4144 + 1444)"]
    makeCall $ T.Bid 2 T.Spades


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
    pointRange 22 24
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
    sequence_ . map (forbid . compareSuitLength suit Shorter) .
                filter (/= suit) $ T.allSuits
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
    pointRange 18 19
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


b1C1D1H1S :: Action
b1C1D1H1S = do
    forbid b1C1D1H2H
    minSuitLength T.Spades 4


b1C1D1H1N :: Action
b1C1D1H1N = do
    forbid b1C1D1H2H
    forbid b1C1D1H1S
    pointRange 0 5


b1C1D1H2D :: Action
b1C1D1H2D = do
    forbid b1C1D1H2H
    forbid b1C1D1H1S
    forbid b1C1D1H1N
    suitLength T.Hearts 3
    -- This next line is redundant with forbidding a 1N rebid, but it's nice to
    -- be explicit about it.
    pointRange 6 7


b1C1D1H2C :: Action
b1C1D1H2C = do
    forbid b1C1D1H2H
    forbid b1C1D1H1S
    forbid b1C1D1H1N
    forbid b1C1D1H2D
    pointRange 6 7  -- Redundant with forbidding a 1N rebid, but explicit


b1C1D1S2S :: Action
b1C1D1S2S = do
    minSuitLength T.Spades 4
    maxSuitLength T.Spades 5
    pointRange 0 4


b1C1D1S2H :: Action
b1C1D1S2H = do
    forbid b1C1D1S2S
    pointRange 6 7
    -- We need at least 5 hearts, since opener has at most 3 (unless they're
    -- two-suited with both majors and longer spades, but that's rare and we'll
    -- figure it out next bid anyway).
    minSuitLength T.Hearts 5


b1C1D1S1N :: Action
b1C1D1S1N = do
    forbid b1C1D1S2S
    forbid b1C1D1S2H
    -- Note that we might have 5+ hearts, but are too weak to show it.
    pointRange 0 5


b1C1D1S2D :: Action
b1C1D1S2D = do
    forbid b1C1D1S2S
    forbid b1C1D1S2H
    forbid b1C1D1S1N
    pointRange 6 7 -- Redundant with forbidding a 1N rebid, but explicit
    suitLength T.Spades 3


b1C1D1S2C :: Action
b1C1D1S2C = do
    forbid b1C1D1S2S
    forbid b1C1D1S2H
    forbid b1C1D1S1N
    forbid b1C1D1S2D
    pointRange 6 7 -- Redundant with forbidding a 1N rebid, but explicit


-------------------------------
-- Situation syntactic sugar --
-------------------------------
-- Always make opener be in first seat, until we figure out how to open in other
-- seats.
-- TODO: change this to let other folks be dealer, too
smpWrapS :: (StdGen -> T.Vulnerability -> T.Direction -> Situation) ->
            Situations
smpWrapS sit = wrap $ sit <~ T.allVulnerabilities <~ [T.South]

smpWrapN :: (StdGen -> T.Vulnerability -> T.Direction -> Situation) ->
            Situations
smpWrapN sit = wrap $ sit <~ T.allVulnerabilities <~ [T.North]
