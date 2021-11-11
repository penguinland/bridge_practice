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
  -- syntactic sugar
  , smpWrapN
  , smpWrapS
) where

import System.Random(StdGen)

import Topic(wrap, Situations)
import Auction(forbid, pointRange, minSuitLength, maxSuitLength, Action,
               balancedHand, constrain, makeCall, makePass, alternatives,
               SuitLengthComparator(..), compareSuitLength)
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


b1C :: Action
b1C = do
    pointRange 16 40
    forbid b1N
    forbid b2N
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


b1C1D2H :: Action
b1C1D2H = do
    pointRange 22 40
    forbid b1C1D2N
    minSuitLength T.Hearts 5
    -- This should be your longest suit
    -- TODO: if you're 5-5, which suit do you bid first?
    sequence_ . map (forbid . compareSuitLength T.Hearts Shorter) $
        [T.Clubs, T.Diamonds, T.Spades]
    makeCall $ T.Bid 2 T.Hearts


b1C1D2S :: Action
b1C1D2S = do
    pointRange 22 40
    forbid b1C1D2N
    forbid b1C1D2H
    minSuitLength T.Spades 5
    -- This should be your longest suit
    -- TODO: if you're 5-5, which suit do you bid first?
    sequence_ . map (forbid . compareSuitLength T.Spades Shorter) $
        [T.Clubs, T.Diamonds, T.Hearts]
    makeCall $ T.Bid 2 T.Hearts


b1C1D3C :: Action
b1C1D3C = do
    pointRange 22 40
    forbid b1C1D2N
    forbid b1C1D2H
    forbid b1C1D2S
    minSuitLength T.Clubs 5
    -- This should be your longest suit
    -- TODO: if you're 5-5, which suit do you bid first?
    sequence_ . map (forbid . compareSuitLength T.Clubs Shorter) $
        [T.Diamonds, T.Hearts, T.Spades]
    makeCall $ T.Bid 3 T.Clubs


b1C1D3D :: Action
b1C1D3D = do
    pointRange 22 40
    forbid b1C1D2N
    forbid b1C1D2H
    forbid b1C1D2S
    forbid b1C1D3C
    minSuitLength T.Diamonds 5
    -- This should be your longest suit
    -- TODO: if you're 5-5, which suit do you bid first?
    sequence_ . map (forbid . compareSuitLength T.Diamonds Shorter) $
        [T.Clubs, T.Hearts, T.Spades]
    makeCall $ T.Bid 3 T.Diamonds


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
    -- clubs are at least as long as diamonds
    forbid $ compareSuitLength T.Clubs Shorter T.Diamonds
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
    -- Diamonds must be longer than clubs: with equal lengths, bid clubs.
    compareSuitLength T.Diamonds Longer T.Clubs
    alternatives [ minSuitLength T.Diamonds 6
                 , minSuitLength T.Diamonds 5 >> minSuitLength T.Clubs 4
                 ]
    makeCall $ T.Bid 2 T.Diamonds


-------------------------------
-- Situation syntactic sugar --
-------------------------------
-- Always make opener be in first seat, until we figure out how to open in other
-- seats.
-- TODO: change this to let other folks be dealer, too
smpWrapS :: (StdGen -> T.Vulnerability -> T.Direction -> Situation) -> Situations
smpWrapS sit = wrap $ sit <~ T.allVulnerabilities <~ [T.South]

smpWrapN :: (StdGen -> T.Vulnerability -> T.Direction -> Situation) -> Situations
smpWrapN sit = wrap $ sit <~ T.allVulnerabilities <~ [T.North]
