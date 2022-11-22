module Topics.StandardModernPrecision.BasicBids(
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
  -- syntactic sugar
  , smpWrapN
  , smpWrapS
) where

import System.Random(StdGen)

import Topic(wrap, Situations)
import Auction(forbid, pointRange, minSuitLength,
               Action, balancedHand, constrain, makeCall, makeAlertableCall,
               makePass)
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
    makeAlertableCall (T.Bid 1 T.Notrump) "14-16"


b2N :: Action
b2N = do
    pointRange 19 20  -- A modification from Part 1
    balancedHand
    makeCall $ T.Bid 2 T.Notrump


b3N :: Action
b3N = do
    pointRange 24 40  -- Technically only 24-27, but close enough
    balancedHand
    makeCall $ T.Bid 3 T.Notrump


b1C :: Action
b1C = do
    pointRange 16 40
    sequence_ . map forbid $ [b1N, b2N, b3N]
    makeAlertableCall (T.Bid 1 T.Clubs) "16+ HCP"


b1M :: T.Suit -> Action
b1M suit = do
    firstSeatOpener
    forbid b1C
    forbid b1N
    minSuitLength suit 5
    makeCall $ T.Bid 1 suit


b2C :: Action
b2C = do
    firstSeatOpener
    forbid b1C
    forbid (b1M T.Hearts)
    forbid (b1M T.Spades)
    minSuitLength T.Clubs 6
    makeAlertableCall (T.Bid 2 T.Clubs) "6+ clubs"


b2D :: Action
b2D = do
    firstSeatOpener
    forbid b1C
    constrain "two_diamond_opener" ["shape(", ", 4414 + 4405 + 4315 + 3415)"]
    makeAlertableCall (T.Bid 2 T.Diamonds) "4414, 4315, 3415, or 4405 shape"


b1D :: Action
b1D = do
    firstSeatOpener
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
    makeAlertableCall (T.Bid 1 T.Diamonds) "Could be as short as 2 diamonds"


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
