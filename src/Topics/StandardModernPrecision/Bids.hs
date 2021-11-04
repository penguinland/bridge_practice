module Topics.StandardModernPrecision.Bids(
    firstSeatOpener
  , b1C
  , b1D
  , b1M
  , b1N
  , b2C
  , b2D
  , b2N
  , smpWrapN
  , smpWrapS
) where

import Topic(wrap, Situations)
import Auction(forbid, pointRange, minSuitLength, Action, balancedHand,
               constrain, makeCall)
import Situation(Situation, base, (<~))
import qualified Terminology as T


-- Because we're not using the Rule of 20 and its ilk, we're going to skip the
-- auctions that start with other folks passing for now, and maybe come back to
-- those later.
-- TODO: figure out how to practice the auctions that start P-1C-1S (with 1S
-- being game forcing and natural 5-card spade suit, rather than slam forcing)
firstSeatOpener :: Action
firstSeatOpener = do
    pointRange 11 40  -- Open any good 10 count, too. but that's hard to codify


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


-- syntactic sugar: Always make opener be in first seat, until we figure out how
-- to open in other seats.
-- TODO: change this to let other folks be dealer, too
smpWrapS :: (T.Vulnerability -> T.Direction -> Situation) -> Situations
smpWrapS sit = wrap $ base sit <~ T.allVulnerabilities <~ [T.South]

smpWrapN :: (T.Vulnerability -> T.Direction -> Situation) -> Situations
smpWrapN sit = wrap $ base sit <~ T.allVulnerabilities <~ [T.North]
