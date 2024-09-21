module Bids.Lebensohl(
    b1N  -- re-exported from StandardOpenings
    -- Systems on over X
  , b1NoX2C
  , b1NoX2D
  , b1NoX2H
    -- Systems on over 2C
  , b1No2CX
  , b1No2C2D
  , b1No2C2H
   -- Actual lebensohl bids
  , b1No2C3D
  , b1No2C3H
  , b1No2C3S
  , b1No2D2H
  , b1No2D2S
  , b1No2D3C
  , b1No2D3H
  , b1No2D3S
  , b1No2H2S
  , b1No2H3C
  , b1No2H3D
  , b1No2H3S
  , b1No2S3C
  , b1No2S3D
  , b1No2S3H
) where


import Action(Action, withholdBid)
import qualified Bids.OneNotrump as NT
import EDSL(minSuitLength, makeCall, longerThan, atLeastAsLong, pointRange,
            forbid, balancedHand, hasStopper, alternatives, soundHolding)
import qualified Terminology as T


b1N :: Action
b1N = NT.b1N


-- Ignore the opponents' interference if it doesn't actually take up any bidding
-- space.
b1NoX2C :: Action
b1NoX2C = NT.b1N2C

b1NoX2D :: Action
b1NoX2D = NT.b1N2D

b1NoX2H :: Action
b1NoX2H = NT.b1N2H

b1No2CX :: Action
b1No2CX = do
    withholdBid NT.b1N2C
    makeCall T.Double

b1No2C2D :: Action
b1No2C2D = NT.b1N2D

b1No2C2H :: Action
b1No2C2H = NT.b1N2H

-- Signoffs
b1No2D2H :: Action
b1No2D2H = do
    NT.lessThanInvitational
    pointRange 5 40
    minSuitLength T.Hearts 5
    -- If you're 5-5 in the majors, pick the better suit. I'm too lazy to add
    -- something to the EDSL to figure out which suit is better, so avoid it.
    T.Hearts `longerThan` T.Clubs
    T.Hearts `longerThan` T.Diamonds
    T.Hearts `longerThan` T.Spades
    makeCall $ T.Bid 2 T.Hearts


b1No2D2S :: Action
b1No2D2S = do
    NT.lessThanInvitational
    pointRange 5 40
    minSuitLength T.Spades 5
    T.Spades `longerThan` T.Clubs
    T.Spades `longerThan` T.Diamonds
    T.Spades `longerThan` T.Hearts
    makeCall $ T.Bid 2 T.Spades


b1No2H2S :: Action
b1No2H2S = b1No2D2S


primarySuit_ :: T.Suit -> Action
primarySuit_ T.Clubs = do
    T.Clubs `longerThan` T.Diamonds
    T.Clubs `longerThan` T.Hearts
    T.Clubs `longerThan` T.Spades
primarySuit_ T.Diamonds = do
    T.Diamonds `atLeastAsLong` T.Clubs
    T.Diamonds `longerThan` T.Hearts
    T.Diamonds `longerThan` T.Spades
primarySuit_ T.Hearts = do
    T.Hearts `atLeastAsLong` T.Clubs
    T.Hearts `atLeastAsLong` T.Diamonds
    T.Hearts `longerThan` T.Spades
primarySuit_ T.Spades = do
    T.Spades `atLeastAsLong` T.Clubs
    T.Spades `atLeastAsLong` T.Diamonds
    T.Spades `atLeastAsLong` T.Hearts


gfWithSuit_ :: T.Suit -> T.Suit -> Action
gfWithSuit_ ourSuit oppsSuit = do
    NT.gameForcing
    minSuitLength ourSuit 5
    primarySuit_ ourSuit
    -- It's no fun when you bid a suit headed by the queen-nine. Probably do
    -- that at the table, but practice the more obvious holdings instead.
    soundHolding ourSuit
    -- Prefer notrump when possible
    alternatives [forbid balancedHand, forbid (hasStopper oppsSuit)]
    makeCall $ T.Bid 3 ourSuit


-- With 5-5 in two suits, bid the higher one, so you can rebid the lower one
-- later.
b1No2D3C :: Action
b1No2D3C = gfWithSuit_ T.Clubs T.Diamonds

b1No2H3C :: Action
b1No2H3C = gfWithSuit_ T.Clubs T.Hearts

b1No2S3C :: Action
b1No2S3C = gfWithSuit_ T.Clubs T.Spades

b1No2C3D :: Action
b1No2C3D = gfWithSuit_ T.Diamonds T.Clubs

b1No2H3D :: Action
b1No2H3D = gfWithSuit_ T.Diamonds T.Hearts

b1No2S3D :: Action
b1No2S3D = gfWithSuit_ T.Diamonds T.Spades

b1No2C3H :: Action
b1No2C3H = gfWithSuit_ T.Hearts T.Clubs

b1No2D3H :: Action
b1No2D3H = gfWithSuit_ T.Hearts T.Diamonds

b1No2S3H :: Action
b1No2S3H = gfWithSuit_ T.Hearts T.Spades

b1No2C3S :: Action
b1No2C3S = gfWithSuit_ T.Spades T.Clubs

b1No2D3S :: Action
b1No2D3S = gfWithSuit_ T.Spades T.Diamonds

b1No2H3S :: Action
b1No2H3S = gfWithSuit_ T.Spades T.Hearts
