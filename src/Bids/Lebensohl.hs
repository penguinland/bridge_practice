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
  , b1N2N3C
  , b1No2C3D
  , b1No2C3H
  , b1No2C3S
  , b1No2D2H
  , b1No2D2S
  , b1No2D2N
  , b1No2D2N3CP
  , b1No2D3C
  , b1No2D3H
  , b1No2D3S
  , b1No2H2S
  , b1No2H2N
  , b1No2H2N3CP
  , b1No2H2N3C3D
  , b1No2H3C
  , b1No2H3D
  , b1No2H3S
  , b1No2S2N
  , b1No2S2N3CP
  , b1No2S2N3C3D
  , b1No2S2N3C3H
  , b1No2S3C
  , b1No2S3D
  , b1No2S3H
  , b1NoBM2N  -- For when the opponents show both majors
) where


import Action(Action, withholdBid)
import qualified Bids.OneNotrump as NT
import EDSL(minSuitLength, makeCall, makeAlertableCall, pointRange, forEach,
            forbid, balancedHand, hasStopper, alternatives, soundHolding,
            longerThan, atLeastAsLong)
import Output((.+))
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
signoff_ :: Int -> T.Suit -> Action
signoff_ level suit = do
    NT.lessThanInvitational
    pointRange 5 40  -- We should have at least half the points to compete
    -- NOTE: if we're nonvul, we might compete with less than half the points if
    -- we're extra shapely. That's hard to encode here because we don't know the
    -- vulnerability yet, and annoying to code in a topic because we can't reuse
    -- this function for that situation. Just skip it; users won't get to
    -- practice but it's a rare situation that is hopefully obvious when it
    -- occurs.
    minSuitLength suit 5
    -- Have a good reason to bid this suit.
    alternatives [soundHolding suit, minSuitLength suit 6]
    -- If you're 5-5 in the majors, pick the better suit. I'm too lazy to add
    -- something to the EDSL to figure out which suit is better, so avoid it.
    forEach (filter (/= suit) T.allSuits) (suit `longerThan`)
    makeCall $ T.Bid level suit


b1No2D2H :: Action
b1No2D2H = signoff_ 2 T.Hearts


b1No2D2S :: Action
b1No2D2S = signoff_ 2 T.Spades


b1No2H2S :: Action
b1No2H2S = signoff_ 2 T.Spades


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
primarySuit_ T.Notrump = error "notrump is not a primary suit"


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


-- Signoffs that you couldn't bid at the 2 level
b1No2D2N3CP :: Action
b1No2D2N3CP = do
    withholdBid $ signoff_ 8 T.Clubs
    makeCall T.Pass

b1No2H2N3CP :: Action
b1No2H2N3CP = do
    withholdBid $ signoff_ 8 T.Clubs
    makeCall T.Pass

b1No2S2N3CP :: Action
b1No2S2N3CP = do
    withholdBid $ signoff_ 8 T.Clubs
    makeCall T.Pass

b1No2H2N3C3D :: Action
b1No2H2N3C3D = signoff_ 3 T.Diamonds

b1No2S2N3C3D :: Action
b1No2S2N3C3D = signoff_ 3 T.Diamonds

b1No2S2N3C3H :: Action
b1No2S2N3C3H = signoff_ 3 T.Hearts


-- Time for the actual lebensohl relays!
b1N2N3C :: Action
b1N2N3C = makeAlertableCall (T.Bid 3 T.Clubs) "relay completed"


b1No2D2N :: Action
b1No2D2N = do
    alternatives [ b1No2D2N3CP
                 ]
    makeAlertableCall (T.Bid 2 T.Notrump) ("relay to " .+ T.Bid 3 T.Clubs)


b1No2H2N :: Action
b1No2H2N = do
    alternatives [ b1No2H2N3CP
                 , b1No2H2N3C3D
                 ]
    makeAlertableCall (T.Bid 2 T.Notrump) ("relay to " .+ T.Bid 3 T.Clubs)


b1No2S2N :: Action
b1No2S2N = do
    alternatives [ b1No2S2N3CP
                 , b1No2S2N3C3D
                 , b1No2S2N3C3H
                 ]
    makeAlertableCall (T.Bid 2 T.Notrump) ("relay to " .+ T.Bid 3 T.Clubs)


-- Have a special one for when the opponents show both majors
b1NoBM2N :: Action
b1NoBM2N = do
    alternatives [ b1No2S2N3CP
                 , b1No2S2N3C3D
                 ]
    makeAlertableCall (T.Bid 2 T.Notrump) ("relay to " .+ T.Bid 3 T.Clubs)
